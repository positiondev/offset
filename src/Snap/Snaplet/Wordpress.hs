{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Snap.Snaplet.Wordpress (
   Wordpress(..)
 , WordpressConfig(..)
 , CachePeriod(..)
 , initWordpress
 , initWordpress'
 , CacheKey(..)
 , cacheLookup
 , cacheSet
 , transformName
 ) where

import           Prelude                  hiding ((++))

import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Attoparsec.Text     as A
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Char                (toUpper)
import           Data.Default
import qualified Data.HashMap.Strict      as M
import           Data.Map.Syntax
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Set                 (Set)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Database.Redis           (Redis)
import qualified Database.Redis           as R
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import qualified Network.Wreq             as W
import           Snap
import           Snap.Snaplet.Heist       (Heist, addConfig)
import           Snap.Snaplet.RedisDB     (RedisDB)
import qualified Snap.Snaplet.RedisDB     as R

(++) :: Monoid a => a -> a -> a
(++) = mappend

data CachePeriod = NoCache | CacheSeconds Int

data WordpressConfig = WordpressConfig { endpoint    :: Text
                                       , requester   :: Maybe (Text -> [(Text, Text)] -> IO Text)
                                       , cachePeriod :: CachePeriod
                                       }

instance Default WordpressConfig where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600)

data Wordpress b = Wordpress { runRedis :: forall a. Redis a -> Handler b (Wordpress b) a
                             , runHTTP  :: Text -> [(Text, Text)] -> IO Text
                             }

initWordpress = initWordpress' def

initWordpress' :: WordpressConfig
               -> Snaplet (Heist b)
               -> Simple Lens b (Snaplet RedisDB)
               -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
               -> SnapletInit b (Wordpress b)
initWordpress' wpconf heist redis wordpress =
  makeSnaplet "wordpress" "" Nothing $
    do conf <- getSnapletUserConfig
       addConfig heist $ set scCompiledSplices (wordpressSplices wpconf wordpress) mempty
       let req = fromMaybe wreqRequester (requester wpconf)
       return $ Wordpress (withTop' id . R.runRedisDB redis) req

wordpressSplices :: WordpressConfig
                 -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                 -> Splices (Splice (Handler b b))
wordpressSplices conf wordpress = do "wpPosts" ## wpPostsSplice conf wordpress
                                     "wpPostByPermalink" ## wpPostByPermalinkSplice conf wordpress


wpPostsSplice :: WordpressConfig
              -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
              -> Splice (Handler b b)
wpPostsSplice conf wordpress =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren postSplices (getPromise promise)
     return $ yieldRuntime $
       do (Wordpress _ req) <- lift (use (wordpress . snapletValue))
          res <- liftIO $ req (endpoint conf ++ "/posts") []
          case decodeStrict . T.encodeUtf8 $ res of
            Just posts -> do putPromise promise posts
                             codeGen outputChildren
            Nothing -> codeGen (yieldPureText "")


wpPostByPermalinkSplice :: WordpressConfig
                        -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                        -> Splice (Handler b b)
wpPostByPermalinkSplice conf wordpress =
  do promise <- newEmptyPromise
     outputChildren <- withSplices runChildren postSplices (getPromise promise)
     return $ yieldRuntime $
       do mperma <- (parsePermalink . T.decodeUtf8 . rqURI) <$> lift getRequest
          case mperma of
            Nothing -> codeGen (yieldPureText "")
            Just (year, month, slug) ->
              do mres <- lift $ with wordpress $ cacheLookup (PostByPermalinkKey year month slug)
                 res <- case mres of
                          Just r' -> return (Just [r'])
                          Nothing ->
                            do (Wordpress _ req) <- lift $ use (wordpress . snapletValue)
                               (decodeStrict . T.encodeUtf8) <$>
                                 (liftIO $ req  (endpoint conf ++ "/posts")
                                                [("filter[year]",year)
                                                ,("filter[monthnum]", month)
                                                ,("filter[name]", slug)])
                 case res of
                   Just (post:_) -> do putPromise promise post
                                       codeGen outputChildren
                   _ -> codeGen (yieldPureText "")

parsePermalink = either (const Nothing) Just . A.parseOnly parser
  where parser = do A.char '/'
                    year <- A.count 4 A.digit
                    A.char '/'
                    month <- A.count 2 A.digit
                    A.char '/'
                    slug <- A.many1 (A.letter <|> A.char '-')
                    return (T.pack year, T.pack month, T.pack slug)

postSplices :: Monad m => Splices (RuntimeSplice m Object -> Splice m)
postSplices = mapV (pureSplice . textSplice) $ mconcat (map buildSplice ["ID", "title", "type", "content", "excerpt", "date"])
  where buildSplice n = transformName n ## \o -> case M.lookup n o of
                                                   Just (String t) -> t
                                                   Just (Number i) -> T.pack $ show i
                                                   _ -> ""


transformName :: Text -> Text
transformName = T.append "wp" . snd . T.foldl f (True, "")
  where f (True, rest) next = (False, T.snoc rest (toUpper next))
        f (False, rest) '_' = (True, rest)
        f (False, rest) next = (False, T.snoc rest next)


type Year = Text
type Month = Text
type Slug = Text
type Filter = Text
class FormatKey a where
  formatKey :: a -> ByteString

data CacheKey = PostByPermalinkKey Year Month Slug
              | PostsKey (Set Filter)

instance FormatKey CacheKey where
  formatKey (PostByPermalinkKey y m s) =
    T.encodeUtf8 $ "wordpress:post_perma:" ++ y ++ "_" ++ m ++ "_" ++ s

cacheLookup :: CacheKey -> Handler b (Wordpress b) (Maybe Object)
cacheLookup key = do (Wordpress run _) <- view snapletValue <$> getSnapletState
                     res <- run $ R.get (formatKey key)
                     case res of
                       Left err -> return Nothing
                       Right val -> return $ val >>= decodeStrict

cacheSet :: CacheKey -> Object -> Handler b (Wordpress b) Bool
cacheSet key o = do (Wordpress run _) <- view snapletValue <$> getSnapletState
                    res <- run $ R.set (formatKey key) (toStrict $ encode o)
                    case res of
                      Left err -> return False
                      Right val -> return True

wreqRequester :: Text -> [(Text, Text)] -> IO Text
wreqRequester u ps = do r <- W.getWith (set W.params ps W.defaults) (T.unpack u)
                        return $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
