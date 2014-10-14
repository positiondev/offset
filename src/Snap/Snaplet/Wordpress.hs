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
 , TagSpec(..)
 , TagSpecList(..)
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
import qualified Data.Configurator        as C
import           Data.Default
import qualified Data.HashMap.Strict      as M
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import           Data.List                (intercalate, partition)
import           Data.Map.Syntax
import           Data.Maybe               (catMaybes, fromMaybe, isJust,
                                           listToMaybe)
import           Data.Monoid
import           Data.Ratio
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
import qualified Text.XmlHtml             as X

import           Debug.Trace

(++) :: Monoid a => a -> a -> a
(++) = mappend

readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

tshow = T.pack . show

data CachePeriod = NoCache | CacheSeconds Int

data WordpressConfig = WordpressConfig { endpoint    :: Text
                                       , requester   :: Maybe (Text -> [(Text, Text)] -> IO Text)
                                       , cachePeriod :: CachePeriod
                                       }

instance Default WordpressConfig where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600)

data Wordpress b = Wordpress { runRedis       :: forall a. Redis a -> Handler b (Wordpress b) a
                             , runHTTP        :: Text -> [(Text, Text)] -> IO Text
                             , requestPostSet :: Maybe IntSet
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
       req <- case requester wpconf of
                Nothing -> do u <- liftIO $ C.require conf "username"
                              p <- liftIO $ C.require conf "password"
                              return $ wreqRequester u p
                Just r -> return r
       return $ Wordpress (withTop' id . R.runRedisDB redis) req Nothing

wordpressSplices :: WordpressConfig
                 -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                 -> Splices (Splice (Handler b b))
wordpressSplices conf wordpress =
  do "wpPosts" ## wpPostsSplice conf wordpress
     "wpPostByPermalink" ## wpPostByPermalinkSplice conf wordpress
     "wpNoPostDuplicates" ## wpNoPostDuplicatesSplice wordpress


wpNoPostDuplicatesSplice :: Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
                         -> Splice (Handler b b)
wpNoPostDuplicatesSplice wordpress =
  return $ yieldRuntime $
    do (Wordpress red ht ps) <- lift $ use (wordpress . snapletValue)
       case ps of
         Nothing -> lift $ assign (wordpress . snapletValue) (Wordpress red ht (Just IntSet.empty))
         Just _ -> return ()
       codeGen $ yieldPureText ""

data TagSpec = TagPlus Text | TagMinus Text deriving (Eq, Ord)

data TagSpecId = TagPlusId Int | TagMinusId Int deriving (Eq, Show, Ord)

instance Show TagSpec where
  show (TagPlus t) = "+" ++ (T.unpack t)
  show (TagMinus t) = "-" ++ (T.unpack t)

tagChars = ['a'..'z'] ++ "-"

instance Read TagSpec where
  readsPrec _ ('+':cs) | all (`elem` tagChars) cs = [(TagPlus (T.pack cs), "")]
  readsPrec _ ('-':cs) | all (`elem` tagChars) cs = [(TagMinus (T.pack cs), "")]
  readsPrec _ cs | all (`elem` tagChars) cs = [(TagPlus (T.pack cs), "")]
  readsPrec _ _ = []

newtype TagSpecList = TagSpecList { unTagSpecList :: [TagSpec]} deriving (Eq, Ord)

instance Show TagSpecList where
  show (TagSpecList ts) = intercalate "," (map show ts)

instance Read TagSpecList where
  readsPrec _ ts = let vs = map (readSafe) $ T.splitOn "," $ T.pack ts in
                     if all isJust vs
                        then [(TagSpecList $ catMaybes vs, "")]
                        else []

wpPostsSplice :: WordpressConfig
              -> Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))
              -> Splice (Handler b b)
wpPostsSplice conf wordpress =
  do promise <- newEmptyPromise
     outputChildren <- manyWithSplices runChildren postSplices (getPromise promise)
     n <- getParamNode
     let limit = (fromMaybe 20 $ readSafe =<< X.getAttribute "limit" n) :: Int
         num = (fromMaybe 20 $ readSafe =<< X.getAttribute "num" n) :: Int
         offset' = (fromMaybe 0 $ readSafe =<< X.getAttribute "offset" n) :: Int
         page' = (fromMaybe 1 $ readSafe =<< X.getAttribute "page" n) :: Int
         page = if page' < 1 then 1 else page'
         offset = num * (page - 1) + offset'
         tags' = unTagSpecList (fromMaybe (TagSpecList []) $ readSafe =<< X.getAttribute "tags" n)
     (tagsPlus, tagsMinus) <- partition (\t -> case t of
                                                 (TagPlusId _) -> True
                                                 (TagMinusId _) -> False) <$>
                                (lift $ lookupTagIds conf tags')
     return $ yieldRuntime $
       do (Wordpress _ req postSet) <- lift (use (wordpress . snapletValue))
          res <- liftIO $ req (endpoint conf ++ "/posts") $ [("filter[posts_per_page]", tshow num)
                                                            ,("filter[offset]", tshow offset)
                                                            ] ++ (map (\(TagPlusId i) -> ("filter[tag__in]", tshow i)) tagsPlus)
                                                              ++ (map (\(TagMinusId i) -> ("filter[tag__not_in]", tshow i)) tagsMinus)
          case decodeStrict . T.encodeUtf8 $ res of
            Just posts -> do let postsW = extractPostIds posts
                             let postsND = noDuplicates postSet . take limit $ postsW
                             lift $ addPostIds wordpress (map fst postsND)
                             putPromise promise (map snd postsND)
                             codeGen outputChildren
            Nothing -> codeGen (yieldPureText "")
  where noDuplicates :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        noDuplicates Nothing = id
        noDuplicates (Just postSet) = filter (\(i,_) -> IntSet.notMember i postSet)

newtype TagRes = TagRes (Int, Text)

instance FromJSON TagRes where
  parseJSON (Object o) = TagRes <$> ((,) <$> o .: "ID" <*> o .: "slug")
  parseJSON _ = mzero

lookupTagIds :: WordpressConfig -> [TagSpec] -> IO [TagSpecId]
lookupTagIds _ [] = return []
lookupTagIds conf specs =
  do res <- W.get $ (T.unpack $ endpoint conf) ++ "/taxonomies/post_tag/terms"
     let (Just tags) = decode $ res ^. W.responseBody :: Maybe [TagRes]
     return $ map (getSpecId tags) specs
 where getSpecId tags (TagPlus slug) = matchWith tags slug TagPlusId
       getSpecId tags (TagMinus slug) = matchWith tags slug TagMinusId
       matchWith tags slug constr = case filter (\(TagRes (_,s)) -> s == slug) tags of
                                       [] -> error $ "Couldn't find tag: " ++ T.unpack slug
                                       (TagRes  (i,_):_) -> constr i

extractPostIds :: [Object] -> [(Int, Object)]
extractPostIds = map (\p -> let i = M.lookup "ID" p
                                is = case i of
                                      Just (String n) -> readSafe n
                                      Just (Number n) ->
                                        let rat = toRational n in
                                        case denominator rat of
                                          1 -> Just $ fromInteger (numerator rat)
                                          _ -> Nothing
                                      _ -> Nothing
                                id' = fromMaybe 0 is in
                              (id', p))

addPostIds :: Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b)) -> [Int] -> Handler b b ()
addPostIds wordpress ids =
  do (Wordpress red req postSet) <- use (wordpress . snapletValue)
     assign (wordpress . snapletValue)
            (Wordpress red req ((`IntSet.union` (IntSet.fromList ids)) <$> postSet))


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
                            do (Wordpress _ req posts) <- lift $ use (wordpress . snapletValue)
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
cacheLookup key = do (Wordpress run _ _) <- view snapletValue <$> getSnapletState
                     res <- run $ R.get (formatKey key)
                     case res of
                       Left err -> return Nothing
                       Right val -> return $ val >>= decodeStrict

cacheSet :: CacheKey -> Object -> Handler b (Wordpress b) Bool
cacheSet key o = do (Wordpress run _ _) <- view snapletValue <$> getSnapletState
                    res <- run $ R.set (formatKey key) (toStrict $ encode o)
                    case res of
                      Left err -> return False
                      Right val -> return True

wreqRequester :: Text -> Text -> Text -> [(Text, Text)] -> IO Text
wreqRequester user pass u ps =
  do let opts = (W.defaults & W.params .~ ps
                            & W.auth .~ W.basicAuth user' pass'
                            )
     -- print (u, ps, view W.headers opts, view W.auth opts)
     r <- W.getWith opts (T.unpack u)
     return $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 pass
