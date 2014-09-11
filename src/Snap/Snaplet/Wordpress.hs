{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Snap.Snaplet.Wordpress (
   Wordpress(..)
 , WordpressConfig(..)
 , CachePeriod(..)
 , initWordpress
 , initWordpress') where

import           Prelude              hiding ((++))

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Default
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Database.Redis       (Redis)
import           Heist
import           Heist.Compiled
import           Snap
import           Snap.Snaplet.Heist   (Heist, addConfig)
import           Snap.Snaplet.RedisDB (RedisDB)
import qualified Snap.Snaplet.RedisDB as R

(++) :: Monoid a => a -> a -> a
(++) = mappend

data CachePeriod = NoCache | CacheSeconds Int

data WordpressConfig = WordpressConfig { endpoint    :: Text
                                       , requester   :: Maybe (Text -> IO (Maybe Text))
                                       , cachePeriod :: CachePeriod
                                       }

instance Default WordpressConfig where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600)

data Wordpress b = Wordpress { runRedis :: forall a. Redis a -> Handler b b a}

data Post = Post {postId :: Int, postTitle :: Text}


instance FromJSON Post where
   parseJSON (Object v) = Post <$> v .: "ID"
                               <*> v .: "title"
   parseJSON _          = mzero


initWordpress :: Snaplet (Heist b)
              -> Simple Lens b (Snaplet RedisDB)
              -> SnapletInit b (Wordpress b)
initWordpress = initWordpress' def

initWordpress' :: WordpressConfig
               -> Snaplet (Heist b)
               -> Simple Lens b (Snaplet RedisDB)
               -> SnapletInit b (Wordpress b)
initWordpress' wpconf heist r =
  makeSnaplet "wordpress" "" Nothing $
    do conf <- getSnapletUserConfig
       addConfig heist mempty { hcCompiledSplices = wordpressSplices wpconf }
       return $ Wordpress (R.runRedisDB r)

wordpressSplices :: WordpressConfig -> Splices (Splice (Handler b b))
wordpressSplices conf = "wpPosts" ## wpPostsSplice conf


wpPostsSplice :: WordpressConfig -> Splice (Handler b b)
wpPostsSplice conf =
  case requester conf of
    Just r -> do res <- liftIO $ r (endpoint conf ++ "/posts")
                 case (T.encodeUtf8 <$> res) >>= decodeStrict of
                   Just posts -> manyWithSplices runChildren postSplices (return posts)
                   Nothing -> return (yieldPureText "")

postSplices :: Monad m => Splices (RuntimeSplice m Post -> Splice m)
postSplices = mapS (pureSplice . textSplice)$ do "wpId" ## T.pack . show . postId
                                                 "wpTitle" ## postTitle
