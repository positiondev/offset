{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Snap.Snaplet.Wordpress.Init where

import           Control.Concurrent.MVar
import           Control.Lens                       hiding (children)
import           Control.Monad.Trans                (liftIO)
import qualified Data.Configurator                  as C
import           Data.Default
import qualified Data.Map                           as Map
import           Data.Monoid
import qualified Database.Redis                     as R
import           Heist
import           Snap                               hiding (path, rqURI)
import           Snap.Snaplet.Heist                 (Heist, addConfig)
import           Snap.Snaplet.RedisDB               (RedisDB)
import qualified Snap.Snaplet.RedisDB               as RDB

import           Snap.Snaplet.Wordpress.Cache
import           Snap.Snaplet.Wordpress.Cache.Types
import           Snap.Snaplet.Wordpress.HTTP
import           Snap.Snaplet.Wordpress.Internal
import           Snap.Snaplet.Wordpress.Splices
import           Snap.Snaplet.Wordpress.Types

instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600) [] Nothing

initWordpress :: Snaplet (Heist b)
              -> Snaplet RedisDB
              -> WPLens b
              -> SnapletInit b (Wordpress b)
initWordpress = initWordpress' def

initWordpress' :: WordpressConfig (Handler b b)
               -> Snaplet (Heist b)
               -> Snaplet RedisDB
               -> WPLens b
               -> SnapletInit b (Wordpress b)
initWordpress' wpconf heist redis wpLens =
  makeSnaplet "wordpress" "" Nothing $
    do conf <- getSnapletUserConfig
       let logf = wpLogInt $ wpConfLogger wpconf
       wpReq <- case wpConfRequester wpconf of
                Nothing -> do u <- liftIO $ C.require conf "username"
                              p <- liftIO $ C.require conf "password"
                              return $ wreqRequester logf u p
                Just r -> return r
       active <- liftIO $ newMVar Map.empty
       let rrunRedis = R.runRedis $ view (snapletValue . RDB.redisConnection) redis
       let wpInt = WordpressInt{ wpRequest = wpRequestInt wpReq (wpConfEndpoint wpconf)
                               , wpCacheSet = wpCacheSetInt rrunRedis (wpConfCacheBehavior wpconf)
                               , wpCacheGet = wpCacheGetInt rrunRedis (wpConfCacheBehavior wpconf)
                               , startReqMutex = startReqMutexInt active
                               , stopReqMutex = stopReqMutexInt active }
       let wp = Wordpress{ requestPostSet = Nothing
                         , wpExpireAggregates = wpExpireAggregatesInt rrunRedis
                         , wpExpirePost = wpExpirePostInt rrunRedis
                         , cachingGet = cachingGetInt wpInt
                         , cachingGetRetry = cachingGetRetryInt wpInt
                         , cachingGetError = cachingGetErrorInt wpInt
                         , cacheInternals = wpInt
                         , wpLogger = logf
                         }
       let extraFields = wpConfExtraFields wpconf
       addConfig heist $ set scCompiledSplices (wordpressSplices wp extraFields wpLens) mempty
       return wp
