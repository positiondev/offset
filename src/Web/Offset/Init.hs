{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Init where

import           Control.Concurrent.MVar
import           Control.Lens            hiding (children)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Configurator       as C
import           Data.Default
import qualified Data.Map                as Map
import           Data.Monoid
import qualified Database.Redis          as R
import           Heist
import Control.Monad.State
import Data.Text (Text)
import Heist.Compiled

import           Web.Offset.Cache
import           Web.Offset.Cache.Types
import           Web.Offset.HTTP
import           Web.Offset.Internal
import           Web.Offset.Splices
import           Web.Offset.Types

instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1:8080/wp-json" (Left ("offset", "111")) (CacheSeconds 600) [] Nothing

initWordpress :: (MonadIO m, MonadState s m) =>
                 WordpressConfig m
              -> R.Connection
              -> m Text
              -> WPLens b s m
              -> IO (Wordpress b, Splices (Splice m))
initWordpress wpconf redis getURI wpLens = do
  let rrunRedis = R.runRedis redis
  let logf = wpLogInt $ wpConfLogger wpconf
  let wpReq = case wpConfRequester wpconf of
                Left (u,p) -> wreqRequester logf u p
                Right r -> r
  active <- newMVar Map.empty
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
  return (wp, wordpressSplices wp extraFields getURI wpLens)
{-
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
-}
