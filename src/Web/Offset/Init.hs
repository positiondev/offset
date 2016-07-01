{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Init where

import           Control.Concurrent.MVar
import           Control.Lens            hiding (children)
import           Control.Monad.State
import           Control.Monad.Trans     (liftIO)
import qualified Data.Configurator       as C
import           Data.Default
import qualified Data.Map                as Map
import           Data.Monoid
import           Data.Text               (Text)
import qualified Database.Redis          as R
import           Larceny

import           Web.Offset.Cache
import           Web.Offset.Cache.Types
import           Web.Offset.HTTP
import           Web.Offset.Internal
import           Web.Offset.Splices
import           Web.Offset.Types


instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1:8080/wp-json" (Left ("offset", "111")) (CacheSeconds 600) [] Nothing

initWordpress :: WordpressConfig s
              -> R.Connection
              -> StateT s IO Text
              -> WPLens b s
              -> IO (Wordpress b, Substitutions s)
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
                          , stopReqMutex = stopReqMutexInt active
                          , runRedis = rrunRedis
                          }
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
  return (wp, wordpressSubs wp extraFields getURI wpLens)
