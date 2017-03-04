{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Init where

import           Control.Concurrent.MVar
import           Control.Monad.State
import qualified Data.Map                     as Map
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Database.Redis               as R
import           Web.Larceny

import           Web.Offset.Cache
import           Web.Offset.HTTP
import           Web.Offset.Internal
import           Web.Offset.Splices
import           Web.Offset.Types
import           Web.Offset.WordPress.Splices

initCMS :: CMSConfig s
        -> R.Connection
        -> StateT s IO Text
        -> CMSLens b s
        -> IO (CMS b, Substitutions s)
initCMS cmsconf redis getURI cmsLens = do
  let rrunRedis = R.runRedis redis
  let logf = cmsLogInt $ cmsConfLogger cmsconf
  let wpReq = case cmsConfRequest cmsconf of
                Left (u,p) -> wreqRequester logf u p
                Right r -> r
  active <- newMVar Map.empty
  let cmsInt = CMSInt{ cmsRequest = cmsRequestInt wpReq (cmsConfEndpoint cmsconf)
                     , cmsCacheSet = cmsCacheSetInt rrunRedis (cmsConfCacheBehavior cmsconf)
                     , cmsCacheGet = cmsCacheGetInt rrunRedis (cmsConfCacheBehavior cmsconf)
                     , startReqMutex = startReqMutexInt active
                     , stopReqMutex = stopReqMutexInt active
                     , runRedis = rrunRedis
                     }
  let cms = CMS{ requestPostSet = Nothing
               , cmsExpireAggregates = wpExpireAggregatesInt rrunRedis
               , cmsExpirePost = wpExpirePostInt rrunRedis
               , cachingGet = cachingGetInt cmsInt
               , cachingGetRetry = cachingGetRetryInt cmsInt
               , cachingGetError = cachingGetErrorInt cmsInt
               , cacheInternals = cmsInt
               , cmsLogger = logf
               }
  let extraFields = cmsConfExtraFields cmsconf
  return (cms, (wordPressSubs <> cmsSubs) cms extraFields getURI cmsLens)
