{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Offset.Cache where

import           Control.Concurrent.MVar
import           Control.Monad           (void)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Monoid             ((<>))
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time.Clock         (UTCTime, diffUTCTime, getCurrentTime)
import           Database.Redis          (Redis)
import           Snap

import           Web.Offset.Cache.Redis
import           Web.Offset.Cache.Types
import           Web.Offset.Types
import           Web.Offset.Utils

startReqMutexInt :: MVar (Map WPKey UTCTime) -> WPKey -> IO Bool
startReqMutexInt activeMV wpKey =
  do now <- getCurrentTime
     modifyMVar activeMV $ \a ->
      let active = filterCurrent now a
      in if Map.member wpKey active
          then return (active, True)
          else return (Map.insert wpKey now active, False)
  where filterCurrent now = Map.filter (\v -> diffUTCTime now v < 1)

stopReqMutexInt :: MVar (Map WPKey UTCTime) -> WPKey -> IO ()
stopReqMutexInt activeMV wpKey =
  modifyMVar_ activeMV $ return . Map.delete wpKey

cachingGetRetryInt :: (WordpressInt b) -> WPKey -> IO Text
cachingGetRetryInt wp = retryUnless . (cachingGetInt wp)

cachingGetErrorInt :: (WordpressInt b) -> WPKey -> IO Text
cachingGetErrorInt wp wpKey = errorUnless msg (cachingGetInt wp wpKey)
  where msg = ("Could not retrieve " <> tshow wpKey)

cachingGetInt :: WordpressInt b
           -> WPKey
           -> IO (Maybe Text)
cachingGetInt WordpressInt{..} wpKey =
  do cached <- wpCacheGet wpKey
     case cached of
       Just _ -> return cached
       Nothing ->
         do running <- startReqMutex wpKey
            if running
               then return Nothing
               else
                 do o <- wpRequest wpKey
                    wpCacheSet wpKey o
                    stopReqMutex wpKey
                    return $ Just o

wpCacheGetInt :: RunRedis -> CacheBehavior -> WPKey -> IO (Maybe Text)
wpCacheGetInt runRedis b = runRedis . (cacheGet b) . formatKey

cacheGet :: CacheBehavior -> Text -> Redis (Maybe Text)
cacheGet NoCache _ = return Nothing
cacheGet _ key = rget key

wpCacheSetInt :: RunRedis -> CacheBehavior -> WPKey -> Text -> IO ()
wpCacheSetInt runRedis b key = void . runRedis . (cacheSet b (formatKey key))

cacheSet :: CacheBehavior -> Text -> Text -> Redis Bool
cacheSet b k v =
  case b of
   (CacheSeconds n) -> rsetex k n v
   CacheForever -> rset k v
   NoCache -> return True

wpExpireAggregatesInt :: RunRedis -> IO Bool
wpExpireAggregatesInt runRedis = runRedis expireAggregates

expireAggregates :: Redis Bool
expireAggregates = rdelstar "wordpress:posts:*"

wpExpirePostInt :: RunRedis -> WPKey -> IO Bool
wpExpirePostInt runRedis = runRedis . expire

expire :: WPKey -> Redis Bool
expire key = rdel [formatKey key] >> expireAggregates

formatKey :: WPKey -> Text
formatKey = format
  where format (PostByPermalinkKey y m s) = ns "post_perma:" <> y <> "_" <> m <> "_" <> s
        format (PostsKey filters) =
          ns "posts:" <> T.intercalate "_" (map tshow $ Set.toAscList filters)
        format (PostKey n) = ns "post:" <> tshow n
        format (AuthorKey n) = ns "author:" <> tshow n
        format (TaxDictKey t) = ns "tax_dict:" <> t
        ns k = "wordpress:" <> k
