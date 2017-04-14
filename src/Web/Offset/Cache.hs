{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Offset.Cache where

import           Control.Concurrent     as CC
import           Control.Logging        as L
import           Control.Monad          (void)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime, diffUTCTime, getCurrentTime)
import           Database.Redis         (Redis)

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

cachingGetRetryInt :: WordpressInt b -> WPKey -> IO (Either StatusCode Text)
cachingGetRetryInt wp = retryUnless . cachingGetInt wp

cachingGetErrorInt :: WordpressInt b -> WPKey -> IO (Either StatusCode Text)
cachingGetErrorInt wp wpKey = errorUnless msg (cachingGetInt wp wpKey)
  where msg = "Could not retrieve " <> tshow wpKey

retryUnless :: IO (CacheResult a) -> IO (Either StatusCode a)
retryUnless action =
  do ma <- action
     case ma of
       Successful r -> return $ Right r
       Abort code -> return $ Left code
       Retry -> do CC.threadDelay 100000
                   retryUnless action

errorUnless :: Text -> IO (CacheResult a) -> IO (Either StatusCode a)
errorUnless _ action =
  do ma <- action
     case ma of
      Successful a -> return $ Right a
      Abort code -> return $ Left code
      Retry -> return $ Left 500

cachingGetInt :: WordpressInt b
           -> WPKey
           -> IO (CacheResult Text)
cachingGetInt WordpressInt{..} wpKey =
  do mCached <- wpCacheGet wpKey
     case mCached of
       Just cached -> do log' ("Offset: Cache Hit: " <> tshow wpKey)
                         return $ Successful cached
       Nothing ->
         do running <- startReqMutex wpKey
            if running
               then return Retry
               else
                 do log' ("Offset: Cache Miss: " <> tshow wpKey)
                    o <- wpRequest wpKey
                    case o of
                      Left errorCode ->
                        return $ Abort errorCode
                      Right jsonBlob -> do
                        wpCacheSet wpKey jsonBlob
                        stopReqMutex wpKey
                        return $ Successful jsonBlob

wpCacheGetInt :: RunRedis -> CacheBehavior -> WPKey -> IO (Maybe Text)
wpCacheGetInt runRedis b = runRedis . cacheGet b . formatKey

cacheGet :: CacheBehavior -> Text -> Redis (Maybe Text)
cacheGet NoCache _ = return Nothing
cacheGet _ key = rget key

wpCacheSetInt :: RunRedis -> CacheBehavior -> WPKey -> Text -> IO ()
wpCacheSetInt runRedis b key = void . runRedis . cacheSwitch b key

cacheSwitch :: CacheBehavior -> WPKey -> Text -> Redis Bool
cacheSwitch b k@(EndpointKey t) = cacheSetAlwaysExpire b (formatKey k)
cacheSwitch b k = cacheSet b (formatKey k)

cacheSet :: CacheBehavior -> Text -> Text -> Redis Bool
cacheSet b k v =
  case b of
   (CacheSeconds n) -> rsetex k n v
   CacheForever -> rset k v
   NoCache -> return True

cacheSetAlwaysExpire :: CacheBehavior -> Text -> Text -> Redis Bool
cacheSetAlwaysExpire b k v =
  case b of
   (CacheSeconds n) -> rsetex k n v
   CacheForever -> rsetex k (10 * 60) v -- cache 10 minutes
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
        format (PageKey s) = ns "page:" <> s
        format (AuthorKey n) = ns "author:" <> tshow n
        format (TaxDictKey t) = ns "tax_dict:" <> t
        format (TaxSlugKey tn ts) = ns "tax_slug:" <> tn <> ":" <> ts
        format (EndpointKey e) = ns "endpoint:" <> e
        ns k = "wordpress:" <> k
