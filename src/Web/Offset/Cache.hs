{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Offset.Cache where

import           Control.Concurrent     as CC
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

startReqMutexInt :: MVar (Map CMSKey UTCTime) -> CMSKey -> IO Bool
startReqMutexInt activeMV wpKey =
  do now <- getCurrentTime
     modifyMVar activeMV $ \a ->
      let active = filterCurrent now a
      in if Map.member wpKey active
          then return (active, True)
          else return (Map.insert wpKey now active, False)
  where filterCurrent now = Map.filter (\v -> diffUTCTime now v < 1)

stopReqMutexInt :: MVar (Map CMSKey UTCTime) -> CMSKey -> IO ()
stopReqMutexInt activeMV wpKey =
  modifyMVar_ activeMV $ return . Map.delete wpKey

cachingGetRetryInt :: CMSInt b -> CMSKey -> IO (Either StatusCode Text)
cachingGetRetryInt wp = retryUnless . cachingGetInt wp

cachingGetErrorInt :: CMSInt b -> CMSKey -> IO (Either StatusCode Text)
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

cachingGetInt :: CMSInt b
           -> CMSKey
           -> IO (CacheResult Text)
cachingGetInt CMSInt{..} wpKey =
  do mCached <- cmsCacheGet wpKey
     case mCached of
       Just cached -> return $ Successful cached
       Nothing ->
         do running <- startReqMutex wpKey
            if running
               then return Retry
               else
                 do o <- cmsRequest wpKey
                    case o of
                      Left errorCode ->
                        return $ Abort errorCode
                      Right jsonBlob -> do
                        cmsCacheSet wpKey jsonBlob
                        stopReqMutex wpKey
                        return $ Successful jsonBlob

cmsCacheGetInt :: RunRedis -> CacheBehavior -> CMSKey -> IO (Maybe Text)
cmsCacheGetInt runRedis b = runRedis . cacheGet b . formatKey

cacheGet :: CacheBehavior -> Text -> Redis (Maybe Text)
cacheGet NoCache _ = return Nothing
cacheGet _ key = rget key

cmsCacheSetInt :: RunRedis -> CacheBehavior -> CMSKey -> Text -> IO ()
cmsCacheSetInt runRedis b key = void . runRedis . cacheSet b (formatKey key)

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

wpExpirePostInt :: RunRedis -> CMSKey -> IO Bool
wpExpirePostInt runRedis = runRedis . expire

expire :: CMSKey -> Redis Bool
expire key = rdel [formatKey key] >> expireAggregates

formatKey :: CMSKey -> Text
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
