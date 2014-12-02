{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Snap.Snaplet.Wordpress.Cache where

import           Control.Monad                         (void)
import           Data.Text                             (Text)
import           Database.Redis                        (Redis)

import           Snap.Snaplet.Wordpress.Cache.Internal
import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever deriving (Show, Eq)
type RunRedis = forall a. Redis a -> IO a

wpCacheGetInt :: RunRedis -> CacheBehavior -> WPKey -> IO (Maybe Text)
wpCacheGetInt runRedis b wpKey =
  runRedis $
    do case wpKey of
        key@PostByPermalinkKey{} ->
          (cacheGet b) =<<< (cacheGet b (formatKey key))
        key -> cacheGet b (formatKey key)

wpCacheSetInt :: RunRedis -> CacheBehavior -> WPKey -> Text -> IO ()
wpCacheSetInt runRedis b key o =
  void $ runRedis $
    do case key of
        PostByPermalinkKey{} -> do
          let (Just p) = decode o
              (i,_) = extractPostId p
          (cacheSet b (formatKey $ PostKey i) o) >> cacheSet b (formatKey key) (formatKey $ PostKey i)
        _ -> cacheSet b (formatKey key) o

wpExpireAggregatesInt :: RunRedis -> IO Bool
wpExpireAggregatesInt runRedis = runRedis expireAggregates

wpExpirePostInt :: RunRedis -> Int -> IO Bool
wpExpirePostInt runRedis i = runRedis $ expirePost i

cacheGet :: CacheBehavior -> Text -> Redis (Maybe Text)
cacheGet NoCache _ = return Nothing
cacheGet _ key = rget key

cacheSet :: CacheBehavior -> Text -> Text -> Redis Bool
cacheSet b k v =
  case b of
   (CacheSeconds n) -> rsetex k n v
   CacheForever -> rset k v
   NoCache -> return True

expireAggregates :: Redis Bool
expireAggregates = rdelstar "wordpress:posts:*"

expirePost :: Int -> Redis Bool
expirePost i = rdel [formatKey (PostKey i)] >> expireAggregates
