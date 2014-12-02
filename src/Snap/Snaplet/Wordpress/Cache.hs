{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Snap.Snaplet.Wordpress.Cache where

import           Control.Monad                         (void)
import           Data.Text                             (Text)
import           Database.Redis                        (Redis)

import           Snap.Snaplet.Wordpress.Cache.Internal
import           Snap.Snaplet.Wordpress.Types

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever deriving (Show, Eq)
type RunRedis = forall a. Redis a -> IO a

wpCacheGetInt :: RunRedis -> CacheBehavior -> WPKey -> IO (Maybe Text)
wpCacheGetInt runRedis b = runRedis . (cacheGet b) . formatKey

wpCacheSetInt :: RunRedis -> CacheBehavior -> WPKey -> Text -> IO ()
wpCacheSetInt runRedis b key = void . runRedis . (cacheSet b (formatKey key))

wpExpireAggregatesInt :: RunRedis -> IO Bool
wpExpireAggregatesInt runRedis = runRedis expireAggregates

wpExpirePostInt :: RunRedis -> WPKey -> IO Bool
wpExpirePostInt runRedis = runRedis . expire

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

expire :: WPKey -> Redis Bool
expire key = rdel [formatKey key] >> expireAggregates
