{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Wordpress.Cache where

import           Data.Monoid                           ((<>))
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Database.Redis                        (Redis)
import           Snap

import           Snap.Snaplet.Wordpress.Cache.Redis
import           Snap.Snaplet.Wordpress.Cache.Types
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

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

formatKey :: WPKey -> Text
formatKey = format
  where format (PostByPermalinkKey y m s) = "wordpress:post_perma:" <> y <> "_" <> m <> "_" <> s
        format (PostsKey filters) =
          "wordpress:posts:" <> T.intercalate "_" (map tshow $ Set.toAscList filters)
        format (PostKey n) = "wordpress:post:" <> tshow n
        format (TaxDictKey t) = "wordpress:tax_dict:" <> t
