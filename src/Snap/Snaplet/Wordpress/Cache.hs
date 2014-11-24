{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Wordpress.Cache where

import           Data.Monoid
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Database.Redis                        (Redis)
import qualified Database.Redis                        as R

import           Snap.Snaplet.Wordpress.Cache.Internal
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever deriving (Show, Eq)

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
expireAggregates =
  do r <- R.eval "return redis.call('del', unpack(redis.call('keys', ARGV[1])))" [] ["wordpress:posts:*"]
     case r of
      Left _err -> return False
      Right (_ :: Integer) -> return True

expirePost :: Int -> Redis Bool
expirePost i = rdel (formatKey (PostKey i)) >> expireAggregates

