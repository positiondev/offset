{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Cache.Types where

import           Database.Redis (Redis)

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever Int deriving (Show, Eq)
type RunRedis = forall a. Redis a -> IO a
