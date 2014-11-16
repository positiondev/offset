module Snap.Snaplet.Wordpress.Cache where

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever deriving (Show, Eq)
