
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Wordpress.Cache where

import           Control.Applicative
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.Attoparsec.Text         as A
import           Data.ByteString              (ByteString)
import           Data.Char                    (toUpper)
import qualified Data.Configurator            as C
import           Data.Default
import qualified Data.HashMap.Strict          as M
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.List                    (intercalate)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Map.Syntax
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe,
                                               isJust, listToMaybe)
import           Data.Monoid
import           Data.Ratio
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TL
import           Data.Time.Clock
import qualified Data.Vector                  as V
import           Database.Redis               (Redis)
import qualified Database.Redis               as R
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import qualified Network.Wreq                 as W
import           Snap                         hiding (get)
import           Snap.Snaplet.Heist           (Heist, addConfig)
import           Snap.Snaplet.RedisDB         (RedisDB)
import qualified Snap.Snaplet.RedisDB         as R
import qualified Text.XmlHtml                 as X

import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever deriving (Show, Eq)

cacheGet :: CacheBehavior -> WPKey -> Redis (Maybe Text)
cacheGet b key@PostByPermalinkKey{} =
  do x <- cGet b (formatKey key)
     case x of
      Just key' -> cGet b key'
      Nothing -> return Nothing
cacheGet b key = cGet b (formatKey key)

cGet :: CacheBehavior -> Text -> Redis (Maybe Text)
cGet NoCache _ = return Nothing
cGet _ key = rget key

cacheSet :: CacheBehavior -> WPKey -> Text -> Redis Bool
cacheSet b k v = cSet b (formatKey k) v

cSet :: CacheBehavior -> Text -> Text -> Redis Bool
cSet (CacheSeconds n) k v = rsetex k n v
cSet CacheForever k v = rset k v
cSet NoCache _ _ = return True


rsetex :: Text -> Int -> Text -> Redis Bool
rsetex k n v = isSuccess <$> R.setex (T.encodeUtf8 k) (toInteger n) (T.encodeUtf8 v)

rset :: Text -> Text -> Redis Bool
rset k v = isSuccess <$> R.set (T.encodeUtf8 k) (T.encodeUtf8 v)

isSuccess :: Either a b -> Bool
isSuccess res = case res of
                 Left _err -> False
                 Right _val -> True

rget :: Text -> Redis (Maybe Text)
rget k = (fmap T.decodeUtf8) <$> join <$> eitherToMaybe <$> R.get (T.encodeUtf8 k)
  where eitherToMaybe :: Either a b -> Maybe b
        eitherToMaybe e =
          case e of
           Right a -> Just a
           Left _err -> Nothing


rdel :: Text -> Redis Bool
rdel k = isSuccess <$> R.del [T.encodeUtf8 k]

formatKey :: WPKey -> Text
formatKey = format
  where format (PostByPermalinkKey y m s) = "wordpress:post_perma:" <> y <> "_" <> m <> "_" <> s
        format (PostsKey filters) =
          "wordpress:posts:" <> T.intercalate "_" (map tshow $ Set.toAscList filters)
        format (PostKey n) = "wordpress:post:" <> tshow n

expireAggregates :: Redis Bool
expireAggregates =
  do r <- R.eval "return redis.call('del', unpack(redis.call('keys', ARGV[1])))" [] ["wordpress:posts:*"]
     case r of
      Left _err -> return False
      Right (_ :: Integer) -> return True

expirePost :: Int -> Redis Bool
expirePost i = rdel (formatKey (PostKey i)) >> expireAggregates
