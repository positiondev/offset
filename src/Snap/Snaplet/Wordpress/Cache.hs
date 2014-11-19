
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
import           Control.Lens
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
import           Snap
import           Snap.Snaplet.Heist           (Heist, addConfig)
import           Snap.Snaplet.RedisDB         (RedisDB)
import qualified Snap.Snaplet.RedisDB         as R
import qualified Text.XmlHtml                 as X

import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

data CacheBehavior = NoCache | CacheSeconds Int | CacheForever deriving (Show, Eq)

cacheGet :: R.RedisCtx m (Either t) => CacheBehavior -> WPKey -> m (Maybe Text)
cacheGet NoCache _ = return Nothing
cacheGet _ key =
  do
    res <- R.get (formatKey key)
    case res of
     Right (Just val) ->
       case key of
        PostByPermalinkKey{} ->
          do res' <- R.get val
             case res' of
              Left _err -> return Nothing
              Right val -> return (T.decodeUtf8 <$> val)
        _ -> return (Just $ T.decodeUtf8 val)
     _ -> return Nothing

cacheSet :: R.RedisCtx m (Either t) => CacheBehavior -> WPKey -> Text -> m Bool
cacheSet NoCache _ _ = return True
cacheSet b key o =
  do res <- case key of
             PostByPermalinkKey{} ->
               do let (Just p) = decodeStrict . T.encodeUtf8 $ o
                      (i,_) = extractPostId p
                  r <- cset b (formatKey $ PostKey i) (T.encodeUtf8 o)
                  case r of
                   Left _err -> return r
                   Right _ ->
                     cset b (formatKey key) (formatKey $ PostKey i)
             _ -> cset b (formatKey key) (T.encodeUtf8 o)
     case res of
      Left _err -> return False
      Right _val -> return True
  where cset (CacheSeconds n) k v = R.setex k (toInteger n) v
        cset CacheForever k v = R.set k v
        cset NoCache _ v = return $ return $ R.Status v

formatKey :: WPKey -> ByteString
formatKey = T.encodeUtf8 . format
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
expirePost i =
       do r1 <- R.del [formatKey $ PostKey i]
          case r1 of
           Left _err -> return False
           _ -> expireAggregates