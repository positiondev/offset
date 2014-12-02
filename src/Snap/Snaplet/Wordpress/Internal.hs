{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Wordpress.Internal where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.MVar
import           Control.Lens
import           Data.Aeson                   hiding (decode, encode)
import qualified Data.Attoparsec.Text         as A
import           Data.Char                    (toUpper)
import qualified Data.Configurator            as C
import           Data.Default
import qualified Data.HashMap.Strict          as M
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Map.Syntax
import           Data.Maybe                   (fromJust, fromMaybe)
import           Data.Monoid
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
import           Snap                         hiding (path)
import           Snap.Snaplet.Heist           (Heist, addConfig)
import           Snap.Snaplet.RedisDB         (RedisDB)
import qualified Snap.Snaplet.RedisDB         as R
import qualified Text.XmlHtml                 as X

import           Snap.Snaplet.Wordpress.Cache
import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

newtype Requester = Requester { unRequester :: forall a. Text -> [(Text, Text)] -> (Text -> a) -> IO a}

data WordpressInt b =
     WordpressInt { wpCacheGet :: WPKey -> IO (Maybe Text)
                  , wpCacheSet :: WPKey -> Text -> IO ()
                  , startReqMutex :: WPKey -> IO Bool
                  , wpRequest :: WPKey -> IO Text
                  , stopReqMutex :: WPKey -> IO ()
                  }

wpRequestInt :: Requester -> Text -> WPKey -> IO Text
wpRequestInt runHTTP endpt key =
  case key of
   TaxDictKey resName -> req ("/taxonomies/" <> resName <> "/terms") []
   PostByPermalinkKey year month slug ->
     req "/posts" [("filter[year]", year)
                  ,("filter[monthnum]", month)
                  ,("filter[name]", slug)]
   PostsKey{} -> req "/posts" (buildParams key)
   PostKey i -> req ("/posts" <> tshow i) []
  where req path params = (unRequester runHTTP) (endpt <> path) params id

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

cachingGetRetryInt :: (WordpressInt b) -> WPKey -> IO Text
cachingGetRetryInt wp = retryUnless . (cachingGetInt wp)

cachingGetErrorInt :: (WordpressInt b) -> WPKey -> IO Text
cachingGetErrorInt wp wpKey = errorUnless msg (cachingGetInt wp wpKey)
  where msg = ("Could not retrieve " <> formatKey wpKey)

cachingGetInt :: WordpressInt b
           -> WPKey
           -> IO (Maybe Text)
cachingGetInt WordpressInt{..} wpKey =
  do cached <- wpCacheGet wpKey
     case cached of
       Just _ -> return cached
       Nothing ->
         do running <- startReqMutex wpKey
            if running
               then return Nothing
               else
                 do o <- wpRequest wpKey
                    wpCacheSet wpKey o
                    stopReqMutex wpKey
                    return $ Just o

buildParams :: WPKey -> [(Text, Text)]
buildParams (PostsKey filters) = params
  where params = Set.toList $ Set.map mkFilter filters
        mkFilter (TagFilter (TaxPlusId i)) = ("filter[tag__in]", tshow i)
        mkFilter (TagFilter (TaxMinusId i)) = ("filter[tag__not_in]", tshow i)
        mkFilter (CatFilter (TaxPlusId i)) = ("filter[category__in]", tshow i)
        mkFilter (CatFilter (TaxMinusId i)) = ("filter[category__not_in]", tshow i)
        mkFilter (NumFilter num) = ("filter[posts_per_page]", tshow num)
        mkFilter (OffsetFilter offset) = ("filter[offset]", tshow offset)
