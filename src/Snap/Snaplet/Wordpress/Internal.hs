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

import           Control.Concurrent.MVar
import           Control.Lens                       hiding (children)
import           Data.Default
import           Data.IntSet                        (IntSet)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import           Data.Time.Clock
import           Snap                               hiding (path)

import           Snap.Snaplet.Wordpress.Cache.Types
import           Snap.Snaplet.Wordpress.Field
import           Snap.Snaplet.Wordpress.HTTP
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils


data WordpressConfig m =
     WordpressConfig { endpoint      :: Text
                     , requester     :: Maybe Requester
                     , cacheBehavior :: CacheBehavior
                     , extraFields   :: [Field m]
                     , logger        :: Maybe (Text -> IO ())
                     }
instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1/wp-json" Nothing (CacheSeconds 600) [] Nothing

data Wordpress b =
     Wordpress { requestPostSet     :: Maybe IntSet
               , wpExpireAggregates :: IO Bool
               , wpExpirePost       :: WPKey -> IO Bool
               , cachingGet         :: WPKey -> IO (Maybe Text)
               , cachingGetRetry    :: WPKey -> IO Text
               , cachingGetError    :: WPKey -> IO Text
               , cacheInternals     :: WordpressInt b
               }

type WPLens b = Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))

data WordpressInt b =
     WordpressInt { wpCacheGet    :: WPKey -> IO (Maybe Text)
                  , wpCacheSet    :: WPKey -> Text -> IO ()
                  , startReqMutex :: WPKey -> IO Bool
                  , wpRequest     :: WPKey -> IO Text
                  , stopReqMutex  :: WPKey -> IO ()
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
  where req path params = (unRequester runHTTP) (endpt <> path) params

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
  where msg = ("Could not retrieve " <> tshow wpKey)

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
