{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Offset.Types where

import           Control.Lens           hiding (children)
import           Control.Monad.State
import           Data.Aeson             (FromJSON, Value (..), parseJSON, (.:))
import           Data.Default
import           Data.IntSet            (IntSet)
import           Data.List              (intercalate)
import           Data.Maybe             (catMaybes, isJust)
import           Data.Monoid            ((<>))
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Web.Offset.Cache.Types
import           Web.Offset.Field
import           Web.Offset.HTTP
import           Web.Offset.Utils

data CMS b =
     CMS { requestPostSet      :: Maybe IntSet
         , cmsExpireAggregates :: IO Bool
         , cmsExpirePost       :: CMSKey -> IO Bool
         , cachingGet          :: CMSKey -> IO (CacheResult Text)
         , cachingGetRetry     :: CMSKey -> IO (Either StatusCode Text)
         , cachingGetError     :: CMSKey -> IO (Either StatusCode Text)
         , cmsLogger           :: Text -> IO ()
         , cacheInternals      :: CMSInt (StateT b IO Text)
         }

type CMSLens b s = Lens' s (CMS b)

type UserPassword = (Text, Text)

data CMSConfig m =
     CMSConfig { cmsConfEndpoint      :: Text
               , cmsConfRequest       :: Either UserPassword Requester
               , cmsConfCacheBehavior :: CacheBehavior
               , cmsConfExtraFields   :: [Field m]
               , cmsConfLogger        :: Maybe (Text -> IO ())
               }

instance Default (CMSConfig m) where
  def = CMSConfig "http://127.0.0.1:8080/wp-json"
                  (Left ("offset", "111"))
                  (CacheSeconds 600)
                  []
                  Nothing

data CMSInt b =
     CMSInt { cmsCacheGet   :: CMSKey -> IO (Maybe Text)
            , cmsCacheSet   :: CMSKey -> Text -> IO ()
            , startReqMutex :: CMSKey -> IO Bool
            , cmsRequest    :: CMSKey -> IO (Either StatusCode Text)
            , stopReqMutex  :: CMSKey -> IO ()
            , runRedis      :: RunRedis
            }

data BlankPrefix = DefaultPrefix | Prefix Text

toPrefix :: BlankPrefix -> Text
toPrefix DefaultPrefix = "cms"
toPrefix (Prefix p) = p

data CMSKey = CMSKey { cRequestUrl :: (Text, [(Text, Text)])
                     , cFormatKey  :: Text
                     , cShow       :: Text }

instance Ord CMSKey where
  compare key1 key2 = compare (cShow key1) (cShow key2)
instance Eq CMSKey where
  key1 == key2 = cShow key1 == cShow key2

type StatusCode = Int

data CacheResult a = Successful a -- cache worked as expected
                   | Retry -- cache didn't work, but keep trying
                   | Abort StatusCode -- we got a 404 or something, no need to retry
  deriving (Show, Functor)
