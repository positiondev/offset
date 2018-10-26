{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Offset.Types (
  attrToTaxSpecList
, CacheResult(..)
, CatType(..)
, decodeWPResponseBody
, Filter(..)
, Requester(..)
, H.ResponseHeaders
, StatusCode
, TagType(..)
, TaxDict(..)
, TaxonomyName(..)
, TaxRes(..)
, TaxSpec(..)
, TaxSpecId(..)
, TaxSpecList(..)
, UserPassword
, Wordpress(..)
, WordpressConfig(..)
, WordpressInt(..)
, WPKey(..)
, WPLens
, WPQuery(..)
, WPResponse(..)
) where

import           Control.Lens             hiding (children)
import           Control.Monad.State
import           Data.Aeson               (FromJSON, Value (..), parseJSON, (.:), ToJSON(..))
import           Data.Default
import qualified Data.ByteString          as BS
import           GHC.Generics
import qualified Data.CaseInsensitive      as CI
import           Data.IntSet              (IntSet)
import           Data.List                (intercalate)
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes, isJust)
import           Data.Monoid              ((<>))
import qualified Data.Semigroup           as SG
import           Data.Set                 (Set)
import           Data.Text                (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Network.HTTP.Types.Header as H

import           Web.Offset.Cache.Types
import           Web.Offset.Field
import           Web.Offset.Utils

data Wordpress b =
     Wordpress { requestPostSet     :: Maybe IntSet
               , wpExpireAggregates :: IO Bool
               , wpExpirePost       :: WPKey -> IO Bool
               , cachingGet         :: WPKey -> IO (CacheResult WPResponse)
               , cachingGetRetry    :: WPKey -> IO (Either StatusCode WPResponse)
               , cachingGetError    :: WPKey -> IO (Either StatusCode WPResponse)
               , wpLogger           :: Text -> IO ()
               , cacheInternals     :: WordpressInt (StateT b IO Text)
               }

type WPLens b s = Lens' s (Wordpress b)

type UserPassword = (Text, Text)

data WPResponse = WPResponse { wpHeaders :: H.ResponseHeaders
                             , wpBody :: Text } deriving (Eq, Show, Generic)

decodeWPResponseBody :: FromJSON a => WPResponse -> Maybe a
decodeWPResponseBody (WPResponse _ body) = decodeJson body

instance ToJSON WPResponse
instance FromJSON WPResponse

instance ToJSON (CI.CI BS.ByteString) where
  toJSON str = toJSON $ T.toLower $ T.decodeUtf8 $ CI.original str
instance FromJSON (CI.CI BS.ByteString) where
  parseJSON (String str) = return $ CI.mk $ T.encodeUtf8 str
  parseJSON _ = mzero

instance ToJSON BS.ByteString where
  toJSON str = toJSON $ T.decodeUtf8 str
instance FromJSON BS.ByteString where
  parseJSON (String str) = return $ T.encodeUtf8 str

newtype Headers = Headers { headers :: M.Map Text Text} deriving (Show, Eq, Generic)

instance SG.Semigroup Headers where
  (Headers h1) <> (Headers h2) = Headers (h1 SG.<> h2)

instance Monoid Headers where
  mempty = Headers mempty
  mappend h1 h2 = h1 SG.<> h2

instance ToJSON Headers

newtype Requester = Requester { unRequester :: Text
                                            -> [(Text, Text)]
                                            -> IO (Either Int WPResponse) }

data WordpressConfig m =
     WordpressConfig { wpConfEndpoint      :: Text
                     , wpConfRequester     :: Either UserPassword Requester
                     , wpConfCacheBehavior :: CacheBehavior
                     , wpConfExtraFields   :: [Field m]
                     , wpConfLogger        :: Maybe (Text -> IO ())
                     }

instance Default (WordpressConfig m) where
  def = WordpressConfig "http://127.0.0.1:8080/wp-json"
                        (Left ("offset", "111"))
                        (CacheSeconds 600)
                        []
                        Nothing

data WordpressInt b =
     WordpressInt { wpCacheGet    :: WPKey -> IO (Maybe Text)
                  , wpCacheSet    :: WPKey -> Text -> IO ()
                  , startReqMutex :: WPKey -> IO Bool
                  , wpRequest     :: WPKey -> IO (Either StatusCode WPResponse)
                  , stopReqMutex  :: WPKey -> IO ()
                  , runRedis      :: RunRedis
                  }

data TaxSpec = TaxPlus Text | TaxMinus Text deriving (Eq, Ord)

data TaxSpecId = TaxPlusId Int | TaxMinusId Int deriving (Eq, Show, Ord)

data CatType
data TagType
type CustomType = Text

instance Show TaxSpec where
  show (TaxPlus t) = '+' : T.unpack t
  show (TaxMinus t) = '-' : T.unpack t

newtype TaxRes = TaxRes (Int, Text) deriving (Show)

instance FromJSON TaxRes where
  parseJSON (Object o) = TaxRes <$> ((,) <$> o .: "id" <*> o .: "slug")
  parseJSON _ = mzero

data TaxDict = TaxDict { dict :: [TaxRes]
                       , desc :: Text} deriving (Show)

type Year = Text
type Month = Text
type Slug = Text
type TaxonomyName = Text

data Filter = TaxFilter TaxonomyName TaxSpecId
            | NumFilter Int
            | OffsetFilter Int
            | UserFilter Text
            deriving (Eq, Ord)

instance Show Filter where
  show (TaxFilter n t) = show n ++ "_" ++ show t
  show (NumFilter n) = "num_" ++ show n
  show (OffsetFilter n) = "offset_" ++ show n
  show (UserFilter u) = T.unpack $ "user_" <> u

data WPKey = PostKey Int
           | PostByPermalinkKey Year Month Slug
           | PostsKey (Set Filter)
           | PageKey Text
           | AuthorKey Int
           | TaxDictKey Text
           | TaxSlugKey TaxonomyName Slug
           | EndpointKey Text
           deriving (Eq, Show, Ord)

tagChars :: String
tagChars = ['a'..'z'] ++ "-" ++ digitChars

digitChars :: String
digitChars = ['0'..'9']

instance Read TaxSpec where
  readsPrec _ ('+':cs) | not (null cs) && all (`elem` tagChars) cs = [(TaxPlus (T.pack cs), "")]
  readsPrec _ ('-':cs) | not (null cs) && all (`elem` tagChars) cs = [(TaxMinus (T.pack cs), "")]
  readsPrec _ cs | not (null cs) && all (`elem` tagChars) cs       = [(TaxPlus (T.pack cs), "")]
  readsPrec _ _ = []

instance Read TaxSpecId where
  readsPrec _ ('+':cs) | not (null cs) && all (`elem` digitChars) cs = [(TaxPlusId (read cs), "")]
  readsPrec _ ('-':cs) | not (null cs) && all (`elem` digitChars) cs = [(TaxMinusId (read cs), "")]
  readsPrec _ cs       | not (null cs) && all (`elem` digitChars) cs = [(TaxPlusId (read cs), "")]
  readsPrec _ _ = []

data TaxSpecList = TaxSpecList { taxName :: TaxonomyName
                               , taxList :: [TaxSpec]} deriving (Eq, Ord)

instance Show TaxSpecList where
  show (TaxSpecList n ts) = T.unpack n ++ ": " ++ intercalate "," (map show ts)

attrToTaxSpecList :: (Text, Text) -> TaxSpecList
attrToTaxSpecList (k, ts) =
  let vs = map readSafe $ T.splitOn "," ts in
  if all isJust vs
  then TaxSpecList k (catMaybes vs)
  else TaxSpecList k []

data WPQuery = WPPostsQuery{ qlimit  :: Int
                           , qnum    :: Int
                           , qoffset :: Int
                           , qpage   :: Int
                           , qtaxes  :: [TaxSpecList]
                           , quser   :: Maybe Text
                           } deriving (Show)

type StatusCode = Int

data CacheResult a = Successful a -- cache worked as expected
                   | Retry -- cache didn't work, but keep trying
                   | Abort StatusCode -- we got a 404 or something, no need to retry
  deriving (Show, Functor)
