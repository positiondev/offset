{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyDataDecls        #-}
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

data CMSKey = CMSKey { cRequestUrl :: (Text, [(Text, Text)])
                     , cFormatKey  :: Text
                     , cShow       :: Text }

instance Ord CMSKey where
  compare key1 key2 = compare (cShow key1) (cShow key2)
instance Eq CMSKey where
  key1 == key2 = cShow key1 == cShow key2

toCMSKey :: WPKey -> CMSKey
toCMSKey wpKey =
  case wpKey of
    PostKey i ->
      CMSKey ("/wp/v2/posts/" <> tshow i, [])
             (ns "post:" <> tshow i)
             (tshow wpKey)
    PostByPermalinkKey y m s ->
      CMSKey ("/wp/v2/posts", [("slug", s)])
          (ns "post_perma:" <> y <> "_" <> m <> "_" <> s)
             (tshow wpKey)
    PostsKey filters ->
      CMSKey ("/wp/v2/posts", buildParams' filters)
              (ns "posts:" <> T.intercalate "_"
                          (map tshow $ Set.toAscList filters))
             (tshow wpKey)
    PageKey slug ->
      CMSKey ("/wp/v2/pages", [("slug", slug)])
          (ns "page:" <> slug)
             (tshow wpKey)
    AuthorKey i ->
      CMSKey ("/wp/v2/users/" <> tshow i, [])
          (ns "author:" <> tshow i)
             (tshow wpKey)
    TaxDictKey resName ->
      CMSKey ("/wp/v2/" <> resName, [])
          (ns "tax_dict:" <> resName)
             (tshow wpKey)
    TaxSlugKey tn slug ->
      CMSKey ("/wp/v2/" <> tn, [("slug", slug)])
          (ns "tax_slug:" <> tn <> ":" <> slug)
             (tshow wpKey)
    EndpointKey endpoint ->
      CMSKey ("/" <> endpoint, [])
          (ns "endpoint:" <> endpoint)
             (tshow wpKey)
  where ns k = "wordpress:" <> k

buildParams' :: Set.Set Filter -> [(Text, Text)]
buildParams' filters = params
  where params = Set.toList $ Set.map mkFilter filters
        mkFilter (TaxFilter taxonomyName (TaxPlusId i)) = (taxonomyName <> "[]", tshow i)
        mkFilter (TaxFilter taxonomyName (TaxMinusId i)) = (taxonomyName <> "_exclude[]", tshow i)
        mkFilter (NumFilter num) = ("per_page", tshow num)
        mkFilter (OffsetFilter offset) = ("offset", tshow offset)
        mkFilter (UserFilter user) = ("author[]", user)

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
