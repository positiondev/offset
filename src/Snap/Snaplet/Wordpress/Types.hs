{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Wordpress.Types where

import           Control.Lens                 hiding (children)
import           Data.Aeson                   (FromJSON, Value (..), parseJSON,
                                               (.:))
import           Data.IntSet                  (IntSet)
import           Data.List                    (intercalate)
import           Data.Maybe                   (catMaybes, isJust)
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Snap

import           Snap.Snaplet.Wordpress.Cache.Types
import           Snap.Snaplet.Wordpress.Field
import           Snap.Snaplet.Wordpress.Utils
import           Snap.Snaplet.Wordpress.HTTP

data Wordpress b =
     Wordpress { requestPostSet     :: Maybe IntSet
               , wpExpireAggregates :: IO Bool
               , wpExpirePost       :: WPKey -> IO Bool
               , cachingGet         :: WPKey -> IO (Maybe Text)
               , cachingGetRetry    :: WPKey -> IO Text
               , cachingGetError    :: WPKey -> IO Text
               , wpLogger           :: Text -> IO ()
               , cacheInternals     :: WordpressInt b
               }

type WPLens b = Lens b b (Snaplet (Wordpress b)) (Snaplet (Wordpress b))

data WordpressConfig m =
     WordpressConfig { wpConfEndpoint      :: Text
                     , wpConfRequester     :: Maybe Requester
                     , wpConfCacheBehavior :: CacheBehavior
                     , wpConfExtraFields   :: [Field m]
                     , wpConfLogger        :: Maybe (Text -> IO ())
                     }

data WordpressInt b =
     WordpressInt { wpCacheGet    :: WPKey -> IO (Maybe Text)
                  , wpCacheSet    :: WPKey -> Text -> IO ()
                  , startReqMutex :: WPKey -> IO Bool
                  , wpRequest     :: WPKey -> IO Text
                  , stopReqMutex  :: WPKey -> IO ()
                  }

data TaxSpec a = TaxPlus Text | TaxMinus Text deriving (Eq, Ord)

data TaxSpecId a = TaxPlusId Int | TaxMinusId Int deriving (Eq, Show, Ord)

data CatType
data TagType

instance Show (TaxSpec a) where
  show (TaxPlus t) = "+" ++ (T.unpack t)
  show (TaxMinus t) = "-" ++ (T.unpack t)

newtype TaxRes a = TaxRes (Int, Text)

instance FromJSON (TaxRes a) where
  parseJSON (Object o) = TaxRes <$> ((,) <$> o .: "ID" <*> o .: "slug")
  parseJSON _ = mzero

data TaxDict a = TaxDict {dict :: [TaxRes a], desc :: Text}

type Year = Text
type Month = Text
type Slug = Text
data Filter = TagFilter (TaxSpecId TagType)
            | CatFilter (TaxSpecId CatType)
            | NumFilter Int
            | OffsetFilter Int
            deriving (Eq, Ord)

instance Show Filter where
  show (TagFilter t) = "tag_" ++ show t
  show (CatFilter t) = "cat_" ++ show t
  show (NumFilter n) = "num_" ++ show n
  show (OffsetFilter n) = "offset_" ++ show n

data WPKey = PostKey Int
           | PostByPermalinkKey Year Month Slug
           | PostsKey (Set Filter)
           | AuthorKey Int
           | TaxDictKey Text
           deriving (Eq, Show, Ord)

tagChars :: String
tagChars = ['a'..'z'] ++ "-"

digitChars :: String
digitChars = ['0'..'9']

instance Read (TaxSpec a) where
  readsPrec _ ('+':cs) | not (null cs) && all (`elem` tagChars) cs = [(TaxPlus (T.pack cs), "")]
  readsPrec _ ('-':cs) | not (null cs) && all (`elem` tagChars) cs = [(TaxMinus (T.pack cs), "")]
  readsPrec _ cs | not (null cs) && all (`elem` tagChars) cs       = [(TaxPlus (T.pack cs), "")]
  readsPrec _ _ = []

instance Read (TaxSpecId a) where
  readsPrec _ ('+':cs) | not (null cs) && all (`elem` digitChars) cs = [(TaxPlusId (read cs), "")]
  readsPrec _ ('-':cs) | not (null cs) && all (`elem` digitChars) cs = [(TaxMinusId (read cs), "")]
  readsPrec _ cs       | not (null cs) && all (`elem` digitChars) cs = [(TaxPlusId (read cs), "")]
  readsPrec _ _ = []

newtype TaxSpecList a = TaxSpecList { unTaxSpecList :: [TaxSpec a]} deriving (Eq, Ord)

instance Show (TaxSpecList a) where
  show (TaxSpecList ts) = intercalate "," (map show ts)

instance Read (TaxSpecList a) where
  readsPrec _ ts = let vs = map (readSafe) $ T.splitOn "," $ T.pack ts in
                     if all isJust vs
                        then [(TaxSpecList $ catMaybes vs, "")]
                        else []

data WPQuery = WPPostsQuery{ qlimit  :: Int
                           , qnum    :: Int
                           , qoffset :: Int
                           , qpage   :: Int
                           , qtags   :: TaxSpecList TagType
                           , qcats   :: TaxSpecList CatType}
