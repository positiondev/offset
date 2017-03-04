{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Offset.WordPress.Types where

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
import           Web.Offset.Types
import           Web.Offset.Utils

data TaxSpec = TaxPlus Text | TaxMinus Text deriving (Eq, Ord)

data TaxSpecId = TaxPlusId Int | TaxMinusId Int deriving (Eq, Show, Ord)

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
           deriving (Eq, Show, Ord)

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
  where ns k = "wordpress:" <> k

buildParams' :: Set.Set Filter -> [(Text, Text)]
buildParams' filters = params
  where params = Set.toList $ Set.map mkFilter filters
        mkFilter (TaxFilter taxonomyName (TaxPlusId i)) = (taxonomyName <> "[]", tshow i)
        mkFilter (TaxFilter taxonomyName (TaxMinusId i)) = (taxonomyName <> "_exclude[]", tshow i)
        mkFilter (NumFilter num) = ("per_page", tshow num)
        mkFilter (OffsetFilter offset) = ("offset", tshow offset)
        mkFilter (UserFilter user) = ("author[]", user)

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
