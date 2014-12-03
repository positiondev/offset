{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Wordpress.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.List                    (intercalate)
import           Data.Maybe                   (catMaybes, isJust)
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Snap

import           Snap.Snaplet.Wordpress.Utils

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
           | TaxDictKey Text
           deriving (Eq, Show, Ord)

formatKey :: WPKey -> Text
formatKey = format
  where format (PostByPermalinkKey y m s) = "wordpress:post_perma:" <> y <> "_" <> m <> "_" <> s
        format (PostsKey filters) =
          "wordpress:posts:" <> T.intercalate "_" (map tshow $ Set.toAscList filters)
        format (PostKey n) = "wordpress:post:" <> tshow n
        format (TaxDictKey t) = "wordpress:tax_dict:" <> t

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
