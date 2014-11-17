{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.Wordpress.Types where

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

import           Snap.Snaplet.Wordpress.Utils

data TaxSpec = TaxPlus Text | TaxMinus Text deriving (Eq, Ord)

data TaxSpecId = TaxPlusId Int | TaxMinusId Int deriving (Eq, Show, Ord)

instance Show TaxSpec where
  show (TaxPlus t) = "+" ++ (T.unpack t)
  show (TaxMinus t) = "-" ++ (T.unpack t)


type Year = Text
type Month = Text
type Slug = Text
data Filter = TagFilter TaxSpecId
            | CatFilter TaxSpecId
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
              deriving (Eq, Show, Ord)



instance Read TaxSpec where
  readsPrec _ ('+':cs) | all (`elem` tagChars) cs = [(TaxPlus (T.pack cs), "")]
  readsPrec _ ('-':cs) | all (`elem` tagChars) cs = [(TaxMinus (T.pack cs), "")]
  readsPrec _ cs | all (`elem` tagChars) cs = [(TaxPlus (T.pack cs), "")]
  readsPrec _ _ = []

tagChars :: String
tagChars = ['a'..'z'] ++ "-"

newtype TaxSpecList = TaxSpecList { unTaxSpecList :: [TaxSpec]} deriving (Eq, Ord)

instance Show TaxSpecList where
  show (TaxSpecList ts) = intercalate "," (map show ts)

instance Read TaxSpecList where
  readsPrec _ ts = let vs = map (readSafe) $ T.splitOn "," $ T.pack ts in
                     if all isJust vs
                        then [(TaxSpecList $ catMaybes vs, "")]
                        else []
