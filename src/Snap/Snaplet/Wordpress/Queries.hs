{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Wordpress.Queries where

import           Control.Concurrent.MVar
import           Control.Lens                       hiding (children)
import           Data.Aeson                         hiding (decode, encode)
import qualified Data.Attoparsec.Text               as A
import           Data.Char                          (toUpper)
import qualified Data.Configurator                  as C
import           Data.Default
import qualified Data.HashMap.Strict                as M
import           Data.IntSet                        (IntSet)
import qualified Data.IntSet                        as IntSet
import qualified Data.Map                           as Map
import           Data.Map.Syntax
import           Data.Maybe                         (fromJust, fromMaybe)
import           Data.Monoid
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Database.Redis                     as R
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import           Snap                               hiding (path, rqURI)
import           Snap.Snaplet.Heist                 (Heist, addConfig)
import           Snap.Snaplet.RedisDB               (RedisDB)
import qualified Snap.Snaplet.RedisDB               as RDB
import qualified Text.XmlHtml                       as X

import           Snap.Snaplet.Wordpress.Cache
import           Snap.Snaplet.Wordpress.Cache.Types
import           Snap.Snaplet.Wordpress.Field
import           Snap.Snaplet.Wordpress.HTTP
import           Snap.Snaplet.Wordpress.Internal
import           Snap.Snaplet.Wordpress.Posts
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

lookupTaxDict :: WPKey -> Wordpress b -> IO (TaxSpec a -> TaxSpecId a)
lookupTaxDict key@(TaxDictKey resName) Wordpress{..} =
  do res <- decodeJsonErr <$> cachingGetError key
     return (getSpecId $ TaxDict res resName)

getSpecId :: TaxDict a -> TaxSpec a -> TaxSpecId a
getSpecId taxDict spec =
  case spec of
   TaxPlus slug -> TaxPlusId $ idFor taxDict slug
   TaxMinus slug -> TaxMinusId $ idFor taxDict slug
  where
    idFor :: TaxDict a -> Text -> Int
    idFor (TaxDict{..}) slug =
      case filter (\(TaxRes (_,s)) -> s == slug) dict of
       [] -> terror $ "Couldn't find " <> desc <> ": " <> slug
       (TaxRes (i,_):_) -> i
