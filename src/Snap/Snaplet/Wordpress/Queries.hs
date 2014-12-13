{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Wordpress.Queries where

import           Data.Monoid
import           Data.Text                          (Text)
import           Snap                               hiding (path, rqURI)

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
