{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Offset.Queries where

import           Data.Monoid
import           Data.Text              (Text)

import           Web.Offset.Cache
import           Web.Offset.Cache.Types
import           Web.Offset.Types
import           Web.Offset.Utils

lookupTaxDict :: WPKey -> Wordpress b -> IO (TaxonomyName, TaxSpec -> TaxSpecId)
lookupTaxDict key@(TaxDictKey resName) wp@Wordpress{..} =
  do resp <- cachingGetErrorInt (cacheInternals { wpCacheSet = wpCacheSetInt (runRedis cacheInternals)
                                (CacheSeconds (12 * 60 * 60))}) key
     case decodeJson resp of
       Nothing -> do wpExpirePostInt (runRedis cacheInternals) key
                     terror $ "Unparsable JSON: " <> resp
       Just res -> return (resName, getSpecId $ TaxDict res resName)

lookupTaxSlug :: WPKey -> Wordpress b -> IO (Int -> Text)
lookupTaxSlug key@(TaxDictKey resName) wp@Wordpress{..} =
  do resp <- cachingGetErrorInt (cacheInternals { wpCacheSet = wpCacheSetInt (runRedis cacheInternals) (CacheSeconds (12 * 60 * 60))}) key
     case decodeJson resp of
       Nothing -> do wpExpirePostInt (runRedis cacheInternals) key
                     terror $ "Unparsable JSON: " <> resp
       Just res -> return (getTaxSlug $ TaxDict res resName)

getTaxonomies :: [TaxDict] -> TaxSpec -> TaxSpecId
getTaxonomies taxDicts spec = undefined

getSpecId :: TaxDict -> TaxSpec -> TaxSpecId
getSpecId taxDict spec =
  case spec of
   TaxPlus slug -> TaxPlusId $ idFor taxDict slug
   TaxMinus slug -> TaxMinusId $ idFor taxDict slug
  where
    idFor :: TaxDict -> Text -> Int
    idFor (TaxDict{..}) slug =
      case filter (\(TaxRes (_,s)) -> s == slug) dict of
       [] -> terror $ "Couldn't find " <> desc <> ": " <> slug
       (TaxRes (i,_):_) -> i

getTaxSlug :: TaxDict -> Int -> Text
getTaxSlug (TaxDict{..}) taxId =
  case filter (\(TaxRes (i, _)) -> i == taxId) dict of
    [] -> terror $ "Couldn't find " <> desc <> " with id: " <> tshow taxId
    (TaxRes (_,s):_) -> s
