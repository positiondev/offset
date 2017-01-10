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

lookupSpecId :: Wordpress b -> TaxonomyName -> TaxSpec -> IO (Maybe TaxSpecId)
lookupSpecId wp@Wordpress{..} taxName spec =
  case spec of
   TaxPlus slug -> (fmap . fmap) (\(TaxRes (i, _)) -> TaxPlusId i) (idFor taxName slug)
   TaxMinus slug -> (fmap . fmap) (\(TaxRes (i, _)) -> TaxMinusId i) (idFor taxName slug)
  where
    idFor :: Text -> Text -> IO (Maybe TaxRes)
    idFor taxName slug = do
      let key = TaxSlugKey taxName slug
      let cacheSettings = cacheInternals { wpCacheSet = wpCacheSetInt (runRedis cacheInternals)
                                                                      (CacheSeconds (12 * 60 * 60)) }
      resp <- cachingGetErrorInt cacheSettings key
      case decodeJson resp of
        Nothing -> do
          wpLogger $ "Unparseable JSON in lookupSpecId for: " <> tshow spec <>
                     " response: " <> resp
          return Nothing
        Just [] ->  do
          wpLogger $ "No id found in lookupSpecId for: " <> tshow spec
          return Nothing
        Just [taxRes] -> return $ Just taxRes
        Just (x:xs) ->  do
          wpLogger $ "JSON response in lookupSpecId for: " <> tshow spec
                     <> " contains multiple results: " <> resp
          return Nothing
