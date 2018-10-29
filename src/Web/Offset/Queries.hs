{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Offset.Queries where

import           Data.Aeson             (FromJSON(..))
import           Data.Monoid
import           Data.Text              (Text)

import           Web.Offset.Cache
import           Web.Offset.Cache.Types
import           Web.Offset.Types
import           Web.Offset.Utils

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
lookupSpecId Wordpress{..} taxName spec =
  case spec of
   TaxPlus slug -> (fmap . fmap) (\(TaxRes (i, _)) -> TaxPlusId i) (idFor taxName slug)
   TaxMinus slug -> (fmap . fmap) (\(TaxRes (i, _)) -> TaxMinusId i) (idFor taxName slug)
  where
    idFor :: Text -> Text -> IO (Maybe TaxRes)
    idFor _ slug = do
      let key = TaxSlugKey taxName slug
      let cacheSettings = cacheInternals { wpCacheSet = wpCacheSetInt (runRedis cacheInternals)
                                                                      (CacheSeconds (12 * 60 * 60)) }
      resp <- cachingGetErrorInt cacheSettings key
      case fmap decodeWPResponseBody resp of
        Left errCode -> do
          wpLogger $ "Cache lookup returned HTTP error code " <> tshow errCode
          return Nothing
        Right Nothing -> do
          wpLogger $ "Unparseable JSON in lookupSpecId for: " <> tshow spec <>
                     " response: " <> tshow resp
          return Nothing
        Right (Just []) ->  do
          wpLogger $ "No id found in lookupSpecId for: " <> tshow spec
          return Nothing
        Right (Just [taxRes]) -> return $ Just taxRes
        Right (Just (_:_)) ->  do
          wpLogger $ "JSON response in lookupSpecId for: " <> tshow spec
                     <> " contains multiple results: " <> tshow resp
          return Nothing