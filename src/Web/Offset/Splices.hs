{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Offset.Splices where

import           Control.Monad.State
import           Control.Applicative     ((<|>))
import           Control.Lens            hiding (children)
import           Control.Concurrent.MVar
import           Data.Aeson              hiding (decode, encode, json, object)
import qualified Data.Attoparsec.Text    as A
import qualified Data.HashMap.Strict     as M
import qualified Data.Map as Map
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as IntSet
import           Data.Maybe              (fromJust, fromMaybe, catMaybes)
import           Data.Monoid
import           Data.Scientific         (floatingOrInteger)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Web.Larceny

import           Web.Offset.Field
import           Web.Offset.Types
import           Web.Offset.Utils
import           Web.Offset.Splices.Helpers

cmsSubs :: CMS b
        -> [Field s]
        -> StateT s IO Text
        -> CMSLens b s
        -> Substitutions s
cmsSubs wp extraFields getURI cmsLens =
  subs [ ("cmsCustom", cmsCustomFill wp)
       , ("cmsCustomDate", cmsCustomDateFill)]

cmsCustomDateFill :: Fill s
cmsCustomDateFill =
  useAttrs (a "format" % a "date") customDateFill
  where customDateFill mFormat date =
          let format = fromMaybe "%Y-%m-%d %H:%M:%S" mFormat in
          case parseDate format date of
              Just d -> fillChildrenWith $ datePartSubs DefaultPrefix d
              Nothing -> textFill $ "<!-- Unable to parse date: " <> date <> " -->"

cmsCustomFill :: CMS b -> Fill s
cmsCustomFill CMS{..} =
  useAttrs (a "endpoint") customFill
  where customFill endpoint = Fill $ \attrs (path, tpl) lib ->
          do let key = CMSKey ("/" <> endpoint, [])
                         ("endpoint:" <> endpoint)
                         ("EndpointKey " <> endpoint)
             res <- liftIO $ cachingGetRetry key
             case fmap decode res of
               Left code -> do
                 let notification = "Encountered status code " <> tshow code
                                   <> " when querying \"" <> endpoint <> "\"."
                 liftIO $ cmsLogger notification
                 return $ "<!-- " <> notification <> " -->"
               Right (Just (json :: Value)) ->
                 unFill (jsonToFill json) attrs (path, tpl) lib
               Right Nothing -> do
                 let notification = "Unable to decode JSON for endpoint \"" <> endpoint
                 liftIO $ cmsLogger $ notification <> ": " <> tshow res
                 return $ "<!-- " <> notification <> "-->"

jsonToFill :: Value -> Fill s
jsonToFill (Object o) =
  Fill $ \_ (path, tpl) lib -> runTemplate tpl path objectSubstitutions lib
  where objectSubstitutions =
          subs $ map (\k -> (transformName DefaultPrefix k,
                             jsonToFill (fromJust (M.lookup k o))))
                     (M.keys o)
jsonToFill (Array v) =
  Fill $ \attrs (path, tpl) lib ->
           V.foldr mappend "" <$> V.mapM (\e -> unFill (jsonToFill e) attrs (path, tpl) lib) v
jsonToFill (String s) = textFill s
jsonToFill (Number n) = case floatingOrInteger n of
                          Left r -> textFill $ tshow (r :: Double)
                          Right i -> textFill $ tshow (i :: Integer)
jsonToFill (Bool b) = textFill $ tshow b
jsonToFill (Null) = textFill "<!-- JSON field found, but value is null. -->"

fieldSubs :: BlankPrefix -> [Field s] -> Object -> Substitutions s
fieldSubs prefix fields object = subs (map (buildSplice object) fields)
  where buildSplice o (F n) =
          (transformNameP n, textFill $ getText n o)
        buildSplice o (P n fill') =
          (transformNameP n, fill' $ getText n o)
        buildSplice o (PN n fill') =
          (transformNameP n, fill' (unObj . M.lookup n $ o))
        buildSplice o (PM n fill') =
          (transformNameP n, fill' (unArray . M.lookup n $ o))
        buildSplice o (N n fs) =
          (transformNameP n, fillChildrenWith $ subs
                            (map (buildSplice (unObj . M.lookup n $ o)) fs))
        buildSplice o (C n path) =
          (transformNameP n, textFill (getText (last path) . traverseObject (init path) $ o))
        buildSplice o (CN n path fs) =
          (transformNameP n, fillChildrenWith $ subs
                            (map (buildSplice (traverseObject path o)) fs))
        buildSplice o (M n fs) =
          (transformNameP n,
            mapSubs (\oinner -> subs $ map (buildSplice oinner) fs)
                    (unArray . M.lookup n $ o))

        unObj (Just (Object o)) = o
        unObj _ = M.empty
        unArray (Just (Array v)) = map (unObj . Just) $ V.toList v
        unArray _ = []
        traverseObject pth o = foldl (\o' x -> unObj . M.lookup x $ o') o pth
        getText n o = case M.lookup n o of
                        Just (String t) -> t
                        Just (Number i) -> either (tshow :: Double -> Text)
                                                  (tshow :: Integer -> Text) (floatingOrInteger i)
                        _ -> ""
        transformNameP = transformName prefix

-- * -- Internal -- * --

cmsGetSingle :: (MonadState s m, MonadIO m) => CMSLens b s -> CMSKey -> m (Maybe Object)
cmsGetSingle cmsLens wpKey =
  do wp <- use cmsLens
     liftIO $ getSingle wp wpKey

getSingle :: CMS b -> CMSKey -> IO (Maybe Object)
getSingle CMS{..} wpKey = decodeObj <$> cachingGetRetry wpKey
  where decodeObj :: Either StatusCode Text -> Maybe Object
        decodeObj (Right t) =
          do obj' <- decodeJson t
             case obj' of
              Just (obj:_) -> Just obj
              _ -> Nothing
        decodeObj (Left _) = Nothing

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
