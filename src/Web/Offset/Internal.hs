{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Offset.Internal where

import           Data.Monoid      ((<>))
import qualified Data.Set         as Set
import           Data.Text        (Text)

import           Web.Offset.HTTP
import           Web.Offset.Types
import           Web.Offset.Utils

cmsRequestInt :: Requester -> Text -> CMSKey -> IO (Either StatusCode Text)
cmsRequestInt runHTTP endpt key = req (cRequestUrl key)
  where req (path, params) = unRequester runHTTP (endpt <> path) params

cmsLogInt :: Maybe (Text -> IO ()) -> Text -> IO ()
cmsLogInt logger msg = case logger of
                         Nothing -> return ()
                         Just f -> f msg
