{-# LANGUAGE OverloadedStrings #-}

module Web.Offset.HTTP where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.CaseInsensitive      as CI
import           Data.Monoid               ((<>))
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wreq              as W

import           Web.Offset.Types

wreqRequester :: (Text -> IO ())
              -> Text
              -> Text
              -> Requester
wreqRequester logger user passw =
  Requester $ \u ps -> do let opts = W.defaults & W.params .~ ps
                                                & W.auth ?~ W.basicAuth user' pass'
                                                & W.checkResponse ?~ (\_ _ -> return ())
                          logger $ "wreq: " <> u <> " with params: " <>
                            (T.intercalate "&" . map (\(a,b) -> a <> "=" <> b) $ ps)
                          r <- W.getWith opts (T.unpack u)
                          case r ^. W.responseStatus ^. W.statusCode of
                            200 -> return $ Right $ toWPResponse r
                            n -> return $ Left n
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 passw

toWPResponse :: W.Response LBS.ByteString -> WPResponse
toWPResponse r =
    WPResponse (r ^. W.responseHeaders)
               (TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody)