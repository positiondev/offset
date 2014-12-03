{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.Wordpress.HTTP where

import           Control.Lens
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.Wreq            as W

newtype Requester = Requester { unRequester :: Text -> [(Text, Text)] -> IO Text }

wreqRequester :: Maybe (Text -> IO ())
              -> Text
              -> Text
              -> Requester
wreqRequester logger user passw =
  Requester $ \u ps -> do let opts = (W.defaults & W.params .~ ps
                                      & W.auth .~ W.basicAuth user' pass')
                          wplog logger $ "wreq: " <> u <> " with params: " <>
                            (T.intercalate "&" . map (\(a,b) -> a <> "=" <> b) $ ps)
                          r <- W.getWith opts (T.unpack u)
                          return $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 passw

wplog :: Maybe (Text -> IO ()) -> Text -> IO ()
wplog logger msg = case logger of
                    Nothing -> return ()
                    Just f -> f msg
