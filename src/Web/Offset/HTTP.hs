{-# LANGUAGE OverloadedStrings #-}

module Web.Offset.HTTP where

import           Control.Lens
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.Wreq            as W

newtype Requester = Requester { unRequester :: Text
                                            -> [(Text, Text)]
                                            -> IO (Either Int Text) }

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
                            200 -> return $ Right $ TL.toStrict . TL.decodeUtf8 $ r ^. W.responseBody
                            n -> return $ Left n
  where user' = T.encodeUtf8 user
        pass' = T.encodeUtf8 passw
