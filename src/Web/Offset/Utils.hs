{-# LANGUAGE OverloadedStrings #-}

module Web.Offset.Utils where

import qualified Control.Concurrent.Async as CC
import           Data.Aeson               (FromJSON, ToJSON)
import qualified Data.Aeson               as J
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime,
                                           parseTimeM)

readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show

terror :: Text -> a
terror = error . T.unpack

(=<<<) :: Monad r => (a -> r (Maybe b)) -> r (Maybe a) -> r (Maybe b)
f =<<< g = maybe (return Nothing) f =<< g

decode :: (FromJSON a) => Text -> Maybe a
decode = J.decodeStrict . T.encodeUtf8

encode :: (ToJSON a) => a -> Text
encode = TL.toStrict . TL.decodeUtf8 . J.encode

decodeJson :: FromJSON a => Text -> Maybe a
decodeJson = decode

-- * -- IO Utilities -- * --
performOnJust :: (o -> IO ()) -> Maybe o -> IO ()
performOnJust = maybe (return ())

concurrently :: [IO a] -> IO [a]
concurrently [] = return []
concurrently [a] =
  do res <- a
     return [res]
concurrently (a:as) =
  do (r1, rs) <- CC.concurrently a (concurrently as)
     return (r1:rs)

parseDate :: Text -> Text -> Maybe UTCTime
parseDate format date =
  parseTimeM False
             defaultTimeLocale
             (T.unpack format)
             (T.unpack date) :: Maybe UTCTime

useLogger :: Maybe (Text -> IO ()) -> Text -> IO ()
useLogger logger msg = case logger of
                         Nothing -> return ()
                         Just f -> f msg
