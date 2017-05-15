{-# LANGUAGE OverloadedStrings #-}

module Web.Offset.Date where

import           Data.Aeson       (Value (..))
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

jsonParseDate :: Value -> UTCTime
jsonParseDate (String t) =
  fromMaybe (error $ "Unable to parse date: " ++ show t)
    $ parseWPDate "%Y-%m-%dT%H:%M:%S" t
jsonParseDate _ = error "Unable to parse date."

parseWPDate :: Text -> Text -> Maybe UTCTime
parseWPDate wpFormat date =
  parseTimeM False
             defaultTimeLocale
             (T.unpack wpFormat)
             (T.unpack date) :: Maybe UTCTime

iso8601Format :: UTCTime -> String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%d"
