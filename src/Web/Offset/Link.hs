{-# LANGUAGE OverloadedStrings #-}

module Web.Offset.Link where

import           Data.Aeson       hiding (decode, encode, json, object)
import           Data.Aeson.Types (parseMaybe)
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Format (defaultTimeLocale, formatTime)

import           Web.Offset.Date

data Link = Link { linkHref  :: Text
                 , linkTitle :: Text } deriving (Eq, Show)

data Permalink =
  Permalink { pYear  :: Text
            , pMonth :: Text
            , pSlug  :: Text
            , pTitle :: Text }

instance FromJSON Permalink where
  parseJSON (Object v) =
    Permalink <$> toPermalinkPart "%0Y" <*>
                  toPermalinkPart "%m" <*>
                   v .: "slug"  <*>
                  (do t <- v .: "title"
                      t .: "rendered")
    where toPermalinkPart formatStr =
            T.pack <$>
              Data.Time.Format.formatTime defaultTimeLocale formatStr <$>
              jsonParseDate <$> (v .: "date")
  parseJSON _ = error "bad post"

permalinkToLink :: Text -> Permalink -> Link
permalinkToLink baseurl (Permalink y m s t) =
  Link (baseurl </> y </> m </> s <> "/") t
  where a </> b = a <> "/" <> b

buildPermalink :: Text -> Object -> Maybe Link
buildPermalink baseurl o =
  let permalink = parseMaybe parseJSON (Object o) :: Maybe Permalink in
    permalinkToLink baseurl <$> permalink
