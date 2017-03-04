{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Splices.Helpers where

import           Control.Arrow    (first)
import           Data.Aeson       (Object)
import           Data.Char        (toUpper)
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           Web.Larceny

import           Web.Offset.Types
import           Web.Offset.Utils

datePartSubs :: BlankPrefix -> UTCTime -> Substitutions s
datePartSubs prefix date =
  subs $ addPrefixes [ ("year",     datePartFill "%0Y" date)
                     , ("month",    datePartFill "%m"  date)
                     , ("day",      datePartFill "%d"  date)
                     , ("fullDate", datePartFill "%D"  date) ]
  where addPrefixes = map (first $ transformName prefix)
        datePartFill defaultFormat utcTime =
          useAttrs (a "format") $ \mf ->
                   let f = fromMaybe defaultFormat mf in
                   textFill $ T.pack $ formatTime defaultTimeLocale (T.unpack f) utcTime

transformName :: BlankPrefix -> Text -> Text
transformName prefix = T.append (toPrefix prefix)
                       . snd . T.foldl f (True, "")
  where f (True, rest) next = (False, T.snoc rest (toUpper next))
        f (False, rest) '_' = (True, rest)
        f (False, rest) '-' = (True, rest)
        f (False, rest) next = (False, T.snoc rest next)
