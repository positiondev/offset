{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Splices.Helpers where

import           Data.Aeson       (Object)
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           Web.Larceny

import           Web.Offset.Utils

datePartSubs :: UTCTime -> Substitutions s
datePartSubs date = subs [ ("wpYear",     datePartFill "%0Y" date)
                         , ("wpMonth",    datePartFill "%m"  date)
                         , ("wpDay",      datePartFill "%d"  date)
                         , ("wpFullDate", datePartFill "%D"  date) ]
  where datePartFill defaultFormat utcTime =
          useAttrs (a "format") $ \mf ->
                   let f = fromMaybe defaultFormat mf in
                   textFill $ T.pack $ formatTime defaultTimeLocale (T.unpack f) utcTime
