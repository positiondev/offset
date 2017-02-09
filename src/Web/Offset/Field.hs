{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Field where

import           Control.Applicative ((<$>))
import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    (defaultTimeLocale, formatTime, parseTimeM)
import           Web.Larceny

-- TODO(dbp 2014-10-14): date should be parsed and nested.
data Field s = F Text -- A single flat field
             | P Text (Text -> Fill s) -- A customly parsed flat field
             | N Text [Field s] -- A nested object field
             | C Text [Text] -- A nested text field that is found by following the specified path
             | CN Text [Text] [Field s] -- A nested set of fields that is found by follwing the specified path
             | M Text [Field s] -- A list field, where each element is an object

mergeFields :: [Field s] -> [Field s] -> [Field s]
mergeFields fo [] = fo
mergeFields fo (f:fs) = mergeFields (overrideInList False f fo) fs
  where overrideInList :: Bool -> Field s -> [Field s] -> [Field s]
        overrideInList False fl [] = [fl]
        overrideInList True _ [] = []
        overrideInList v fl (m:ms) = if matchesName m fl
                                     then mergeField m fl : overrideInList True fl ms
                                     else m : overrideInList v fl ms
        matchesName a b = getName a == getName b
        getName (F t) = t
        getName (P t _) = t
        getName (N t _) = t
        getName (C t _) = t
        getName (CN t _ _) = t
        getName (M t _) = t
        mergeField (N _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (M _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (N _ left) (M nm right) = M nm (mergeFields left right)
        mergeField (M _ left) (M nm right) = M nm (mergeFields left right)
        mergeField _ right = right

instance Show (Field s) where
  show (F t) = "F(" <> T.unpack t <> ")"
  show (P t _) = "P(" <> T.unpack t <> ",{code})"
  show (N t n) = "N(" <> T.unpack t <> "," <> show n <> ")"
  show (C t p) = "C(" <> T.unpack t <> ":" <> T.unpack (T.intercalate "/" p) <> ")"
  show (CN t p fs) = "C(" <> T.unpack t <> "," <> T.unpack (T.intercalate "/" p) <> ","<> show fs <> ")"
  show (M t m) = "M(" <> T.unpack t <> "," <> show m <> ")"

postFields :: [Field s]
postFields = [F "id"
             ,C "title" ["title", "rendered"]
             ,F "status"
             ,F "type"
             ,F "author"
             ,C "content" ["content", "rendered"]
             ,P "date" wpDateFill
             ,F "slug"
             ,C "excerpt" ["excerpt", "rendered"]
             ,N "custom_fields" [F "test"]
             ,N "featured_media" [F "content"
                                 ,F "source"
                                 ,N "attachment_meta" [F "width"
                                                      ,F "height"
                                                      ,N "sizes" [N "thumbnail" [F "width"
                                                                                ,F "height"
                                                                                ,F "url"]
                                                                 ]]]
             ,N "terms" [M "category" [F "id", F "name", F "slug", F "count"]
                        ,M "post_tag" [F "id", F "name", F "slug", F "count"]]
             ]

wpDateFill :: Text -> Fill s
wpDateFill date =
  let wpFormat = "%Y-%m-%dT%H:%M:%S"
      parsedDate = parseTimeM False
                    defaultTimeLocale
                    (T.unpack wpFormat)
                    (T.unpack date) :: Maybe UTCTime in
  case parsedDate of
    Just d -> let dateSubs = subs [ ("wpYear",     datePartFill "%0Y" d)
                                  , ("wpMonth",    datePartFill "%m"  d)
                                  , ("wpDay",      datePartFill "%d"  d)
                                  , ("wpFullDate", datePartFill "%D"  d) ] in
                fillChildrenWith dateSubs
    Nothing -> textFill $ "<!-- Unable to parse date: " <> date <> " -->"
  where datePartFill defaultFormat date =
          useAttrs (a "format") $ \mf ->
                   let f = fromMaybe defaultFormat mf in
                   textFill $ T.pack $ formatTime defaultTimeLocale (T.unpack f) date
