{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.WordPress.Field where

import           Data.Aeson                 (Object)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             parseTimeM)
import           Web.Larceny

import           Web.Offset.Field
import           Web.Offset.Splices.Helpers
import           Web.Offset.Types
import           Web.Offset.Utils

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
  let wpFormat = "%Y-%m-%dT%H:%M:%S" in
  case parseDate wpFormat date of
    Just d -> fillChildrenWith $ datePartSubs (Prefix "wp") d
    Nothing -> textFill $ "<!-- Unable to parse date: " <> date <> " -->"
