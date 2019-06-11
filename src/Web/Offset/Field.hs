{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Field where

import           Data.Aeson       (Object)
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Web.Larceny

import           Web.Offset.Date

data Field s = F Text -- A single flat text or number field
             | B Text -- A single flat boolean field
             | Q Text IdToEndpoint -- A field that will make another request
             | QM Text IdsToEndpoint -- A list of fields that will make another request
             | P Text (Text -> Fill s) -- A customly parsed flat field
             | PN Text (Object -> Fill s) -- A customly parsed nested field
             | PM Text ([Object] -> Fill s) -- A customly parsed list field
             | N Text [Field s] -- A nested object field
             | C Text [Text] -- A nested text field that is found by following the specified path
             | CB Text [Text]  -- A nested bool field that is found by following the specified path
             | CN Text [Text] [Field s] -- A nested set of fields that is found by following the specified path
             | M Text [Field s] -- A list field, where each element is an object

data IdToEndpoint = UseId Text | UseSlug Text deriving (Eq, Show)
data IdsToEndpoint =  UseInclude Text deriving (Eq, Show)

-- NOTE(dbp 2014-11-07): We define equality that is 'good enough' for testing.
-- In truth, our definition is wrong because of the functions inside of 'P' variants.
-- NOTE(emh 2017-02-09): Moving this to Field to avoid orphan instance (even though we
-- want this to be orphaned!). This eq instance is only for testing!!
instance Eq (Field s) where
  F t1 == F t2 = t1 == t2
  B t1 == B t2 = t1 == t2
  Q t1 tE1 == Q t2 tE2 = t1 == t2 && tE1 == tE2
  QM t1 tE1 == QM t2 tE2 = t1 == t2 && tE1 == tE2
  P t1 _ == P t2 _ = t1 == t2
  PN t1 _ == PN t2 _ = t1 == t2
  PM t1 _ == PM t2 _ = t1 == t2
  N t1 n1 == N t2 n2 = t1 == t2 && n1 == n2
  M t1 m1 == M t2 m2 = t1 == t2 && m1 == m2
  _ == _ = True -- wait


mergeFields :: [Field s] -> [Field s] -> [Field s]
mergeFields fo [] = fo
mergeFields fo (f:fs) = mergeFields (overrideInList False f fo) fs
  where overrideInList :: Bool -> Field s -> [Field s] -> [Field s]
        overrideInList False fl [] = [fl]
        overrideInList True _ [] = []
        overrideInList v fl (m:ms) = if matchesName m fl
                                     then mergeField m fl : overrideInList True fl ms
                                     else m : overrideInList v fl ms
        matchesName c d = getName c == getName d
        getName (F t) = t
        getName (B t) = t
        getName (Q t _) = t
        getName (QM t _) = t
        getName (P t _) = t
        getName (PN t _) = t
        getName (PM t _) = t
        getName (N t _) = t
        getName (C t _) = t
        getName (CB t _) = t
        getName (CN t _ _) = t
        getName (M t _) = t
        mergeField (N _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (M _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (N _ left) (M nm right) = M nm (mergeFields left right)
        mergeField (M _ left) (M nm right) = M nm (mergeFields left right)
        mergeField _ right = right

instance Show (Field s) where
  show (F t) = "F(" <> T.unpack t <> ")"
  show (B t) = "B(" <> T.unpack t <> ")"
  show (Q t e) = "Q("<> T.unpack t <> ":" <> show e <>")"
  show (QM t e) = "Q("<> T.unpack t <> ":" <> show e <>")"
  show (P t _) = "P(" <> T.unpack t <> ",{code})"
  show (PN t _) = "PN(" <> T.unpack t <> ",{code})"
  show (PM t _) = "PM(" <> T.unpack t <> ",{code})"
  show (N t n) = "N(" <> T.unpack t <> "," <> show n <> ")"
  show (C t p) = "C(" <> T.unpack t <> ":" <> T.unpack (T.intercalate "/" p) <> ")"
  show (CB t p) = "C(" <> T.unpack t <> ":" <> T.unpack (T.intercalate "/" p) <> ")"
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

datePartSubs :: UTCTime -> Substitutions s
datePartSubs date = subs [ ("wpYear",     datePartFill "%0Y" date)
                         , ("wpMonth",    datePartFill "%m"  date)
                         , ("wpDay",      datePartFill "%d"  date)
                         , ("wpFullDate", datePartFill "%D"  date) ]
  where datePartFill defaultFormat utcTime =
          useAttrs (a "format") $ \mf ->
                   let f = fromMaybe defaultFormat mf in
                   rawTextFill $ T.pack $ formatTime defaultTimeLocale (T.unpack f) utcTime

wpDateFill :: Text -> Fill s
wpDateFill date =
  let wpFormat = "%Y-%m-%dT%H:%M:%S" in
  case parseWPDate wpFormat date of
    Just d -> fillChildrenWith $ datePartSubs d
    Nothing -> textFill $ "<!-- Unable to parse date: " <> date <> " -->"
