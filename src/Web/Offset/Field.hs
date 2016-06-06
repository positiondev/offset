{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Field where

import           Control.Applicative ((<$>))
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Heist
import           Heist.Interpreted

-- TODO(dbp 2014-10-14): date should be parsed and nested.
data Field m = F Text -- A single flat field
             | P Text (Text -> Splice m) -- A customly parsed flat field
             | N Text [Field m] -- A nested object field
             | C Text [Text] -- A nested text field that is found by following the specified path
             | CN Text [Text] [Field m] -- A nested set of fields that is found by follwing the specified path
             | M Text [Field m] -- A list field, where each element is an object

mergeFields :: (Functor m, Monad m) => [Field m] -> [Field m] -> [Field m]
mergeFields fo [] = fo
mergeFields fo (f:fs) = mergeFields (overrideInList False f fo) fs
  where overrideInList :: (Functor m, Monad m) => Bool -> Field m -> [Field m] -> [Field m]
        overrideInList False fl [] = [fl]
        overrideInList True _ [] = []
        overrideInList v fl (m:ms) = (if matchesName m fl
                                        then mergeField m fl : (overrideInList True fl ms)
                                        else m : (overrideInList v fl ms))
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

instance (Functor m, Monad m) =>  Show (Field m) where
  show (F t) = "F(" <> T.unpack t <> ")"
  show (P t _) = "P(" <> T.unpack t <> ",{code})"
  show (N t n) = "N(" <> T.unpack t <> "," <> show n <> ")"
  show (C t p) = "C(" <> T.unpack t <> ":" <> T.unpack (T.intercalate "/" p) <> ")"
  show (CN t p fs) = "C(" <> T.unpack t <> "," <> T.unpack (T.intercalate "/" p) <> ","<> show fs <> ")"
  show (M t m) = "M(" <> T.unpack t <> "," <> show m <> ")"

postFields :: (Functor m, Monad m) => [Field m]
postFields = [F "ID"
             ,F "title"
             ,F "status"
             ,F "type"
             ,N "author" [F "ID",F "name",F "first_name",F "last_name",F "description", F "username"]
             ,F "content"
             ,P "date" dateSplice
             ,F "slug"
             ,F "excerpt"
             ,N "custom_fields" [F "test"]
             ,N "featured_image" [F "content"
                                 ,F "source"
                                 ,N "attachment_meta" [F "width"
                                                      ,F "height"
                                                      ,N "sizes" [N "thumbnail" [F "width"
                                                                                ,F "height"
                                                                                ,F "url"]
                                                                 ]]]
             -- ,CN "thumbnail"
             --     ["featured_image", "attachment_meta", "sizes", "thumbnail"]
             --     [F "width", F "height", F "url"]
             ,N "terms" [M "category" [F "ID", F "name", F "slug", F "count"]
                        ,M "post_tag" [F "ID", F "name", F "slug", F "count"]]
             ]

dateSplice :: (Functor m, Monad m) => Text -> Splice m
dateSplice date = runChildrenWith (let (y,m,d) = parseDate date in
                                   do "wpYear" ## textSplice y
                                      "wpMonth" ## textSplice m
                                      "wpDay" ## textSplice d)
  where parseDate :: Text -> (Text,Text,Text)
        parseDate = tuplify . T.splitOn "-" . T.takeWhile (/= 'T')
        tuplify (y:m:d:_) = (y,m,d)