{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Wordpress.Internal where

import           Data.Monoid                  ((<>))
import qualified Data.Set                     as Set
import           Data.Text                    (Text)

import           Snap.Snaplet.Wordpress.HTTP
import           Snap.Snaplet.Wordpress.Types
import           Snap.Snaplet.Wordpress.Utils

wpRequestInt :: Requester -> Text -> WPKey -> IO Text
wpRequestInt runHTTP endpt key =
  case key of
   TaxDictKey resName -> req ("/terms/" <> resName) []
   PostByPermalinkKey year month slug ->
     req "/posts" [("filter[year]", year)
                  ,("filter[monthnum]", month)
                  ,("filter[name]", slug)]
   PostsKey{} -> req "/posts" (buildParams key)
   PostKey i -> req ("/posts/" <> tshow i) []
   AuthorKey i -> req ("/users/" <> tshow i) []
  where req path params = (unRequester runHTTP) (endpt <> path) params

buildParams :: WPKey -> [(Text, Text)]
buildParams (PostsKey filters) = params
  where params = Set.toList $ Set.map mkFilter filters
        mkFilter (TagFilter (TaxPlusId i)) = ("filter[tag__in]", tshow i)
        mkFilter (TagFilter (TaxMinusId i)) = ("filter[tag__not_in]", tshow i)
        mkFilter (CatFilter (TaxPlusId i)) = ("filter[category__in]", tshow i)
        mkFilter (CatFilter (TaxMinusId i)) = ("filter[category__not_in]", tshow i)
        mkFilter (NumFilter num) = ("filter[posts_per_page]", tshow num)
        mkFilter (OffsetFilter offset) = ("filter[offset]", tshow offset)

wpLogInt :: Maybe (Text -> IO ()) -> Text -> IO ()
wpLogInt logger msg = case logger of
                    Nothing -> return ()
                    Just f -> f msg
