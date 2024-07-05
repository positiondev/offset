{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Offset (
   Wordpress(..)
 , WordpressConfig(..)
 , Requester(..)
 , CacheBehavior(..)
 , initWordpress
 , wpGetPost
 , getPost
 , WPKey(..)
 , Filter(..)
 , transformName
 , TaxSpec(..)
 , TagType
 , CatType
 , TaxSpecList(..)
 , Field(..)
 , IdToEndpoint(..)
 , IdsToEndpoint(..)
 , mergeFields
 , attrToTaxSpecList
 , feedSubs
 , toXMLFeed
 , WPFeed(..)
 , FeedFormat(..)
 , WPAuthorStyle(..)
 , Link(..)
 , buildPermalink
 ) where

import           Web.Offset.Cache.Types
import           Web.Offset.Feed
import           Web.Offset.Field
import           Web.Offset.HTTP
import           Web.Offset.Init
import           Web.Offset.Link
import           Web.Offset.Splices
import           Web.Offset.Types
