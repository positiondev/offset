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
   CMS(..)
 , CMSConfig(..)
 , Requester(..)
 , CacheBehavior(..)
 , initCMS
 , wpGetPost
 , getPost
 , CMSKey(..)
 , Filter(..)
 , transformName
 , TaxSpec(..)
 , TagType
 , CatType
 , TaxSpecList(..)
 , Field(..)
 , mergeFields
 , attrToTaxSpecList
 ) where

import           Web.Offset.Cache.Types
import           Web.Offset.Field
import           Web.Offset.HTTP
import           Web.Offset.Init
import           Web.Offset.Splices
import           Web.Offset.Types
