{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Snap.Snaplet.Wordpress.Posts where

import           Control.Applicative
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.MVar
import           Control.Lens
import           Data.Aeson
import qualified Data.Attoparsec.Text         as A
import           Data.ByteString              (ByteString)
import           Data.Char                    (toUpper)
import qualified Data.Configurator            as C
import           Data.Default
import qualified Data.HashMap.Strict          as M
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.List                    (intercalate)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Map.Syntax
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe,
                                               isJust, listToMaybe)
import           Data.Monoid
import           Data.Ratio
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TL
import           Data.Time.Clock
import qualified Data.Vector                  as V
import           Database.Redis               (Redis)
import qualified Database.Redis               as R
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import qualified Network.Wreq                 as W
import           Snap
import           Snap.Snaplet.Heist           (Heist, addConfig)
import           Snap.Snaplet.RedisDB         (RedisDB)
import qualified Snap.Snaplet.RedisDB         as R
import qualified Text.XmlHtml                 as X

import           Snap.Snaplet.Wordpress.Utils

extractPostId :: Object -> (Int, Object)
extractPostId p = let i = M.lookup "ID" p
                      is = case i of
                            Just (String n) -> readSafe n
                            Just (Number n) ->
                              let rat = toRational n in
                              case denominator rat of
                                1 -> Just $ fromInteger (numerator rat)
                                _ -> Nothing
                            _ -> Nothing
                      id' = fromMaybe 0 is in
                    (id', p)

