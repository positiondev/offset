{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Offset.Posts where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Maybe          (fromMaybe)
import           Data.Ratio

import           Web.Offset.Utils

extractPostId :: Object -> (Int, Object)
extractPostId p = let i = M.lookup "id" p
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

extractPostIds :: [Object] -> [(Int, Object)]
extractPostIds = map extractPostId
