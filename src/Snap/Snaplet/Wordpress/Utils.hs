module Snap.Snaplet.Wordpress.Utils where

import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show
