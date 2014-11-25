module Snap.Snaplet.Wordpress.Utils where

import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Aeson              as J
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show

(=<<<) :: Monad r => (a -> r (Maybe b)) -> r (Maybe a) -> r (Maybe b)
f =<<< k = z f =<< k
  where z :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
        z f k = case k of
                 Just k' -> f k'
                 Nothing -> return Nothing

decode :: (FromJSON a) => Text -> Maybe a
decode = J.decodeStrict . T.encodeUtf8

encode :: (ToJSON a) => a -> Text
encode = TL.toStrict . TL.decodeUtf8 . J.encode
