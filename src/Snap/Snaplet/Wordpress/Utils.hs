module Snap.Snaplet.Wordpress.Utils where

import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

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
