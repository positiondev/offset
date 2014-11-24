module Snap.Snaplet.Wordpress.Cache.Internal where

import           Control.Applicative
import           Control.Monad
import           Data.Text           (Text)
import qualified Data.Text.Encoding  as T
import           Database.Redis      (Redis)
import qualified Database.Redis      as R

rsetex :: Text -> Int -> Text -> Redis Bool
rsetex k n v = isSuccess <$> R.setex (T.encodeUtf8 k) (toInteger n) (T.encodeUtf8 v)

rset :: Text -> Text -> Redis Bool
rset k v = isSuccess <$> R.set (T.encodeUtf8 k) (T.encodeUtf8 v)

isSuccess :: Either a b -> Bool
isSuccess res = case res of
                 Left _err -> False
                 Right _val -> True

rget :: Text -> Redis (Maybe Text)
rget k = (fmap T.decodeUtf8) <$> join <$> eitherToMaybe <$> R.get (T.encodeUtf8 k)
  where eitherToMaybe :: Either a b -> Maybe b
        eitherToMaybe e =
          case e of
           Right a -> Just a
           Left _err -> Nothing


rdel :: Text -> Redis Bool
rdel k = isSuccess <$> R.del [T.encodeUtf8 k]
