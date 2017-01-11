module Web.Offset.Cache.Redis where

import           Control.Applicative ((<$>))
import           Control.Monad       (join)
import           Data.Either         (isRight)
import           Data.Text           (Text)
import qualified Data.Text.Encoding  as T
import           Database.Redis      (Redis)
import qualified Database.Redis      as R

rsetex :: Text -> Int -> Text -> Redis Bool
rsetex k n v = isRight <$> R.setex (T.encodeUtf8 k) (toInteger n) (T.encodeUtf8 v)

rset :: Text -> Text -> Redis Bool
rset k v = isRight <$> R.set (T.encodeUtf8 k) (T.encodeUtf8 v)

rget :: Text -> Redis (Maybe Text)
rget k = fmap T.decodeUtf8 <$> join <$> eitherToMaybe <$> R.get (T.encodeUtf8 k)
  where eitherToMaybe :: Either a b -> Maybe b
        eitherToMaybe e =
          case e of
           Right a -> Just a
           Left _err -> Nothing

rdel :: [Text] -> Redis Bool
rdel k = isRight <$> R.del (map T.encodeUtf8 k)

rkeys :: Text -> Redis [Text]
rkeys k =
  do e <- R.keys $ T.encodeUtf8 k
     case e of
      Right a -> return (map T.decodeUtf8 a)
      Left _err -> return []

rdelstar :: Text -> Redis Bool
rdelstar k = rdel =<< rkeys k
