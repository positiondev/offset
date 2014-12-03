module Snap.Snaplet.Wordpress.Field where

import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Heist
import           Heist.Compiled

-- TODO(dbp 2014-10-14): date should be parsed and nested.
data Field m = F Text -- A single flat field
             | P Text (RuntimeSplice m Text -> Splice m) -- A customly parsed flat field
             | N Text [Field m] -- A nested object field
             | M Text [Field m] -- A list field, where each element is an object

mergeFields :: (Functor m, Monad m) => [Field m] -> [Field m] -> [Field m]
mergeFields fo [] = fo
mergeFields fo (f:fs) = mergeFields (overrideInList False f fo) fs
  where overrideInList :: (Functor m, Monad m) => Bool -> Field m -> [Field m] -> [Field m]
        overrideInList False fl [] = [fl]
        overrideInList True _ [] = []
        overrideInList v fl (m:ms) = (if matchesName m fl
                                        then mergeField m fl : (overrideInList True fl ms)
                                        else m : (overrideInList v fl ms))
        matchesName a b = getName a == getName b
        getName (F t) = t
        getName (P t _) = t
        getName (N t _) = t
        getName (M t _) = t
        mergeField (N _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (M _ left) (N nm right) = N nm (mergeFields left right)
        mergeField (N _ left) (M nm right) = M nm (mergeFields left right)
        mergeField (M _ left) (M nm right) = M nm (mergeFields left right)
        mergeField _ right = right

instance (Functor m, Monad m) =>  Show (Field m) where
  show (F t) = "F(" <> T.unpack t <> ")"
  show (P t _) = "P(" <> T.unpack t <> ",{code})"
  show (N t n) = "N(" <> T.unpack t <> "," <> show n <> ")"
  show (M t m) = "M(" <> T.unpack t <> "," <> show m <> ")"
