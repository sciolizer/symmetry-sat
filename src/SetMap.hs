module SetMap (

) where

import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.SetMap as SM

instance (Ord k, Ord v) => Monoid (SM.SetMap k v) where
  mempty = SM.empty
  mappend = unionSetMap

unionSetMap :: (Ord k, Ord v) => SM.SetMap k v -> SM.SetMap k v -> SM.SetMap k v
unionSetMap sm1 = foldl addKey sm1 . M.toList . SM.toMap where
  addKey m (k, setV) = foldl (flip (SM.insert k)) m . S.toList $ setV

instance (Eq k, Eq v) => Eq (SM.SetMap k v) where
  (==) = (==) `on` SM.toMap

instance (Ord k, Ord v) => Ord (SM.SetMap k v) where
  compare = compare `on` SM.toMap

instance (Show k, Show v) => Show (SM.SetMap k v) where
  show = show . SM.toMap