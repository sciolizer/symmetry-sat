module Hamiltonian.AppendOnlyBiMap (
  ABMap,
  fromList,
  toList,
  insert,
  lookupLeft,
  lookupRight
) where

import qualified Data.Map.Strict as M

data ABMap a b = ABMap {
  left :: M.Map a b,
  right :: M.Map b a
}
  deriving (Eq, Ord, Read, Show)

empty :: ABMap a b
empty = ABMap M.empty M.empty

fromList :: (Ord a, Ord b) => [(a, b)] -> ABMap a b
fromList = foldl (\mp (a, b) -> insert a b mp) empty

toList :: ABMap a b -> [(a, b)]
toList = M.toList . left

insert :: (Ord a, Ord b) => a -> b -> ABMap a b -> ABMap a b
insert a b (ABMap l r) =
  case (M.lookup a l, M.lookup b r) of
    (Nothing, Nothing) -> ABMap (M.insert a b l) (M.insert b a r)
    _ -> error $ "non-unique insertion"

lookupLeft :: (Ord a) => a -> ABMap a b -> Maybe b
lookupLeft a mp = M.lookup a (left mp)

lookupRight :: (Ord b) => b -> ABMap a b -> Maybe a
lookupRight b mp = M.lookup b (right mp)