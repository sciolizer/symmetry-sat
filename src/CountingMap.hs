{-# LANGUAGE BangPatterns #-}

module CountingMap (
  CountingMap,
  lookup,
  insert,
  empty
) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as M
import qualified Data.MultiSet as MS

data CountingMap k v = CountingMap (M.Map k v) (MS.MultiSet v)
  deriving (Show)

empty :: CountingMap k v
empty = CountingMap M.empty MS.empty

lookup :: (Ord k) => k -> CountingMap k v -> Maybe v
lookup k (CountingMap mp _) = M.lookup k mp

insert :: (Ord k, Ord v) => k -> v -> CountingMap k v -> (CountingMap k v, Maybe v) -- Just means the last copy of a value was removed from the map
insert k v cm@(CountingMap mp counts) =
  case M.lookup k mp of
    Nothing -> (CountingMap (M.insert k v mp) (MS.insert v counts), Nothing)
    Just oldValue
      | v == oldValue -> (cm, Nothing)
      | otherwise -> (CountingMap (M.insert k v mp) (MS.insert v (MS.delete oldValue counts)), if MS.occur oldValue counts == 1 then Just oldValue else Nothing)
