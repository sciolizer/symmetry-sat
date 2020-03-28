module IdBiMap (
  IdBiMap,
  idFor,
  forget,
  empty
) where

import qualified Data.Map.Strict as M

data IdBiMap a = IdBiMap (M.Map a Int) (M.Map Int a) Int
  deriving (Show)

empty :: IdBiMap a
empty = IdBiMap M.empty M.empty 1

idFor :: (Ord a) => a -> IdBiMap a -> (IdBiMap a, Int)
idFor k ibm@(IdBiMap mpF mpB nextId) =
  case M.lookup k mpF of
    Nothing -> (IdBiMap (M.insert k nextId mpF) (M.insert nextId k mpB) nextId, nextId + 1)
    Just c -> (ibm, c)

forget :: Ord a => Int -> IdBiMap a -> IdBiMap a
forget v (IdBiMap mpF mpB nextId) =
  case M.lookup v mpB of
    Nothing -> error "nothing to forget"
    Just k -> IdBiMap (M.delete k mpF) (M.delete v mpB) nextId