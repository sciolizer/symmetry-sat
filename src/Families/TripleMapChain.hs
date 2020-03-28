{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}

module Families.TripleMapChain (
  TripleMapChain
) where

import Control.Lens
import qualified Data.Map as M

type Counts b c = M.Map b (Int, c)

data TripleMapChain a b c = TripleMapChain {
  left :: M.Map a b,
  right :: Counts b c }
  deriving (Show)

-- most of these uses are probably wrong.
-- you almost never want to both insert and get the original
-- either you want to get the original, or you want to insert,
-- but not both
--getOriginal :: (Ord (Index (Vars a))) => Agn a -> Counts a -> a
--getOriginal elem set = answer where
--  answer =
--    case M.lookup elem set of
--      Nothing -> error "corrupt DedupingMap"
--      Just (_, orig) -> orig
--
--aoInsertAndIncrement :: (Ord (Index (Vars a))) => Agn a -> a -> Counts a -> Counts a
--aoInsertAndIncrement elem ap set =
----  let ac = AltComparator elem in
--  case M.lookup elem set of
--    Nothing -> M.insert elem (1, ap) set
--    Just (!n, _) -> update elem (n + 1, ap) set
--
--aoDecrement :: (Ord (Index (Vars a))) => Agn a -> Counts a -> Counts a
--aoDecrement elem set =
----  let ac = AltComparator elem in
--  case M.lookup elem set of
--    Nothing -> error "decrement failed because DedupignMap is corrupt"
--    Just (n, ap)
--      | n < 1 -> error "somehow non positive number ended up in DedupingMap"
--      | n == 1 -> M.delete elem set
--      | otherwise -> update elem (n - 1, ap) set
--
---- todo: is there any point to this and not just using aoInsertandincrement and aoDecrement seperately?
--aoSwap :: (Problem a, Ord (Index (Vars a)), Eq (Vars a), Eq (Clzs a)) => Agn a -> Agn a -> (a -> a) -> Counts a -> Counts a
--aoSwap oldAgn newAgn _ _
--  | oldAgn == newAgn = error "aoSwap called with identical assignments"
--aoSwap oldAgn newAgn changeAP counts = if consistent then counts' else error "aoSwap provided invalid keys" where
--  (oldN, oldAP) = case M.lookup oldAgn counts of { Nothing -> error "oldAgn not present"; Just z -> z }
----  (newN, newAP) =
--  expectedOldAgn = assignmentToMap (oldAP ^. assignment)
--  newAP = changeAP oldAP
--  expectedNewAgn = assignmentToMap (newAP ^. assignment)
--  consistent = oldAgn == expectedOldAgn && newAgn == expectedNewAgn
--  counts' = decrement . increment $ counts
--  decrement =
--    case M.lookup oldAgn counts of
--      Nothing -> error "aoSwap given old which was not present in map"
--      Just (n, v)
--        | n < 1 -> error "deduping map corrupt with non-positive number"
--        | n == 1 -> M.delete oldAgn
--        | otherwise -> update oldAgn (n - 1, v)
--  increment =
--    case M.lookup newAgn counts of
--      Nothing -> M.insert newAgn (1, newAP)
--      Just (!n, currentAP) -> update newAgn (n + 1, if eqProblem newAP currentAP then newAP else error "increment put in absProblem which differed from existing value")
----    case M.lookup oldAgn
--
