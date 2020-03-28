{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Problem (
  Problem(..),
  Solution(..),
--  Vars,
--  Clzs,
--  reduced,
--  defaultFalsyClauses,
--  defaultUnitClauses
) where

import Debug.Trace

import Control.Lens
import Control.Lens.At
import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Set as S
--import Families.DedupingMap

import Families.Assignment
import Families.Clauses

import SimpleProblem

{-

data family Vars problem
data family Clzs problem

class (
  Assignment (Vars problem),
  Clauses (Clzs problem),
  Index (Index (Clzs problem)) ~ Index (Vars problem),
  AsEmpty problem,
  Show problem)
  => Problem problem where

  clzs :: Iso' problem (Clzs problem)
  assignment :: Iso' problem (Vars problem)

  unassigned :: Fold problem (Index (Vars problem))
  unassigned = folding unassigned'

  falsyClauses :: Fold problem (Index (Clzs problem))
--  falsyClauses = undefined -- defaultFalsyClauses

  unitClauses :: Fold problem (Index (Clzs problem), Index (Vars problem), Bool)
--  unitClauses = defaultUnitClauses

  allClauses :: Fold problem (Index (Clzs problem))
  allClauses = clzs . clauses

-}

-- first is the original clause, second is the clause after reduction
-- folding :: Foldable f => (s -> f a) -> Fold s a
{-
clausesWithReductions :: (Problem problem, At (Vars problem), IxValue (Vars problem) ~ Bool) => Fold problem (Index (Clzs problem), Maybe (Index (Clzs problem)))
clausesWithReductions = folding cwr

cwr :: (Problem problem, At (Vars problem), IxValue (Vars problem) ~ Bool) => problem -> [(Index (Clzs problem), Maybe (Index (Clzs problem)))]
cwr problem = map pairWithReduction cls where
  agn = problem ^. assignment
  cls = problem ^.. allClauses
  pairWithReduction c = (c, reduceClause agn c)

defaultFalsyClauses :: (Problem problem, At (Vars problem), IxValue (Vars problem) ~ Bool) => Fold problem (Index (Clzs problem))
defaultFalsyClauses = sizeFold 0

sizeFold :: (Problem problem, At (Vars problem), IxValue (Vars problem) ~ Bool) => Int -> Fold problem (Index (Clzs problem))
sizeFold size = clausesWithReductions . filtered ifls . _1 where
  ifls (_, Nothing) = False
  ifls (_, Just c) = lengthOf literals c == size

defaultUnitClauses :: (Problem problem, At (Vars problem), IxValue (Vars problem) ~ Bool) => Fold problem (Index (Clzs problem), Index (Vars problem), Bool)
defaultUnitClauses = clausesWithReductions . myConcatMapOf getLit where
  getLit (_, Nothing) = []
  getLit (orig, Just c) =
    case toListOf (literals . withIndex) c of
      [(v, b)] -> [(orig, v, b)]
      _ -> []

myConcatMapOf :: (a -> [b]) -> Fold a b
myConcatMapOf f = folding f

--falsyClauses' :: (Problem problem) => problem -> [Index (Clzs problem)]
--falsyClauses' problem = (problem ^. clzs) ^.. folded . (filtered (isFalsy (problem ^. assignment)))
--
--isFalsy :: (Clause clause, Assignment mp, Index clause ~ Index mp) => mp -> clause -> Bool
isFalsy agn c = reducedSize agn c == Just 0

--reducedSize :: (Clause clause) => M.Map (Index clause) Bool -> clause -> Maybe Int
--reducedSize agn c = lengthOf literals <$> (reduceClause agn c)
--
--unitClauses' :: (Problem problem) => problem -> [(Index (Clzs problem), Index (Index (Clzs problem)), Bool)]
--unitClauses' problem = (problem ^. clzs) ^.. folded . (filtered (\c -> reducedSize (problem ^. assignment) c == Just 1)) . to propagation where
--  propagation c =
--    case reduceClause (problem ^. assignment) c of
--      Nothing -> error "reducedSize and reduceClause are in disagreement"
--      Just c' ->
--        case toListOf (literals . withIndex) c' of
--          [(v,b)] -> (c, v, b)
--          _ -> error "reducedSize returning non units"
--

unassigned' :: (Problem problem) => problem -> S.Set (Index (Vars problem))
unassigned' problem = trace ("clauseVars: " ++ show clauseVars) clauseVars `S.difference` {- trace ("assignedVars: " ++ show assignedVars) -} assignedVars where
  clauseVars = S.fromList . toListOf (clzs . varsInClauses) $ problem
  assignedVars = M.keysSet (assignmentToMap (problem ^. assignment))

--reduced :: (Problem p) => Index (Clzs p) -> Getter p (Maybe (Index (Clzs p)))
reduced c = to getter where
  getter p = reduceClause (p ^. assignment) c

--implication :: (Problem p) => p -> clause -> Maybe (Index (Vars p), Bool)
--implication = undefined

--reducedSize :: (Clause clause, Assignment mp, Index clause ~ Index mp) => mp -> clause -> Maybe Int
reducedSize agn c = lengthOf literals <$> (reduceClause agn c)

--unitClauseCollection :: (Problem p, clause ~ Index (Clzs p)) => p -> Fold clause clause
unitClauseCollection p = filtered (\c -> reducedSize agn c == Just 1) where
  agn = p ^. assignment

--unitClauseColl :: (Problem p, clause ~ Index (Clzs p), Foldable f) => p -> Fold (f clause) clause
unitClauseColl p = folded . unitClauseCollection p

--problemClauses :: (Problem p, clause ~ Index (Clzs p), Foldable f) => p -> Fold p (f clause)
--problemClauses = allClauses

--problemUnits :: (Problem p, clause ~ Index (Clzs p)) => p -> Fold p (Index (Clzs p))
problemUnits p = allClauses . unitClauseCollection p

---- need to figure out how to "remove" p from unitClauseCollection, so that we can make this a standalone
--unitClauses' :: (Problem problem) => problem -> [Index (Clzs problem)]
--unitClauses' problem = cs ^.. folded . (filtered (\c -> reducedSize agn c == Just 1)) where
--  cs = problem ^. allClauses
--  agn = problem ^. assignment

--reduceClause :: (Clause clause, Index clause ~ var, Ord var, At mp, Index mp ~ Index clause, IxValue mp ~ Bool) => mp -> clause -> Maybe clause
--reduceClause agn cl = loop cl (cl ^@.. literals) where
--  loop c [] = Just c
--  loop c ((v,b):vs) =
--    case (agn ^. at v) of
--      Nothing -> loop c vs
--      Just b'
--        | b == b' -> Nothing
--        | otherwise -> loop (sans v c) vs

-}