{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module SimpleProblem (
  Problem(..),
  Solution(..),
  VarAssignment(..),
  ClzPresence(..),
  fullyRelevant,
  insertClauses,
  solution,
  lookupAssignment,
  simpleReduceClause
) where

import Control.Lens
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Set as S

import Families.Assignment
import Families.Clauses

type PickOne problem one = problem -> Maybe one

data VarAssignment = Unassigned | Assigned Bool
  deriving (Eq, Ord, Read, Show)

data ClzPresence = Original | Learned | Absent
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

type Solution p = M.Map (Var p) Bool

class (
  Var problem ~ Index (Clz problem),
  Clause (Clz problem),
  Show problem)
  => Problem problem where
  type Var problem :: *
  type Clz problem :: *

  pickUnassigned :: PickOne problem (Var problem)
  pickFalsyClz :: PickOne problem (Clz problem)
  pickUnitClz :: PickOne problem (Clz problem, Var problem, Bool)

  assignments :: IndexedFold (Var problem) problem VarAssignment

  variable :: Var problem -> Lens' problem VarAssignment
  relevantVariable :: Var problem -> problem -> Bool

  -- must throw an error if clause contains any currently irrelevant variables (if fullyRelevnt c == False)
  clause :: Clz problem -> Lens' problem ClzPresence

fullyRelevant :: (Problem p) => Clz p -> p -> Bool
fullyRelevant c p = all (\v -> relevantVariable v p) (c ^.. varsInClause)

insertClauses :: (Foldable f, Problem p) => f (Clz p) -> p -> p
insertClauses clauses p = foldl (\pp c -> set (clause c) Original pp) p clauses

solution :: (Problem p) => p -> Solution p
solution p = M.fromList . concatMap onlyAssigned $ agns where
  agns = p ^.. assignments . withIndex
  onlyAssigned (v, Unassigned) = []
  onlyAssigned (v, Assigned b) = [(v, b)]

lookupAssignment :: (Problem p) => p -> Var p -> VarAssignment
lookupAssignment = undefined

--type VarAssignmentLookup p = Var p -> VarAssignment

--instance Assignment (v -> VarAssignment) where

-- reduceClause :: (Clause clause) => (Index clause -> Maybe Bool) -> clause -> Maybe clause
simpleReduceClause :: (Problem p) => p -> Clz p -> Maybe (Clz p)
simpleReduceClause p = reduceClause lkup where
  lkup var =
    case p ^. variable var of
      Unassigned -> Nothing
      Assigned b -> Just b

-- what have we achieved here?
-- 1) we can distinguish between irrelevant and unassigned variables, which will make partitioning possible
-- 2) the Vars/Clzs declaration is gone, so declaring things as problems is simpler (less "deriving instance" crap)
-- 3) PickOne is easier to implement then Fold; no deduping is required, and so implementations are more likely to be fast

