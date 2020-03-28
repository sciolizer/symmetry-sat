{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Problem.NaiveProblem (
  NaiveProblem
) where

import Control.Lens
--import Control.Lens.Empty
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Families.Assignment
import Families.Clauses
import Problem

data NaiveProblem clause = NaiveProblem {
  assignment :: M.Map (Index clause) Bool,
  clauses :: S.Set clause
}
--  deriving (Eq, Ord, Show)

deriving instance (Eq (Index clause), Eq clause) => Eq (NaiveProblem clause)
deriving instance (Ord (Index clause), Ord clause) => Ord (NaiveProblem clause)
deriving instance (Show (Index clause), Show clause) => Show (NaiveProblem clause)

--newtype instance Vars (NaiveProblem clause) = NaiveProblemVars (NaiveProblem clause)
----  deriving (Ord, Show)
--
--deriving instance (Eq (Index clause), Eq clause) => Eq (Vars (NaiveProblem clause))
--deriving instance (Ord (Index clause), Ord clause) => Ord (Vars (NaiveProblem clause))
--deriving instance (Show (Index clause), Show clause) => Show (Vars (NaiveProblem clause))

assignmentLens :: Lens' (NaiveProblem clause) (M.Map (Index clause) Bool)
assignmentLens = lens g s where
  g (NaiveProblem a _) = a
  s (NaiveProblem a c) a' = NaiveProblem a' c

--type instance Index (Vars (NaiveProblem clause)) = Index clause
--type instance IxValue (Vars (NaiveProblem clause)) = Bool
--instance (Ord (Index clause)) => Ixed (Vars (NaiveProblem clause))
--instance (Ord (Index clause)) => At (Vars (NaiveProblem clause)) where
--  at k = assignmentLens . at k
--instance AsEmpty (Vars (NaiveProblem clause)) where
--  _Empty = undefined
--instance (Ord (Index clause), Show (Index clause), Ord clause, Show clause) => Assignment (Vars (NaiveProblem clause)) where
--  assignmentEntries = assignmentLens . ifolded

--newtype instance Clzs (NaiveProblem clause) = NaiveProblemClzs (NaiveProblem clause)
----  deriving (Show)
--
--deriving instance (Eq (Index clause), Eq clause) => Eq (Clzs (NaiveProblem clause))
--deriving instance (Ord (Index clause), Ord clause) => Ord (Clzs (NaiveProblem clause))
--deriving instance (Show (Index clause), Show clause) => Show (Clzs (NaiveProblem clause))

clausesLens :: Lens' (NaiveProblem clause) (S.Set clause)
clausesLens = lens g s where
  g (NaiveProblem _ c) = c
  s (NaiveProblem a c) c' = NaiveProblem a c'

--type instance Index (Clzs (NaiveProblem clause)) = clause
--type instance IxValue (Clzs (NaiveProblem clause)) = ()
--instance (Ord clause) => Ixed (Clzs (NaiveProblem clause))
--instance (Ord clause) => At (Clzs (NaiveProblem clause)) where
--  at k = clausesLens . at k

--instance (
--  Ord (Index clause), Show (Index clause),
--  Ord clause, Show clause, Clause clause)
--  => Clauses (Clzs (NaiveProblem clause)) where
--  clauses = clausesLens . folded

--instance (Ord var, Show var, Ord clause, Show clause) => Problem var clause (NaiveProblem var clause) where
--  newtype Vars (NaiveProblem var clause) = VNP (NaiveProblem var clause)import Control.Lens (Ixed)

{-
class (
  Assignment (Vars problem),
  Clauses (Clzs problem),
  AsEmpty problem)
  => Problem problem where
  unassigned :: Fold problem (Index (Vars problem))
  falsyClauses :: Fold problem clause
  unitClauses :: Fold problem clause
-}

instance AsEmpty (NaiveProblem clause) where
  _Empty = empty'

empty' :: Prism' (NaiveProblem clause) ()
empty' = prism s g where
  s () = NaiveProblem M.empty S.empty
  g np@(NaiveProblem m s)
    | M.null m && S.null s = Right ()
    | otherwise = Left np

instance (
  Ord (Index clause), Ord clause,
  Show (Index clause), Show clause,
--  Var (NaiveProblem clause) ~ Index ()
  Clause clause)
  => Problem (NaiveProblem clause) where
  type Var (NaiveProblem clause) = Index clause
  type Clz (NaiveProblem clause) = clause

--  pickUnassigned :: PickOne problem (Var problem)
--  pickFalsyClz :: PickOne problem (Clz problem)
--  pickUnitClz :: PickOne problem (Clz problem, Var problem, Bool)
--
--  assignments :: IndexedFold (Var problem) problem VarAssignment
--
--  newProblem :: (Foldable f) => Proxy problem -> f (Var problem) -> problem
--
--  variable :: Var problem -> Lens' problem VarAssignment
--  -- variable :: Var problem -> Maybe (Lens' problem (Maybe Bool))
--
--  -- must throw an error if clause contains any currently irrelevant variables (if fullyRelevnt c == False)
--  clause :: Clz problem -> Lens' problem ClzPresence



varInClause :: (Clause clause) => Fold clause (Index clause)
varInClause = literals . asIndex

varsInClauses :: (Clause clause) => Fold (S.Set clause) (Index clause)
varsInClauses = folding id . varInClause

clausesInNaiveProblem :: Fold (NaiveProblem clause) (S.Set clause)
clausesInNaiveProblem = folding q where
  q (NaiveProblem _ c) = [c]

-- 0 = unknown, 1 = true, 2 = false
classify :: (Clause clause, Index clause ~ var) => var -> Maybe Bool -> clause -> Int
classify _ Nothing _ = 0
classify v (Just b) c = something where
  something =
    case c ^. at v of
      Nothing -> 0
      Just b'
        | b == b' -> 1
        | otherwise -> 2

-- Nothing indicates clause is already satisfied
-- todo: this could probably be made generic and moved into Problem
-- if so, then default implementations of falsy and unit could also
-- be moved
reduceClause :: (Clause clause, Index clause ~ var, Ord var) => M.Map var Bool -> clause -> Maybe clause
reduceClause agn cl = loop cl (toListOf varsInClause cl) where
  loop c [] = Just c
  loop c (v:vs) =
    case classify v (M.lookup v agn) c of
      0 -> loop c vs
      1 -> Nothing
      2 -> loop (sans v c) vs

reduceClause2 :: (Clause clause, Index clause ~ var, Ord var, At mp, Index mp ~ Index clause, IxValue mp ~ Bool) => mp -> clause -> Maybe clause
reduceClause2 agn cl = loop cl (toListOf varsInClause cl) where
  loop c [] = Just c
  loop c (v:vs) =
    case (agn ^. at v, c ^. at v) of
      (Nothing, _) -> loop c vs
      (_, Nothing) -> loop c vs
      (Just b, Just b')
        | b == b' -> Nothing
        | otherwise -> loop (sans v c) vs
