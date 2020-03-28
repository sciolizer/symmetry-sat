{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Problem.MiniSat (
--  MiniSatProblem
) where

import Control.Lens
import Data.Map as M -- non-strict is maybe better?

import Families.Assignment
import Families.Clauses
import qualified OrderedMap as OM
import Problem
import Queue

{-

type VCIndex clause = M.Map (Index clause) [clause]

-- attempts to mimic the data structure of MiniSat
data MiniSatProblem clause = MiniSatProblem {
  -- when Just, only legal move is to undo most recent assignment
  _falseClause :: Maybe clause,

  _assignments :: OM.StackMap (Index clause) Bool,

  _watches :: VCIndex clause,

  -- when non-empty, only legal assignment is the choice at the front of the queue
  _units :: QueueMap (Index clause) (clause, Bool),

  _undos :: VCIndex clause
}

makeLenses ''MiniSatProblem

deriving instance (Show (Index clause), Show clause) => Show (MiniSatProblem clause)

newtype instance Vars (MiniSatProblem clause) = MiniVars (MiniSatProblem clause)

instance (Clause clause) => Eq (Vars (MiniSatProblem clause)) where (==) = undefined
instance (Clause clause) => Ord (Vars (MiniSatProblem clause)) where compare = undefined
deriving instance (Clause clause) => Show (Vars (MiniSatProblem clause))

type instance Index (Vars (MiniSatProblem clause)) = Index clause
type instance IxValue (Vars (MiniSatProblem clause)) = Bool
instance (Clause clause) => Ixed (Vars (MiniSatProblem clause))
instance (Clause clause) => At (Vars (MiniSatProblem clause)) where
  at k = from assignment . atVars k

atVars :: (Clause clause) => Index clause -> Lens' (MiniSatProblem clause) (Maybe Bool)
atVars k = lens g s where
  g p = p ^. assignments . at k
  s p Nothing = popUndo k . popUnit k . reindex k . unassignLast k . clearFalse $ p

type Delta c = MiniSatProblem c -> MiniSatProblem c

clearFalse :: Delta c
clearFalse = set falseClause Nothing

type IndexedDelta c = (Clause c) => Index c -> Delta c

unassignLast :: IndexedDelta c
unassignLast k = set (assignments . at k) Nothing

-- arg....
-- before going crazy with this, I really should just try
-- changing the SetMap in ClauseIndex to my own implementation
reindex :: IndexedDelta c
reindex k = over watches reorg where
  reorg ws = undefined where
    cs = ws ^. at k

popUnit :: IndexedDelta c
popUnit = undefined

popUndo :: IndexedDelta c
popUndo = undefined

instance {- (Clause clause) => -} AsEmpty (Vars (MiniSatProblem clause)) where
  _Empty = undefined
instance (Clause clause) => Assignment (Vars (MiniSatProblem clause)) where
  assignmentEntries = undefined -- assignmentLens . ifolded

newtype instance Clzs (MiniSatProblem clause) = MiniClzs (MiniSatProblem clause)

instance (Clause clause) => Eq (Clzs (MiniSatProblem clause)) where (==) = undefined
instance (Clause clause) => Ord (Clzs (MiniSatProblem clause)) where compare = undefined
deriving instance (Clause clause) => Show (Clzs (MiniSatProblem clause))

--clausesLens :: Lens' (Clzs (MiniSatProblem clause)) (S.Set clause)
--clausesLens = lens g s where
--  g (MiniSatProblemClzs (MiniSatProblem _ c)) = c
--  s (MiniSatProblemClzs (MiniSatProblem a c)) c' = MiniSatProblemClzs (MiniSatProblem a c')

type instance Index (Clzs (MiniSatProblem clause)) = clause
type instance IxValue (Clzs (MiniSatProblem clause)) = ()
instance (Ord clause) => Ixed (Clzs (MiniSatProblem clause))
instance (Ord clause) => At (Clzs (MiniSatProblem clause)) where
  at k = undefined -- clausesLens . at k

instance
  ( Clause clause
  ) => Clauses (Clzs (MiniSatProblem clause)) where
  clauses = undefined -- clausesLens . folded

instance AsEmpty (MiniSatProblem clause) where
  _Empty = undefined -- empty'

instance
  ( Clause clause
  ) => Problem (MiniSatProblem clause) where
  clzs = iso MiniClzs (\(MiniClzs z) -> z)
  assignment = iso MiniVars (\(MiniVars z) -> z)

--  unassigned :: Fold problem (Index (Vars problem))
  unassigned = undefined -- folding unassigned'
--
--  falsyClauses :: Fold problem (Index (Clzs problem))
  falsyClauses = undefined -- falsyClausesFold
--
--  unitClauses :: Fold problem (Index (Clzs problem), Index (Vars problem), Bool)
  unitClauses = undefined -- unitClausesFold
--
--  allClauses :: Fold problem (Index (Clzs problem))
  allClauses = undefined -- clzs . clauses

-}