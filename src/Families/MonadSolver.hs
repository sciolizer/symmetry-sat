{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Families.MonadSolver (
  MonadNotify(..),
  MonadSprout(..),
  MonadSolver(..),
  Inference(..),
  Change(..),
  PickedClause(..),
  Decision(..)
) where

import Control.Lens
import Families.Clauses
import Families.MonadNotify

data Inference var clause = Implied var Bool | Contradiction var clause | NotUnit

data PickedClause clause = Falsy clause | Unity clause | AllDisjunctive
  deriving (Functor)

data Change = Applied | Ignored
  deriving (Bounded, Enum, Eq, Ord, Show)

data Decision = NewDecision | ReassignedDecision | ConflictingDecision
  deriving (Bounded, Enum, Eq, Ord, Show)

class (Monad m) => MonadSprout m where
  sprout :: m a -> m (Maybe a)
  failSprout :: m a
  superSprout :: m a -> m (Maybe a) -- for now, this gives read-only access to the parent's state

class (MonadSprout m, MonadNotify m, Clause (SolverClause m), SolverVar m ~ Index (SolverClause m)) => MonadSolver m where
  type SolverVar m :: *
  type SolverClause m :: *
  pickUnassignedOrExitWithSolution :: m (SolverVar m)
  pickUnitClause :: m (PickedClause (SolverClause m))
  infer :: SolverClause m -> m (Inference (SolverVar m) (SolverClause m))
  decide :: SolverVar m -> Bool -> m Decision
  learn :: SolverClause m -> m Change -- ignored if clause is already known; affects ALL sprouts
  -- unlearn :: claus -> m Change -- ignored if clause is not known
  dump :: m String
  reduce :: SolverClause m -> m String

