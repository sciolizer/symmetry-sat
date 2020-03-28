module Test.MonadSolverTest (

) where

import Data.Proxy

import Families.MonadSolver

testSolver :: (MonadSolver m) => Proxy m -> IO ()
testSolver = undefined

propExitWithSolution = undefined

propPickUnassigned = undefined

propPickUnitClauseFalsy = undefined

propPickUnitClauseUnit = undefined

propPickUnitClauseAllDisjunctive = undefined

{-
class (Monad m) => MonadSprout m where
  sprout :: m a -> m (Maybe a)
  failSprout :: m a
  superSprout :: m a -> m (Maybe a) -- for now, this gives read-only access to the parent's state

class (MonadSprout m, Clause (SolverClause m), SolverVar m ~ Index (SolverClause m)) => MonadSolver m where
  type SolverVar m :: *
  type SolverClause m :: *
  pickUnassignedOrExitWithSolution :: m (SolverVar m)
  pickUnitClause :: m (PickedClause (SolverClause m))
  infer :: SolverClause m -> m (Inference (SolverVar m) (SolverClause m))
  decide :: SolverVar m -> Bool -> m Decision
  learn :: SolverClause m -> m Change -- ignored if clause is already known; affects ALL sprouts
  -- unlearn :: claus -> m Change -- ignored if clause is not known
  notify :: String -> m ()
  dump :: m String
  reduce :: SolverClause m -> m String

-}