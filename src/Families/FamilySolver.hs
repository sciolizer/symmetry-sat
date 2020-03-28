{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Families.FamilySolver (
  FamilySolverMonadT,
--  runFamilyMonadSolverT,
  search
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Proxy

import Families.CDCL
import Families.Clauses
import Families.MonadSolver
import SimpleProblem
import Problem

-- MonadSolver var clause (FamilySolverMonadT problem Identity)
search :: forall p f m . (Problem p, Show p, Foldable f, MonadNotify m) => p -> f (Clz p) -> m (Maybe (Solution p))
search unconstrainedProblem cls = answer where
  answer = do
    zz <- m
    case zz of
      Left z -> return z
      Right _ -> error "cdcl should never exit normally"
  m = flip runFamilySolverMonadT (insertClauses cls unconstrainedProblem) $ do
--    FamilySolverMonadT $ do
--      forM_ cls $ \cl -> (problemLens . clzs . at cl) .= Just ()
    cdcl

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

valueRWST :: (Monad m) => RWST r w s m a -> r -> s -> m a
valueRWST m r s = fst <$> evalRWST m r s

runFamilySolverMonadT :: (Monad m, Problem p) => FamilySolverMonadT p m a -> p -> m (Either (Maybe (Solution p)) a)
runFamilySolverMonadT (FamilySolverMonadT m) emptyInstance = (valueRWST (runExceptT m) () (State [[]] emptyInstance))

--emptyInstance :: (Problem p) => p
--emptyInstance = undefined -- () ^. re _Empty

data State problem = State {
  stateAssignments :: [[Var problem]],
  internals :: problem
}

deriving instance (Show problem, Show (Var problem)) => Show (State problem)

assignmentsLens :: Lens' (State problem) [[Var problem]]
assignmentsLens = lens stateAssignments (\(State _ i) a -> State a i)

problemLens :: Lens' (State problem) problem
problemLens = lens internals (\(State a _) i -> State a i)

instance MonadNotify m => MonadNotify (FamilySolverMonadT problem m) where
  notify s = FamilySolverMonadT (lift (lift (notify s)))

newtype FamilySolverMonadT problem m a = FamilySolverMonadT { unNaiveMonadSolver :: ExceptT (Maybe (Solution problem)) (RWST () () (State problem) m) a }
  deriving (Applicative, Functor, Monad)

--class InternalsStateMonad var internals m | m -> var, m -> internals where
--  getInternalsState :: m internals
--  putInternalsState :: [var] -> internals -> m ()
--
--instance (Monad m) => InternalsStateMonad var internals (FamilySolverMonadT var clause internals m) where
--  getInternalsState = FamilySolverMonadT $ gets internals {-
--    dl <- ask
--    mp <- get
--    case M.lookup dl mp of
--      Nothing -> error $ "bug! map not initialized for current decision level: " ++ show dl
--      Just v -> return v -}
--
----  putInternals :: (Monad m) => reversible -> DelegatingMonadSolverT var clause reversible m ()
--  putInternalsState newVars pa = FamilySolverMonadT $ do
--    State agns@(a:as) ints <- get
--    put $ State ((newVars ++ a) : as) pa

-- todo: surely this already exists within the lens library somewhere?
(%%=.) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
l %%=. f = l %%= (\z -> ((), f z))

--unassign :: (Problem p, MonadState (State p) m) => [Index (Vars p)] -> m ()
unassign = mapM_ dropVar where
  dropVar var = problemLens . variable var .= Unassigned

instance (Monad m, Problem problem) => MonadSprout (FamilySolverMonadT problem m) where
  sprout (FamilySolverMonadT m) = do
    FamilySolverMonadT $ do
      assignmentsLens %%=. ([]:)
      let sproutWorld = {- local (+1) -} m
      res <- catchError (Right <$> sproutWorld) (return . Left)-- catchError :: m a -> (e -> m a) -> m a
      toErase <- assignmentsLens %%= (\(t:agns) -> (t, agns))
      unassign toErase
      case res of
        Left Nothing -> return Nothing
        Left (Just sol) -> throwError (Just sol)
        Right res -> return (Just res)
  failSprout = FamilySolverMonadT $ throwError Nothing
  superSprout (FamilySolverMonadT m) = FamilySolverMonadT $ do
    restorePoint <- get
    agns <- use assignmentsLens
    case agns of
      [] -> error $ "bug; MutatingSolverState assignments should never be empty"
      [_] -> return Nothing
      (toHide:agns') -> do
        unassign toHide
        let sproutWorld = {- local (\x -> x - 1) -} m
        res <- catchError sproutWorld (error "failing/solving within a super sprout is not supported") -- todo: also guard against sprouting from within a super sprout
        put restorePoint
        return (Just res)

useProblem :: (Problem p, MonadState (State p) m) => (p -> a) -> m a
useProblem g = use (problemLens . to g)

currentUndoList :: (Problem p) => Lens' (State p) [Var p]
currentUndoList = assignmentsLens . lens g s where
  g (vs:_) = vs
  s (vs:vss) vs' = vs':vss

prependUndo v = currentUndoList %%= (\undos -> ((), v:undos)) -- todo: is there a () version of %%= ?

instance (Monad m, MonadNotify m, Show problem, Problem problem) => MonadSolver (FamilySolverMonadT problem m) where
  type SolverVar (FamilySolverMonadT problem m) = Var problem
  type SolverClause (FamilySolverMonadT problem m) = Clz problem
  reduce c = undefined {- do
    rev <- getInternals
    return (siReduce c rev) -}
  dump = FamilySolverMonadT $ do
    pa <- get
    return . show $ pa
--    return "dump not implemented"
  pickUnassignedOrExitWithSolution = do
    FamilySolverMonadT $ do
      s <- get
      case s ^. problemLens . to pickUnassigned of
        Nothing -> do
          sol <- use $ problemLens . to solution
          throwError (Just sol)
        Just v -> return v
  pickUnitClause = do
    FamilySolverMonadT $ do
      s <- get
      case (s ^. problemLens . to pickFalsyClz, s ^. problemLens . to pickUnitClz) of
        (Just f, _) -> return (Falsy f)
        a@(_, (Just (u,_,_))) -> {- trace ("falsy and unit: " ++ show a) -} (return (Unity u))
        _ -> return AllDisjunctive
  infer clause = FamilySolverMonadT $ do
    red <- useProblem (flip simpleReduceClause clause)
    case red of
      Nothing -> return NotUnit
      Just c' ->
        case toListOf (literals . withIndex) c' of
          [] -> return (Contradiction (error "Contradiction v not implemented 29") (error "not sure what c is supposed to be"))
          [(v,b)] -> do
            prependUndo v
            problemLens . variable v .= Assigned b
            return (Implied v b)
          _ -> return NotUnit
  decide var b = FamilySolverMonadT $ do
    bOld <- useProblem (\p -> p ^. variable var)
    case bOld of
      Assigned b'
        | b == b' -> return ReassignedDecision
        | otherwise -> return ConflictingDecision
      Unassigned -> do
        prependUndo var
        problemLens . variable var .= Assigned b
        return NewDecision
  learn c = FamilySolverMonadT $ do
    old <- problemLens . clause c %%= (\o -> (o, Learned))
    case old of
      Absent -> return Applied
      _ -> return Ignored

---- todo: simplify
--learnAll :: (SolverInternals var clause pa) => clause -> pa -> (Change, pa)
--learnAll cl = accum Ignored where
--  accum ch pa =
--    case (ch, siLearn cl pa) of
--      (_, Just pa') -> (Applied, pa')
--      (Applied, Nothing) -> (Applied, pa)
--      (Ignored, Nothing) -> (Ignored, pa)

--getInternals :: (Monad m) => FamilySolverMonadT var clause reversible m reversible
--getInternals = getInternalsState {- DelegatingMonadSolverT $ do
--  dl <- ask
--  mp <- get
--  case M.lookup dl mp of
--    Nothing -> error $ "bug! map not initialized for current decision level: " ++ show dl
--    Just v -> return v -}
--
--putInternals :: (Monad m) => [var] -> reversible -> FamilySolverMonadT var clause reversible m ()
--putInternals = putInternalsState {- DelegatingMonadSolverT $ do
--  dl <- ask
--  modify (M.insert dl pa) -}

--addClausesToEmpty :: (SolverInternals var clause internals, Foldable f) => f clause -> internals -> internals
--addClausesToEmpty cls ni = foldl (flip addClause) ni cls
--
--addClause :: (SolverInternals var clause internals) => clause -> internals -> internals
--addClause clause mi =
--  case siLearn clause mi of
--    Nothing -> mi
--    Just z -> z



-- there's a (literally) odd pattern where every odd-numbered replica takes time to evaluate, while every
-- even numbered replica goes quickly. I suspect that learned clauses are being lost every other replica, somehow.
-- printing out statistics on the size of Partitioned/DedupingMap might clarify things

-- ok, logging "learned new clause", it's clear that clauses are ONLY being learned on the odd numbered replicas.
-- something is definitely being lost