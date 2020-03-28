{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Families.CDCL (
  cdcl
) where

import Control.Applicative
import Control.Monad

import Families.Clauses
import Families.MonadSolver

 -- unit propagation and CDCL

cdcl :: (MonadSolver m) => m ()
--cdcl = void unified
cdcl = notify "begin" >> realCDCL

realCDCL :: (MonadSolver m) => m ()
realCDCL = do
  mbLearned <- propagateCDCL -- todo: if this is decision level 0, learning a clause means we are unsatisfiable
  case mbLearned of
    Nothing -> do
--      notify "before pickUnassignedOrExitWithSolution"
--      d <- dump
--      notify d
      v <- pickUnassignedOrExitWithSolution
--      notify "after pickUnassignedOrExitWithSolution"
      sprout $ do
        notify ("decide " ++ show v ++ " False")
        d <- decide v False
        case d of
          NewDecision -> realCDCL
          _ -> error "pickUnassigned picked already assigned"
--      notify "left sprout; about to call backjump"
      -- if we reach here, then the assignment to false failed, and we have learned a new clause
--      dmp <- dump
--      notify $ "dump size: " ++ show (length dmp)
      backjump
      -- there is never a need to assign anything to True - that will happen implicitly thanks to the new learned clause
    Just learned -> do
      notify ("learned new clause: " ++ show learned)
      ch <- learn learned
      case ch of
        Ignored -> do
          notify $ "learned clause was ignored"
          dmp <- dump
--          notify $ "dump: " ++ show dmp
          backjump
        Applied -> backjump

--backjump = do
--  bj <- superSprout pickUnitClause
--  case bj of
--    Nothing -> do
----      notify "no super sprout"
--      cdcl -- todo: is this right?
--    Just AllDisjunctive -> do
--      local <- pickUnitClause
--      case local of
--        Falsy _ -> do
--          notify "edge case where super sprout is all disjunctive but current current sprout is contradictory; failing out one more time"
--          failSprout
--        _ -> do
----          notify "backjumped sufficiently far"
--          cdcl -- have backjumped sufficiently far
--    Just _ -> do
----      notify "must backjump further"
--      failSprout -- unit propagation can be applied at earlier decision level, so we backtrack further

-- "clever" backjumping seems to perform badly on highly symmetric problems
-- in replica test, solver kept jumping back to replica 1 even when it had moved onto later replicas
-- this is because it kept learning new clauses in the later replicas that allowed it to turn some of its
-- replica 1 decision vars into propagation vars - even though the assignments for that decision were in fact
-- already valid.
-- Naive backtracking is probably not optimal, but in replica experiment it actually performs better than backjumping
backjump = do
  local <- pickUnitClause
  case local of
    Falsy c -> do
--      notify $ "fail sprout: " ++ show c
      failSprout
    _ -> realCDCL

propagateCDCL :: (MonadSolver m) => m (Maybe (SolverClause m))
propagateCDCL = do
  mbUnitClause <- pickUnitClause
  case mbUnitClause of
    Falsy c -> do
--      notify "found falsy clause while seeking unit clause"
      return (Just c)
    AllDisjunctive -> do
--      notify "no unit clauses to propagate"
      return Nothing
    Unity clause -> do
--      notify $ "found unit clause to propagate: " ++ show clause
      inf <- infer clause
      case inf of
        NotUnit -> do
          d <- dump
          error $ "pickUnit returned non-unit clause: "  ++ show clause ++ " under " ++ d
        Contradiction v c -> do -- I think we actually want to resolve the contradicting clause with the unit clause here. Alternatively, pickUnitClause could return the broken clause
          cReduced <- reduce c
--          notify ("propagation found a contradiction on " ++ show v ++ ": " ++ show c ++ " which reduces to " ++ show cReduced)
          let Just mbResolved = resolve v c clause
          return (Just mbResolved)
--          return $ mbResolved <|> Just c -- uh why is this?
        Implied var b -> do
--          notify ("Implied " ++ show var ++ " " ++ show b)
          mbLearned <- propagateCDCL
          case mbLearned of
            Nothing -> do
--              notify "propagation finished successfully"
              return Nothing
            Just learned -> do
--              notify "propagation found a conflict"
              let mbResolved = resolve var clause learned -- todo: exit early when learned clause has a unique implication point (FUIP instead of LUIP)
              case mbResolved of
                Nothing -> do
--                  notify $ "failed to resolve " ++ show var ++ " " ++ show clause ++ " " ++ show learned
                  return $ Just learned
                Just z -> do
--                  notify $ "successfully resolved " ++ show var ++ " " ++ show clause ++ " " ++ show learned ++ " => " ++ show z
                  return (Just z)
--              return $ mbResolved <|> Just learned


--unified :: (MonadSolver var clause m) => m clause
--unified = do
--  mbUnitClause <- pickUnitClause
--  case mbUnitClause of
--    Falsy c -> do
--      notify "backtracking"
--      return c
--    Unity clause -> do
--      notify $ "found unit clause to propagate: " ++ show clause
--      inf <- infer clause
--      case inf of
--        NotUnit -> do
--          d <- dump
--          error $ "pickUnit returned non-unit clause: " ++ show clause ++ " under " ++ d
--        Contradiction v c -> do -- I think we actually want to resolve the contradicting clause with the unit clause here. Alternatively, pickUnitClause could return the broken clause
--          notify ("propagation found a contradiction: " ++ show c)
--          let Just mbResolved = resolve v c clause
--          return mbResolved
----          return $ mbResolved <|> Just c -- uh why is this?
--        Implied var b -> do
--          notify ("Implied " ++ show var ++ " " ++ show b)
--          learned <- unified
--          notify "propagation found a conflict"
--          let mbResolved = resolve var clause learned -- todo: exit early when learned clause has a unique implication point (FUIP instead of LUIP)
--          case mbResolved of
--            Nothing -> do
--              notify $ "failed to resolve " ++ show var ++ " " ++ show clause ++ " " ++ show learned
--              return $ learned
--            Just z -> do
--              notify $ "successfully resolved " ++ show var ++ " " ++ show clause ++ " " ++ show learned ++ " => " ++ show z
--              return z -- (Just z)
----              return $ mbResolved <|> Just learned
--    AllDisjunctive -> do
--      notify "deciding"
--      v <- pickUnassignedOrExitWithSolution
--      choose v False
--      choose v True
--      failSprout
--
--choose :: (MonadSolver var clause m) => var -> Bool -> m ()
--choose v b = do
--  learned <- sprout $ do
--    notify ("decide " ++ show v ++ " False")
--    d <- decide v b
--    case d of
--      NewDecision -> unified
--      _ -> error "pickUnassigned picked already assigned"
--  case learned of
--    Just z -> do
--      ch <- learn z
--      case ch of
--        Ignored -> notify "ignored learned clause"
--        Applied -> notify "applied learned clause"
--    Nothing -> return ()
--  uc <- pickUnitClause
--  case uc of
--    Falsy _ -> failSprout
--    _ -> return ()

--whenJust :: (Monad m) => m (Maybe a) -> (a -> m b) -> m (Maybe b)
--whenJust = undefined

-- pointlessly inefficient. I can't think of a reason (yet) that tracking different learned clauses in different sprouts would be useful
--learnInAllSprouts :: (MonadSolver var clause m) => clause -> m [Change] -- head is thisSprout
--learnInAllSprouts = undefined -- can be implemented using learn and superSprout

--isUnit :: (MonadSolver var clause m) => clause -> m Bool
--isUnit = undefined -- can be implemented using sprout and infer

-- uses the ST trick over "clause" to make sure that invalid clauses cannot be learned

-- naive

{-
naive :: (MonadSolver var clause m) => m a
naive = do
  v <- pickUnassignedOrExitWithSolution
  tryAssign v False
  tryAssign v True

tryAssign :: (MonadSolver var clause m) => var -> Bool -> m a
tryAssign v b = sprout $ do
  d <- decide v b
  case d of
    True -> naive
    False -> failSprout

-- using unit propagation

usingUnitPropagation :: (MonadSolver var clause m) => m a
usingUnitPropagation = do
  propagateAll
  v <- pickUnassignedOrExitWithSolution
  tryAssignUsingUnitPropagation v False
  tryAssignUsingUnitPropagation v True

tryAssignUsingUnitPropagation :: (MonadSolver var clause m) => var -> Bool -> m a
tryAssignUsingUnitPropagation v b = sprout $ do
  d <- decide v b
  when (not d) $ error "propagation overlooked a variable"
  usingUnitPropagation

propagateAll :: (MonadSolver var clause m) => m ()
propagateAll = do
  mbUnitClause <- pickUnitClause
  case mbUnitClause of
    Nothing -> return ()
    Just clause -> do
      inf <- infer clause
      case inf of
        NotUnit -> error "pickUnit returned non-unit clause"
        Implied _ _ -> propagateAll
        Contradiction _ -> failSprout
-}
