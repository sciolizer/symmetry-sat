module Families.SimpleSolver (

) where

{-

instance (Monad m, MonadNotify m, SimpleProblem problem) => MonadSolver (FamilySolverMonadT problem m) where
  type SolverVar (FamilySolverMonadT problem m) = Index (Vars problem)
  type SolverClause (FamilySolverMonadT problem m) = Index (Clzs problem)
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
      case toListOf (problemLens . unassigned) s of
        [] -> do
          sol <- use $ problemLens . assignment
          throwError (Just sol)
        (v:_) -> return v
  pickUnitClause = do
    FamilySolverMonadT $ do
      s <- get
      case (toListOf (problemLens . falsyClauses) s, toListOf (problemLens . unitClauses) s) of
        ((f:_), _) -> return (Falsy f)
        a@(_, ((u,_,_):_)) -> {- trace ("falsy and unit: " ++ show a) -} (return (Unity u))
        _ -> return AllDisjunctive
  infer clause = FamilySolverMonadT $ do
    red <- useProblem (reduced clause)
    case red of
      Nothing -> return NotUnit
      Just c' ->
        case toListOf (literals . withIndex) c' of
          [] -> return (Contradiction (error "Contradiction v not implemented 29") (error "not sure what c is supposed to be"))
          [(v,b)] -> do
            prependUndo v
            problemLens . assignment . at v .= Just b
            return (Implied v b)
          _ -> return NotUnit
  decide var b = FamilySolverMonadT $ do
    bOld <- useProblem (assignment . at var)
    case bOld of
      Just b'
        | b == b' -> return ReassignedDecision
        | otherwise -> return ConflictingDecision
      Nothing -> do
        prependUndo var
        problemLens . assignment . at var .= Just b
        return NewDecision
  learn clause = FamilySolverMonadT $ do
    old <- problemLens . clzs . at clause %%= (\o -> (o, Just ()))
    case old of
      Nothing -> return Applied
      Just () -> return Ignored

-}