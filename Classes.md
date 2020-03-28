obviously, ReplicaClause can be removed and/or made as a shortcut for pairs of Replicas
Group class can be removed, and all usages replaced with Replica
TMC a b c is sort of like Map a (b, c)... so if we create a Map-like interface, we can probably use that
UncheckedInternals::unassigned <=> WatchedClausesUnderAssignment::pickUnit (if we Maybe <=> PickedClause)
much of MonadSolver is just monadic variants of SovlerInternals... and lens probably provides most of this
At/Ixed probably gives us the Map-like interface we are seeking.... need to explore this more
  At is for inserting missing values (and maybe for deletion too? idk)

in fact.... so many of these functions fall into either "getter" categories or "setter" categories...
yet we're not using any of lens for that, though it's the obvious choice

class (Show replica, Show avar, Show gvar, Ord replica, Ord avar, Ord gvar) => Replica replica avar gvar | gvar -> replica, gvar -> avar where
  abstractions :: gvar -> [(replica, avar)]
  instantiate :: replica -> avar -> gvar

class (Replica replica avar gvar, Replica replica avarClause gvarClause, Clause avar avarClause, Clause gvar gvarClause) => ReplicaClause replica avar gvar avarClause gvarClause | gvarClause -> avarClause where
  abstractClause :: gvarClause -> [(replica, avarClause)]
  instantiateClause :: replica -> avarClause -> gvarClause

class (Show tmc) => TMC tmc where
  type Fst tmc :: *
  type Snd tmc :: *
  type Thd tmc :: *
  type MK tmc :: *
  empty :: tmc -- lens has AsEmpty!
  lookupReturningKey :: Fst tmc -> tmc -> (Snd tmc, MK tmc)
  lookupUsingMiddleKey :: MK tmc -> tmc -> Thd tmc
  insertChain :: Fst tmc -> Snd tmc -> Thd tmc -> tmc -> tmc
  fmapLastWithMiddle :: (Snd tmc -> Thd tmc -> Thd tmc) -> tmc -> tmc
  lookupMiddleKey :: Snd tmc -> tmc -> Maybe (MK tmc)
  firstMap :: tmc -> [(Fst tmc, Snd tmc)]


-- .... can we do something like:
-- Index (Vars internals) ~ var, IxValue (Vars internals) ~ Maybe Bool
-- Index (Clauses internals) ~ clause, IxValue (Clauses internals) ~ clause
-- ?
class (Clauses clause internals, Ixed internals, Index internals ~ var, IxValue internals ~ Maybe Bool) => UncheckedInternals var clause internals | internals -> var, internals -> clause where
  unassigned :: Fold internals var
  falsyClauses :: Fold internals clause
  unitClauses :: Fold internals clauses

class (Show var, Show clause, Show wc, UncheckedInternals var clause wc) => UncheckedWatchedClauses var clause wc | wc -> var, wc -> clause where
  insert :: clause -> wc -> wc
  initial :: (Foldable f) => f var -> wc

class (Show var, Show clause, Show wc) => WatchedClausesUnderAssignment var clause wc | wc -> var, wc -> clause where
  pickUnit :: wc -> PickedClause clause
  assign :: var -> Bool -> wc -> Assignment wc
  insert :: clause -> wc -> Maybe wc
--  empty :: wc

class (Show var, Show assignment, Unassignable var assignment) => Assignment var assignment | assignment -> var where
  assignment :: var -> assignment -> Maybe Bool
  uncheckedAssign :: var -> Bool -> assignment -> assignment
  emptyAssignment :: assignment

class Clauses clause clauses | clauses -> clause where
  fromClauses :: (Foldable f) => f clause -> clauses
  insertClause :: clause -> clauses -> clauses

class (Ord var, Show var, Ord clause, Show clause) => Clause var clause | clause -> var where
  resolve :: var -> clause -> clause -> Maybe clause
  toList :: clause -> [(var, Bool)]
  toClause :: [(var, Bool)] -> clause

class Literal var literal | literal -> var where
  var :: literal -> var
  val :: literal -> Bool
  toLiteral :: (var, Bool) -> literal

class (Monad m) => MonadSprout m where
  sprout :: m a -> m (Maybe a)
  failSprout :: m a
  superSprout :: m a -> m (Maybe a) -- for now, this gives read-only access to the parent's state

class (Show var, Show clause, MonadSprout m, Clause var clause) => MonadSolver var clause m | m -> var, m -> clause where
  pickUnassignedOrExitWithSolution :: m var
  pickUnitClause :: m (PickedClause clause)
  infer :: clause -> m (Inference var clause)
  decide :: var -> Bool -> m Decision
  learn :: clause -> m Change -- ignored if clause is already known; affects ALL sprouts
  -- unlearn :: claus -> m Change -- ignored if clause is not known
  notify :: String -> m ()
  dump :: m String
  reduce :: clause -> m String

class (Show var, Show clause, Show reversible) => SolverInternals var clause reversible | reversible -> var, reversible -> clause where
  siPickUnassigned :: reversible -> Either (M.Map var Bool) var
  siPickUnitClause :: reversible -> PickedClause clause
  siInfer :: reversible -> clause -> SIInference var clause reversible
  siDecide :: reversible -> var -> Bool -> SIDecision reversible
  siLearn :: clause -> reversible -> Maybe reversible -- Nothing is clause is already known
  siReduce :: clause -> reversible -> String

class (Show var, Show state) => Unassignable var state | state -> var where
  unassign :: (Foldable f) => f var -> state -> state

