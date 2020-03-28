# structured-sat


-- compact package can help with garbage collection, if that becomes a problem
-- in/out functor types could have just been bifunctor
-- replica and replicaclause have identical definitions... could be merged

reference of all major type classes here (TODO):

class (Monad m) => MonadSprout m where

class (MonadSprout m) => MonadSolver var clause m | m -> var, m -> clause where
instance (Monad m) => MonadSprout (DelegatingMonadSolverT var clause reversible m) where

class SolverInternals var clause reversible | reversible -> var, reversible -> clause where
instance (Show group, Show avar, Show gvar, Group group avar gvar) => SolverInternals gvar (Clause group avar gvar) (NaiveInternals group avar gvar) where
instance (Ord var, Clause var clause, WatchedClausesUnderAssignment var clause wcua) => SolverInternals var clause (WatchedInternals var clause wcua) where

class Unassignable var state | state -> var where
instance (Ord clause, Unassignable var uwc, Clause var clause, Assignment var agn) => Unassignable var (DelegatingWatchedClausesUnderAssignment var clause agn uwc) where
instance (Unassignable var wcua, Ord var) => Unassignable var (WatchedInternals var clause wcua) where

class WatchedClausesUnderAssignment var clause wc | wc -> var, wc -> clause where
instance (Show var, Show clause, Show agn, Show uwc, Ord var, Ord clause, Clause var clause, Assignment var agn, UWC.UncheckedWatchedClauses var clause uwc) => WCA.WatchedClausesUnderAssignment var clause (DelegatingWatchedClausesUnderAssignment var clause agn uwc) where

class UncheckedWatchedClauses var clause wc | wc -> var, wc -> clause where
instance (Ord replica, Show gvar, Show avarClause, Show replica, Show assignment, Show avar, Ord avarClause, Ord avar, Ord assignment, Assignment avar assignment, ReplicaClause replica avar gvar avarClause gvarClause, Ord gvarClause, TMC.TMC tmc, TMC.Fst tmc ~ replica, TMC.Snd tmc ~ assignment, TMC.Thd tmc ~ (M.Map avar (S.Set avarClause))) => UWC.UncheckedWatchedClauses gvar gvarClause (CompactingWatchedClausesUnderAssignment replica avar gvar assignment avarClause gvarClause tmc) where
instance (Ord var, Ord clause, Clause var clause) => UncheckedWatchedClauses var clause (NaiveUncheckedWatchedClauses var clause) where

class Replica replica avar gvar | gvar -> replica, gvar -> avar where


class MonadSprout
  instance DelegatingMonadSolverT: class SolverInternals construction

class SolverInternals
  instance NaiveInternals (no deps, but restricted to "Group replica avar gvar")
  instance WatchedInternals: class WatchedClausesUnderAssignment usage

class WatchedClausesUnderAssignment
  instance DelegatingWatchedClausesUnderAssignment: class UncheckedWatchedClauses usage

class UncheckedWatchedClauses
  instance CompactingWatchedClausesUnderAssignment: class TMC usage
  instance NaiveUncheckedWatchedClauses: no deps

class TMC
  instance NaiveTripleMapChain: no deps
  instance DedupingTripleMapChain: no deps, currently broken

class Clause
  instance MixedClause - probably worth keeping, although should be retrofitted to work with current Clause interface
  instance M.Map var Bool
  instance RepClause - special case fof MixedClause; could remove after cleaning up (and maybe fixing MixedClause)


