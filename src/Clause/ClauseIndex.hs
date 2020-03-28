{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

module Clause.ClauseIndex (
--  ClausePartition(..),
--  defaultReindex,
  IndexedProblem,
--  TotalClauseIndex,
--  VarAssignment,
--  insertTotal
  newIndexedProblem
) where

import Control.Arrow
import Control.Lens
import Control.Lens.At
import Control.Lens.Empty
import Data.Foldable
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable

import Families.Assignment
import Families.Clauses
import Families.Relation
import Problem
import SimpleProblem

--newtype instance Vars (IndexedProblem clause) = IndexedProblemVars (IndexedProblem clause)
--
--deriving instance (Clause clause) => Eq (Vars (IndexedProblem clause))
--deriving instance (Clause clause) => Ord (Vars (IndexedProblem clause))
--deriving instance (Clause clause) => Show (Vars (IndexedProblem clause))
--
--type instance Index (Vars (IndexedProblem clause)) = Index clause
--type instance IxValue (Vars (IndexedProblem clause)) = Bool
--instance (Clause clause) => Ixed (Vars (IndexedProblem clause))
--instance (Clause clause) => At (Vars (IndexedProblem clause)) where
--  at = atVIP -- assignmentLens . at k

-- variable :: Var problem -> Lens' problem VarAssignment
--atVIP :: (Clause clause) => Index clause -> Lens' (Vars (IndexedProblem clause)) (Maybe Bool)
atVIP :: (Clause clause) => Index clause -> Lens' (IndexedProblem clause) VarAssignment
atVIP k = lens g s where
  g (IndexedProblem (VarAssignments m _) _) =
    case M.lookup k m of
      Nothing -> Unassigned
      Just b -> Assigned b
  s o@(IndexedProblem (VarAssignments m u) index) Unassigned =
    case (M.lookup k m, S.member k u) of
      (Nothing, False) -> error "attempting to unassign unknown variable"
      (Just _, True) -> error "VarAssignment inconsistency; variable is both assigned and unassigned"
      (Nothing, True) -> o
      (Just _, False) -> IndexedProblem (VarAssignments m' (S.insert k u)) index' where
        m' = M.delete k m
        index' = reindexTotal (\kk -> M.lookup kk m') k index
  s o@(IndexedProblem (VarAssignments m u) index) (Assigned b) =
    case (M.lookup k m, S.member k u) of
      (Nothing, False) -> error "attempting to assign unknown variable"
      (Just _, True) -> error "VarAssignment inconsistency; variable is both assigned and unassigned"
      (Just b', False)
        | b == b' -> o
        | b /= b' -> error "mutating variable in place unsupported; unassign and reassign instead"
      (Nothing, True) -> IndexedProblem (VarAssignments m' (S.delete k u)) index' where
        m' = M.insert k b m
        index' = reindexTotal (\kk -> M.lookup kk m') k index

        -- reindexTotal :: (Assignment agn, Clause clause, Index agn ~ Index clause) => agn -> Index agn -> TotalClauseIndex clause -> TotalClauseIndex clause

--instance {- (Clause clause) => -} AsEmpty (Vars (IndexedProblem clause)) where
--  _Empty = undefined

--instance (Clause clause) => Assignment (Vars (IndexedProblem clause)) where
---- assignmentEntries :: IndexedFold (Index clause) (Vars (IndexedProblem clause)) Bool
--  assignmentEntries = from assignment . ipAssignmentsLens . assignedLens . ifolded

--newtype instance Clzs (IndexedProblem clause) = IndexedProblemClzs (IndexedProblem clause)
--
--deriving instance (Clause clause) => Eq (Clzs (IndexedProblem clause))
--deriving instance (Clause clause) => Ord (Clzs (IndexedProblem clause))
--deriving instance (Clause clause) => Show (Clzs (IndexedProblem clause))
--
--type instance Index (Clzs (IndexedProblem clause)) = clause
--type instance IxValue (Clzs (IndexedProblem clause)) = ()
--instance (Clause clause) => Ixed (Clzs (IndexedProblem clause))
--instance (Clause clause) => At (Clzs (IndexedProblem clause)) where
--  at = atCIP -- clausesLens . at k

-- clause :: Clz problem -> Lens' problem ClzPresence
--atCIP :: (Clause clause) => clause -> Lens' (Clzs (IndexedProblem clause)) (Maybe ())
atCIP :: (Clause clause) => clause -> Lens' (IndexedProblem clause) ClzPresence
atCIP clause = lens g s where
  g (IndexedProblem _ index) = memberTotal clause index
--  s _ Absent = error "deleting clauses not currently supported"
  s p@(IndexedProblem va@(VarAssignments agn _) index) clauseOrigin = answer where
    answer = IndexedProblem va' index'
    va' = if fullyRelevant clause p then va else error "attempted to add irrelevant clause"
    index' = insertTotal (\k -> M.lookup k agn) index taggedClause
    taggedClause =
      case clauseOrigin of
        Absent -> error "deleting clauses not currently supported"
        Original -> TOriginal clause
        Learned -> TLearned clause

--addMissingVariables :: (Ord var) => [var] -> VarAssignment var -> VarAssignment var
--addMissingVariables vs va = foldl include va vs where
--  include vv@(VarAssignment m s) v =
--    case M.lookup v m of
--      Nothing -> VarAssignment m (S.insert v s)
--      Just _ -> vv

--instance (
--  Ord (Index clause), Show (Index clause),
--  Ord clause, Show clause, Clause clause)
--  => Clauses (Clzs (IndexedProblem clause)) where
--  clauses = from clzs . ipTCIndexLens . partitionFold . gcpClauseFold
--
--instance (Clause clause) => AsEmpty (IndexedProblem clause) where
--  _Empty = emptyIndexedProblem

--emptyIndexedProblem :: (Clause clause) => Prism' (IndexedProblem clause) ()
--emptyIndexedProblem = prism s g where
--  s () = IndexedProblem (VarAssignment M.empty S.empty) emptyTotalClauseIndex
--  g np@(IndexedProblem va tci)
--    | va == VarAssignment M.empty S.empty && tci == emptyTotalClauseIndex = Right ()
--    | otherwise = Left np

instance (
  Clause clause
  ) => Problem (IndexedProblem clause) where
  type Var (IndexedProblem clause) = Index clause
  type Clz (IndexedProblem clause) = clause

--  pickUnassigned :: PickOne problem (Var problem) -- problem -> Maybe (Var problem)
  pickUnassigned (IndexedProblem (VarAssignments _ u) _) =
    case S.toList u of
      [] -> Nothing
      (v:_) -> Just v

--  pickFalsyClz :: PickOne problem (Clz problem)
-- Relation (Index clause) (Tagged clause)
  pickFalsyClz (IndexedProblem _ (TotalClauseIndex _ (GenericClausePartition _ relation) _ _)) =
    case relation ^.. foldRight of
      [] -> Nothing
      (TOriginal c : _) -> Just c
      (TLearned c : _) -> Just c

--  pickUnitClz :: PickOne problem (Clz problem, Var problem, Bool)
-- todo: unify with pickFalsyClz
  pickUnitClz (IndexedProblem (VarAssignments agns _) (TotalClauseIndex _ _ (GenericClausePartition _ relation) _)) = implication <$> mbClause where
    mbClause =
      case relation ^.. foldRight of
        [] -> Nothing
        (TOriginal c : _) -> Just c
        (TLearned c : _) -> Just c
    implication c =
      case clauseToTuples <$> reduceClause (\k -> M.lookup k agns) c of
        Just [(v, b)] -> (c, v, b)
        _ -> error "internal bug: pulled not unit clause from unit index"

--
--  assignments :: IndexedFold (Var problem) problem VarAssignment
  assignments = ifolding z where
    z (IndexedProblem (VarAssignments agns u) _) = assignedHalf ++ unassignedHalf where
      assignedHalf = map (second Assigned) . M.toList $ agns
      unassignedHalf = map (\v -> (v, Unassigned)) . S.toList $ u

--
--  variable :: Var problem -> Lens' problem VarAssignment
  variable = atVIP

--  relevantVariable :: Var problem -> problem -> Bool
  relevantVariable v (IndexedProblem (VarAssignments agns uagns) _) = M.member v agns || S.member v uagns

--
--  -- must throw an error if clause contains any currently irrelevant variables (if fullyRelevnt c == False)
-- todo: add fullyRelevant check
--  clause :: Clz problem -> Lens' problem ClzPresence
  clause = atCIP


newIndexedProblem :: (Foldable f, Clause clause) => f (Index clause) -> IndexedProblem clause
newIndexedProblem vars = IndexedProblem (VarAssignments M.empty (S.fromList (toList vars))) emptyTotalClauseIndex





--  clzs = iso IndexedProblemClzs (\(IndexedProblemClzs z) -> z)
--  assignment = iso IndexedProblemVars (\(IndexedProblemVars z) -> z)
  -- in addition to the undefineds above, we need to shortcut the falsyClauses, unitClauses etc

--  unassigned :: Fold problem (Index (Vars problem))
--  unassigned = ipAssignmentsLens . unassignedLens . folded -- folding unassigned'
--
--  falsyClauses :: Fold problem (Index (Clzs problem))
--  falsyClauses = ipTCIndexLens . tciFalsyIndexLens . gcpClauseFold -- falsyClausesFold
--
--  unitClauses :: Fold problem (Index (Clzs problem), Index (Vars problem), Bool)
-- this is such a common pattern, where I need to "access" the problem object in order to map the final portion of the chain
-- should probably go read some lens documentation and videos again just to make sure I'm not completely overlooking
-- an easy way to do this
--  unitClauses = foldUsingValue using where -- unitClausesFold
--    using problem = ipTCIndexLens . tciUnitIndexLens . gcpClauseFold . to withPropagation where
--      withPropagation c = (c, v, b) where
--        (v, b) =
--          case watches (problem ^. assignment) c of
--            Just [(v, b)] -> (v, b)
--            z -> error $ "non unit clause " ++ show c ++ " => " ++ show z ++ " returned by tciUnitIndexLens: " ++ show (problem ^.. ipTCIndexLens . tciUnitIndexLens . to (\(GenericClausePartition _ sm) -> rawShow sm)) ++ " with relations " ++ show (problem ^.. ipTCIndexLens . tciUnitIndexLens . to show) ++ " in problem " ++ show problem

-- this problem didn't exist back when I was using SetMap, so the problem must be in the code that was changed since then

-- type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
foldUsingValue :: (s -> Fold s a) -> Fold s a
foldUsingValue ff = folding sfa where
  sfa s = FoldWrap s (ff s)

data FoldWrap s a = FoldWrap s (Fold s a)

instance Foldable (FoldWrap s) where
--  foldMap f (FoldWrap fd) = foldMapOf fd f
--  foldMap f (FoldWrap fd) = foldOf (to f)
  foldr merge seed (FoldWrap s fd) = foldrOf fd merge seed s

data VarAssignments var = VarAssignments (M.Map var Bool) (S.Set var)
  deriving (Eq, Ord, Show)

assignedLens :: Lens' (VarAssignments var) (M.Map var Bool)
assignedLens = lens g s where
  g (VarAssignments a _) = a
  s (VarAssignments _ s) a = VarAssignments a s

unassignedLens :: Lens' (VarAssignments var) (S.Set var)
unassignedLens = lens g s where
  g (VarAssignments _ a) = a
  s (VarAssignments m _) a = VarAssignments m a

--type instance Index (VarAssignment var) = var
--type instance IxValue (VarAssignment var) = Bool
--instance AsEmpty (VarAssignment var) where
--  _Empty = undefined
--instance Ixed (VarAssignment var)
--instance At (VarAssignment var)
--instance (Ord var, Show var) => Assignment (VarAssignment var) where

data IndexedProblem clause = IndexedProblem {
  assignments :: VarAssignments (Index clause),
  tcIndex :: TotalClauseIndex clause
}
--  deriving ({- Eq, Ord, -} Show)

ipAssignmentsLens :: Lens' (IndexedProblem clause) (VarAssignments (Index clause))
ipAssignmentsLens = lens g s where
  g (IndexedProblem a _) = a
  s (IndexedProblem _ i) a = IndexedProblem a i

ipTCIndexLens :: Lens' (IndexedProblem clause) (TotalClauseIndex clause)
ipTCIndexLens = lens g s where
  g (IndexedProblem _ i) = i
  s (IndexedProblem a _) i = IndexedProblem a i

deriving instance (Clause clause) => Eq (IndexedProblem clause)
deriving instance (Clause clause) => Ord (IndexedProblem clause)
deriving instance (Clause clause) => Show (IndexedProblem clause)

data TotalClauseIndex clause = TotalClauseIndex {
  satisfiedIndex :: GenericClausePartition clause,
  falsyIndex :: GenericClausePartition clause,
  unitIndex :: GenericClausePartition clause,
  disjunctiveIndex :: GenericClausePartition clause
}
--  deriving ({- -Eq, Ord, -} Show)

tciFalsyIndexLens :: Lens' (TotalClauseIndex clause) (GenericClausePartition clause)
tciFalsyIndexLens = lens g s where
  g (TotalClauseIndex _ f _ _) = f
  s (TotalClauseIndex s _ u d) f = TotalClauseIndex s f u d

tciUnitIndexLens :: Lens' (TotalClauseIndex clause) (GenericClausePartition clause)
tciUnitIndexLens = lens g s where
  g (TotalClauseIndex _ _ u _) = u
  s (TotalClauseIndex s f _ d) u = TotalClauseIndex s f u d

deriving instance (Clause clause) => Eq (TotalClauseIndex clause)
deriving instance (Clause clause) => Ord (TotalClauseIndex clause)
deriving instance (Clause clause) => Show (TotalClauseIndex clause)

partitionFold :: Fold (TotalClauseIndex clause) (GenericClausePartition clause)
partitionFold = folding (\(TotalClauseIndex s f u d) -> [s, f, u, d])

--emptyTotalClauseIndex ::
emptyTotalClauseIndex = TotalClauseIndex s f u d where
  gcp part = GenericClausePartition part Empty
  s = gcp satisfiedPartition
  f = gcp falsyPartition
  u = gcp unitPartition
  d = gcp disjunctivePartition

reindexTotal :: (Clause clause) => AssignmentLookup clause -> Index clause -> TotalClauseIndex clause -> TotalClauseIndex clause
reindexTotal agn var (TotalClauseIndex si fi ui di) = answer where
  (ej1, si') = reindex agn var si
  (ej2, fi') = reindex agn var fi
  (ej3, ui') = reindex agn var ui
  (ej4, di') = reindex agn var di
  ejs = S.unions [ej1, ej2, ej3, ej4]
  answer = foldl (insertTotal agn) (TotalClauseIndex si' fi' ui' di') ejs

memberTotal :: (Clause clause) => clause -> TotalClauseIndex clause -> ClzPresence
memberTotal c (TotalClauseIndex s f u d) = foldl1 mergeClzPresence . map (partMember c) $ [s, f, u, d]

mergeClzPresence :: ClzPresence -> ClzPresence -> ClzPresence
mergeClzPresence Absent c = c
mergeClzPresence c Absent = c
mergeClzPresence _ _ = error "attempted to merge two non-absent clause presences"

insertTotal :: (Clause clause) => AssignmentLookup clause -> TotalClauseIndex clause -> Tagged clause -> TotalClauseIndex clause
insertTotal agn (TotalClauseIndex s f u d) c =
  let ti = tryInsert agn c in
  case (ti s, ti f, ti u, ti d) of
    (Just s', Nothing, Nothing, Nothing) -> TotalClauseIndex s' f u d
    (Nothing, Just f', Nothing, Nothing) -> TotalClauseIndex s f' u d
    (Nothing, Nothing, Just u', Nothing) -> TotalClauseIndex s f u' d
    (Nothing, Nothing, Nothing, Just d') -> TotalClauseIndex s f u d'
    _ -> error "partitions are not mutually exhaustive or disjoint"

insertAll agn = foldl insert where
  insert i c = case tryInsert agn c i of { Just i' -> i'; Nothing -> error "insertAll failed" }

-- change in Assignment for each reindex must be the addition or removal of a single assignment,
-- never the change of a single assignment
class (Clause (PClz index)) => ClausePartition index where
  type PClz index :: *
  -- returns index with var eliminated. Callers must call tryInsert again (with a possibly modified
  -- assignment) to keep the partition in a consistent state
  relevant :: Index (PClz index) -> index -> (S.Set (Tagged (PClz index)), index)
  tryInsert :: AssignmentLookup (PClz index) -> Tagged (PClz index) -> index -> Maybe index

  -- returns set of clauses which were ejected thanks to the assignment change
  reindex :: AssignmentLookup (PClz index) -> Index (PClz index) -> index -> (S.Set (Tagged (PClz index)), index)
  reindex = defaultReindex

--defaultReindex :: (ClausePartition index, Assignment agn, Index agn ~ Index (PClz index)) => agn -> (Index agn) -> index -> (S.Set (PClz index), index)
defaultReindex agn v index = foldl classify (S.empty, reduced) triggered where
  (triggered, reduced) = relevant v index
  classify (ej, i) c =
    case tryInsert agn c i of
      Nothing -> (S.insert c ej, i)
      Just i' -> (ej, i')

data Tagged clause = TOriginal clause | TLearned clause
  deriving (Eq, Ord, Read, Show)

data GenericClausePartition clause = GenericClausePartition (RealClausePartition clause) (Relation (Index clause) (Tagged clause))
--  deriving ({- Eq, Ord, -} Show)

deriving instance (Clause clause) => Eq (GenericClausePartition clause)
deriving instance (Clause clause) => Ord (GenericClausePartition clause)
deriving instance (Clause clause) => Show (GenericClausePartition clause)

partMember :: (Clause clause) => clause -> GenericClausePartition clause -> ClzPresence
partMember c (GenericClausePartition _ sm) = foldl1 mergeClzPresence . map hasC $ vs where
  vs = c ^.. varsInClause
  hasC v =
    case (sm ^. contains (v, TOriginal c), sm ^. contains (v, TLearned c)) of
      (False, False) -> Absent
      (True, False) -> Original
      (False, True) -> Learned
      (True, True) -> error "clause is both original and learned"

--gcpClauseFold :: (Ord clause) => Fold (GenericClausePartition clause) clause
----gcpClauseFold = folding (\(GenericClausePartition _ sm) -> S.unions . toList . SM.toMap $ sm)
--gcpClauseFold = to (\(GenericClausePartition _ sm) -> sm) . foldRight

--safeHead :: GenericClausePartition clause -> Maybe clause
--safeHead (GenericClausePartition _ mp) =
--  case M.toList . SM.toMap $ mp of
--    [] -> Nothing
--    ((_,s):_) ->
--      case S.toList s of
--        [] -> Nothing
--        (z:_) -> Just z

watches :: (Clause c) => AssignmentLookup c -> c -> Maybe [(Index c, Bool)]
watches a c = clauseToTuples <$> reduceClause a c

instance (Clause clause) => ClausePartition (GenericClausePartition clause) where
  type PClz (GenericClausePartition clause) = clause

  -- bug here, at least under the current design.
  -- it is not enough to delete this variable... for all clauses under that variable,
  -- we must also remove them from any other variables which might be indexing them
  -- this makes me think that a SetMap might be the wrong implementation
  -- todo: consider an alternative implementation, e.g. Set (var, clause) which supports:
  --   fast delete (v, *)
  --   fast delete (*, c)
  relevant v (GenericClausePartition rcp sm) = (affected, GenericClausePartition rcp . dropAffected . deleteV $ sm) where
    affected = S.fromList (toList (sm ^. atLeft v)) -- todo: how to fold directly into set
    dropAffected ss = foldl (\sm' c -> set (atRight c) Empty sm') ss affected
    deleteV = id -- previous impl was redundant with dropAffected: set (atLeft v) Empty
--    dropAffected sm = foldl removeClause sm affected
--    removeClause sm c = foldl (\sm' vv -> uninsert vv c sm') sm (c ^.. varsInClause)

  tryInsert agn taggedC (GenericClausePartition rcp sm) =
    let c = case taggedC of { TOriginal cc -> cc; TLearned cc -> cc } in
    case rcpIndices rcp (watches agn c) c of
      Just vs -> Just . GenericClausePartition rcp . foldl insert sm $ vs where
        insert sm' v = set (contains (v, taggedC)) True sm'
      _ -> Nothing
  reindex agn v d@(GenericClausePartition rcp sm) =
    case skipReindexForAssignment rcp (agn v) of
      True -> (S.empty, d)
      False -> defaultReindex agn v d

---- omg this data structure sucks
---- ok this does seem like it's taking up quite a bit of time... 20% maybe?
---- so definitely worth replacing with a different data structure
--uninsert :: (Ord k, Ord a) => k -> a -> SM.SetMap k a -> SM.SetMap k a
--uninsert k elem sm = insertWithoutElem . SM.delete k $ sm where
--  atK = SM.lookup k sm
--  withoutElem = S.delete elem atK
--  insertWithoutElem ss = foldl (\sm' e -> SM.insert k e sm') ss withoutElem

-- relevant is identical in all cases
-- tryInsert differs in the pattern matched, and the vars used as keys in the index
-- reindex differs from the default in Falsy and Disjunctive, where there is a shortcut
-- recognition that no change is necessary

data RealClausePartition clause = RealClausePartition {
  name :: String,
  rcpIndices :: Maybe [(Index clause, Bool)] -> clause -> Maybe [Index clause],
  skipReindexForAssignment :: Maybe Bool -> Bool
}

instance Eq (RealClausePartition clause) where (==) = (==) `on` name
instance Ord (RealClausePartition clause) where compare = compare `on` name
instance Show (RealClausePartition clause) where show = show . name

indexAllVars :: (Clause clause) => clause -> Maybe [Index clause]
indexAllVars = Just . map fst . clauseToTuples

disjunctivePartition :: RealClausePartition clause
disjunctivePartition = RealClausePartition "disjunctivePartition" i s where
  i (Just ((v1,_):(v2,_):_)) _ = Just [v1, v2]
  i _ _ = Nothing
  s Nothing = True
  s _ = False

-- todo: remove this? In MiniSat there is only the watches, the undos, and the propQueue.
-- if we assume variables are unassigned in the reverse order of which they were assigned,
-- then we can make this a lot more efficient
unitPartition :: (Clause clause) => RealClausePartition clause
unitPartition = RealClausePartition "unitPartition" i s where
  i (Just [_]) = indexAllVars
  i _ = const Nothing
  s _ = False

-- todo: don't index on all vars - falsy and satisfied should only need to index on one variable - the one which changed its status
falsyPartition :: (Clause clause) => RealClausePartition clause
falsyPartition = RealClausePartition "falsyPartition" i s where
  i (Just []) = indexAllVars
  i _ = const Nothing
  s (Just _) = True
  s _ = False

satisfiedPartition :: (Clause clause) => RealClausePartition clause
satisfiedPartition = RealClausePartition "satisfiedPartition" i s where
  i Nothing = indexAllVars
  i _ = const Nothing
  s _ = False

--data ClauseIndex var clause = Falsy clause | ClauseIndex (S.Set clause) (SM.SetMap var clause)
--  deriving (Eq, Ord, Show)
--
--singleton :: (Clause var clause, Assignment var assignment) => assignment -> clause -> ClauseIndex var clause
--singleton agn c =
--  case watches agn c of
--    AlreadySatisfied -> mempty
--    Watches [] -> Falsy c
--    Watches [(v,_)] -> ClauseIndex (S.singleton c) SM.empty
--    Watches ((v1,_):(v2,_):_) -> ClauseIndex S.empty (SM.insert v1 c (SM.insert v2 c SM.empty))
--
--instance (Ord var, Ord clause) => Monoid (ClauseIndex var clause) where
--  mempty = ClauseIndex S.empty SM.empty
--
--  -- We assume that the clauses in each are disjoint. The behavior is undefined if there is overlap.
--  mappend f@(Falsy _) _ = f
--  mappend _ f@(Falsy _) = f
--  mappend (ClauseIndex unit1 index1) (ClauseIndex unit2 index2) = ClauseIndex unit' index' where
--    unit' = S.union unit1 unit2
--    index' = mappend index1 index2
--


{-

Here's a "debugging" strategy...
for every "implementation", I have a minimal "stupid" implementation.
The stupid implementations are difficult to get wrong, so I first develop using the stupid implementations.
Then, when I've confirmed that the implementation is way too slow, I develop a second implementation that
has exactly the same interface. I ALSO develop a third implementation that proxies to both of the
sub-implementations, and which optionally checks, at EVERY operation, that the two implementations remain
identical with every operation. Once I've confirmed that the smart implementation is identical in meaning
to the dumb implementation, I toggle the proxy to delegate only to the smart implementation.

ARGH... I really wish I had an IDE command for "jump to all implementations"
I guess I should rely on module hierarchies for that instead?

-}