{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- todo pretty sure switching this from append-only to counting made my program MUCH slower
-- benchmark this to make sure (ideally make swappable implementations)

module Families.DedupingMap (
--  DedupingMap,
--  dedupingMapVarsIso,
--  dedupingMapClzsIso,
----  primeRep,
--  dmFalsyClauses,
--  dmUnitClauses,
--  dmUnassigned
----  dedupedTraversal, -- used by getOne and insertAll (getOne is shortcut for reading)
----  dmFold, -- used by assignmentEntries, enumerating clauses, and delegating clause shortcuts (falsy, unit, etc)
) where

import Debug.Trace

import Control.Arrow
import Control.Lens
import Data.Function
import qualified Data.Map as M -- NOT strict! We want to be able to shortcut calculating clause indexes when they have already been built.
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S

import Families.Assignment
import Families.Clauses
import Problem

{-

type Agn ap = M.Map (Index (Vars ap)) Bool
type Counts ap = M.Map (Agn ap) (Int, ap)

-- I feel like the surface area of this module is much too large...
-- it seems the actual data structure should be reduced to something
-- which JUST does the counting (like I haven't done that several times already)
-- and then... whatever else is going on here can stay here
-- something like TripleMapChain may have been the right idea

--newtype DedupingMap rep ap = DedupingMap (TripleMapChain rep (Agn ap) ap)
data DedupingMap rep ap = DedupingMap {
  inner :: M.Map rep (Agn ap),
  representatives :: Counts ap
  -- we can get defaultValue from AsEmpty
}

deriving instance (Show rep, Show ap, Show (Index (Vars ap))) => Show (DedupingMap rep ap)

--instance (Show k, Show v, Ord k {- , AssignmentOrd v -}) => Show (DedupingMap k v) where
--  show (DedupingMap i r) = "showDedupingMap not implemented" {- innerStr ++ "\nin" ++ repsStr ++ "\nwith comparators " ++ altComparatorsStr where
--    ireps = M.toAscList r
--    one = 1 :: Int
--    int2vCount = M.fromList . map (\(n, (AltComparator v, (c, vv))) -> (n, (c, vv))) . zip [one..] $ ireps
--    v2Int = M.fromList . map (\(n, (_, vv)) -> (AltComparator vv, n)) . M.toList $ int2vCount
--    k2Int = M.fromList . map (\(k, v) -> (k, fromJust (M.lookup (AltComparator v) v2Int))) . M.toList $ i
--    innerStr = concatMap (\(k, n) -> "\n  " ++ show k ++ " => " ++ show n) . M.toAscList $ k2Int
--    repsStr = concatMap (\(n, (c, vv)) -> "\n  " ++ show n ++ " => " ++ show c ++ " of " ++ show vv) . M.toAscList $ int2vCount
--    altComparatorsStr = show . map snd . M.toList $ i -}

--dmFold :: (AssignmentOrd v) => IndexedFold k (DedupingMap k v) v
--dmFold = dmFold'
--
---- -- class Indexable i p where indexed :: p a b -> i -> a -> b
---- indexed pvfv :: k -> v -> f v
--dmFold' :: (Indexable k p, Contravariant f, Applicative f, AssignmentOrd v) => p v (f v) -> DedupingMap k v -> f (DedupingMap k v)
--dmFold' pvfv (DedupingMap m s) = DedupingMap <$> m' <*> pure s where
--  m' = sequenceA $ M.mapWithKey (\k v -> flip getOriginal s <$> indexed pvfv k v) m

-- throws an error if any new value does not alt-compare EQ with the old value.
-- i.e. this traversal is only for reads and clause inserts. It may not be used for
-- assignment changes. Additionally, the user of this traversal should make sure
-- that clause mutations are identical in all cases... there is no way to enforce
-- this afaik with the current types, unfortunately.

-- in order for dmFold to reflect changes made by dedupedTraversal, all users of dedupedTraversal
-- must prime the rep key, e.g. by over-ing the identity function for each key (totalIx)
--dedupedTraversal :: (AssignmentOrd v) => Traversal' (DedupingMap k v) v
--dedupedTraversal = dedupedTraversal'
--

---- mapMonotonic :: (a->b) -> Set a -> Set b
---- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
---- type Traversal' s a = Traversal s s a a
---- type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s
---- Traversal' (DedupingMap k v) v = forall f. Applicative f => (v -> f v) -> DedupingMap k v -> f (DedupingMap k v)
--dedupedTraversal' :: (AssignmentOrd v, Applicative f) => (v -> f v) -> DedupingMap k v -> f (DedupingMap k v)
--dedupedTraversal' vfv (DedupingMap i r) = DedupingMap i <$> fr where -- because inner is unchanged here, we must always use insertReturningOriginal when reading from it
--  rl = toFstPair . M.toList $ r
--  frl = (traverse (checkedVFV vfv) rl)
--  fr = M.fromList . fromFstPair <$> frl

--newtype FstPair a = FstPair { unFstPair :: [(a, Int)] }
--  deriving (Eq, Functor, Foldable, Traversable)

--toFstPair :: [(AltComparator a, (Int, a))] -> FstPair a
--toFstPair = FstPair . map unac where
--  unac (AltComparator _, (t, a)) = (a, t)
--
--fromFstPair :: FstPair a -> [(AltComparator a, (Int, a))]
--fromFstPair = map ac . unFstPair where
--  ac (a, t) = (AltComparator a, (t, a))

--unpair :: [(a, Int)] -> FstPair a
--unpair = FstPair uncurry FstPair -- (v, i) = FstPair v i

--checkedVFV :: (AssignmentOrd v, Applicative f) => (v -> f v) -> v -> f v
--checkedVFV vfv v = fv' where
--  fv = vfv v
--  fv' = (\vv -> if AltComparator vv == AltComparator v then vv else error "non-monotic deduped traversal") <$> fv
--  same = all (== AltComparator v) (fmap AltComparator fv)

newtype instance Vars (DedupingMap rep ap) = DedupingMapVars (DedupingMap rep ap)

deriving instance (Show rep, Show ap, Show (Index (Vars ap))) => Show (Vars (DedupingMap rep ap))

dedupingMapVarsIso :: Iso' (DedupingMap rep ap) (Vars (DedupingMap rep ap))
dedupingMapVarsIso = iso DedupingMapVars (\(DedupingMapVars dm) -> dm)

type instance Index (Vars (DedupingMap rep ap)) = (rep, Index (Vars ap))
type instance IxValue (Vars (DedupingMap k v)) = Bool

instance (Problem ap, At (Vars ap), IxValue (Vars ap) ~ Bool, Ord rep, Eq (Vars ap), Eq (Clzs ap)) => Ixed (Vars (DedupingMap rep ap))
instance (Problem ap, At (Vars ap), IxValue (Vars ap) ~ Bool, Ord rep, Eq (Vars ap), Eq (Clzs ap)) => At (Vars (DedupingMap rep ap)) where
  at = undefined -- atVarsDedupingMap

{-
atVarsDedupingMap :: (Problem ap, At (Vars ap), IxValue (Vars ap) ~ Bool, Ord rep, Eq (Vars ap), Eq (Clzs ap)) => (rep, Index (Vars ap)) -> Lens' (Vars (DedupingMap rep ap)) (Maybe Bool)
atVarsDedupingMap (rep, av) = lens g s where
  g vars = originalAP rep vars ^. assignment . at av where
  s vars@(DedupingMapVars (DedupingMap nr reps)) mbBool = DedupingMapVars (DedupingMap nr' reps') where
    originalAgn = case M.lookup rep nr of { Just agn -> agn; Nothing -> error "replica not initialized" }
    originalMBBool = originalAgn ^. at av
    newAgn = set (at av) mbBool originalAgn
    (nr', reps') = if mbBool == originalMBBool then (nr, reps) else (nr'', reps'')
    nr'' = update rep newAgn nr
    reps'' = aoSwap originalAgn newAgn changeAgn reps
    changeAgn = set (assignment . at av) mbBool

originalAP :: (Problem ap, Ord rep) => rep -> Vars (DedupingMap rep ap) -> ap
originalAP rep (DedupingMapVars (DedupingMap nr set)) = getOriginal v set where
  v =
    case M.lookup rep nr of
      Nothing -> error "attempted to query from non-existent replica. All replicas should be initialized by insertion of clauses."
      Just z -> z
-}

-- NOT making this an Assignment is, I think, part of the reason why we have PartitionedProblem as a wrapper
-- around DedupingMap???
--instance (Ord rep, Show rep, {- Ord (Vars (DedupingMap rep ap)), -} Show (Vars (DedupingMap rep ap)), Problem ap, Eq (Vars ap), Eq (Clzs ap)) => Assignment (Vars (DedupingMap rep ap)) where

instance (Ord rep, Show rep, Problem ap) => Assignment (Vars (DedupingMap rep ap)) where
  --   assignmentEntries :: IndexedFold (Index agn) agn Bool
  assignmentEntries = aeVDM

aeVDM :: forall rep ap. (Problem ap) => IndexedFold (rep, Index (Vars ap)) (Vars (DedupingMap rep ap)) Bool
aeVDM = aeVDM1 <.> aeVDM2 (Proxy :: Proxy ap)

innerLens :: Lens' (DedupingMap rep ap) (M.Map rep (Agn ap))
innerLens = lens g s where
  g (DedupingMap nr _) = nr
  s (DedupingMap nr counts) nr' = DedupingMap nr' counts

aeVDM1 :: (Problem ap) => IndexedFold rep (Vars (DedupingMap rep ap)) (Agn ap)
aeVDM1 = from dedupingMapVarsIso . innerLens .> ifolded

aeVDM2 :: (Problem ap) => Proxy ap -> IndexedFold (Index (Vars ap)) (Agn ap) Bool
aeVDM2 _ = ifolded

dmUnassigned :: (Show rep, Problem ap) => IndexedFold rep (DedupingMap rep ap) (Index (Vars ap))
dmUnassigned = undefined -- ifolding fuseDM <. unassigned

showIt prefix = to (\x -> trace (prefix ++ ": " ++ show x) x)

newtype instance Clzs (DedupingMap rep ap) = DedupingMapClzs (DedupingMap rep ap)

dedupingMapClzsIso :: Iso' (DedupingMap rep ap) (Clzs (DedupingMap rep ap))
dedupingMapClzsIso = iso DedupingMapClzs (\(DedupingMapClzs dm) -> dm)

type instance Index (Clzs (DedupingMap rep ap)) = Index (Clzs ap)
type instance IxValue (Clzs (DedupingMap rep ap)) = ()

instance (Problem ap, Eq (Clzs ap)) => Ixed (Clzs (DedupingMap rep ap))
instance (Problem ap, Eq (Clzs ap)) => At (Clzs (DedupingMap rep ap)) where
  at = atClzsDedupingMap

{-
atClzsDedupingMap :: (Problem ap, Eq (Clzs ap)) => Index (Clzs ap) -> Lens' (Clzs (DedupingMap rep ap)) (Maybe ())
atClzsDedupingMap ac = lens g s where
  g (DedupingMapClzs (DedupingMap _ reps)) = if allClausesSame reps then ans else error "counts inconsistent wrt clauses" where
    ans =
      case reps ^.. values . _2 . clzs . at ac of
        [] -> error "deduping map lacks a single count entry"
        (x:_) -> x
  s _ Nothing = error "removing clauses not currently supported"
  s dmc@(DedupingMapClzs (DedupingMap nr reps)) (Just ()) = if g dmc == Just () then dmc else dmc' where
    dmc' = DedupingMapClzs (DedupingMap nr reps')
    reps' = set (values . _2 . clzs . at ac) (Just ()) reps

values :: Traversal' (M.Map k v) v
values = traversed

-- todo: disable this obviously expensive check
allClausesSame :: (Problem ap, Eq (Clzs ap)) => Counts ap -> Bool
allClausesSame counts =
  case counts ^.. values . _2 . clzs of
    [] -> error "deduping map has not a single count entry"
    (x:xs) -> all (== x) xs
-}
-- this is super bad... if we need to prime reps, then I'd say this abstraction has completely failed
--primeRep :: (Ord rep) => rep -> DedupingMap rep ap -> DedupingMap rep ap
--primeRep rep (DedupingMap nr counts) = DedupingMap nr' counts where
--  nr' =
--    case M.lookup rep nr of
--      Nothing -> M.insert rep M.empty nr
--      Just _ -> nr

instance (Problem ap, Ord (Clzs (DedupingMap rep ap)), Show (Clzs (DedupingMap rep ap)), Eq (Clzs ap)) => Clauses (Clzs (DedupingMap rep ap)) where
--clauses :: Fold (Clzs (DedupingMap rep ap)) (Index clauses)
  clauses = clausesDM

clausesDM :: (Problem ap) => Fold (Clzs (DedupingMap rep ap)) (Index (Clzs ap))
--clausesDM = to (\(DedupingMapClzs (DedupingMap _ reps)) -> reps) . values . _2 . clzs . clauses
clausesDM = undefined -- urgh, this is not right.... ideally we just pull clauses from a single replica... of course this raises the question (again) as to why we're even walking the total collection of clauses to begin with. Shouldn't we always be accessing via the partitions?

{-

getOne :: (Ord (Rep gvc), Problem ap, Show (Rep gvc), Show ap) => Proxy gvc -> Index (Clzs ap) -> DedupingMap (Rep gvc) ap -> Maybe ()
getOne _ ac mp = answer where
  allVals = mp ^.. {- taking 1 -} dedupedTraversal -- . clzAt ac -- . to (:[])
  answer =
    case allVals of
      [] -> error "dedupedTraversal was empty, even though its set should always have at least one element (the empty assignment)"
      xx@(x:xs)
        | clausesAreIdentical x xs -> x ^. clzAt ac -- todo: remove this check for efficiency once we've confirmed it works
        | otherwise -> error $ "assignments fell out of sync with each other. Clause " ++ show ac ++ " is present in some replicas and missing from others. Keys: " ++ show (mp ^.. dmFold . asIndex) ++ ", values: " ++ show (map (\z -> (assignmentToMap (z ^. assignment), sort $ z ^.. allClauses)) xx) ++ ", dedupingMap: " ++ show mp


I really don't want to represent things as a map from replicas to problems...
I WANT to be able to treat the collection of clauses as its own isolated data structure.

I also want to completely disallow the insertion of aribrary absProblems... all manipulation of absProblems
should be done using "over" like semantics.

Maybe that is what Setter is for???
Kind of...
I want only a restricted set of manipulations to occur:
  adding clauses
  changing associations between replicas and assignments.
  All index updating should be handled INTERNALLY... we should not trust the consumer of the DedupingMap
  to be updating the indices... so the consumer should never have direct access to the absProblems.

we can in theory completely neglect variable assignment... though that increases the risk of inconsistency
the ONLY reason we are doing this partitioning is so that we can get fast lookup of learned unit/falsy clauses

what if, instead of a map from assignments to clause indices (where the clause indices are a set of maps from
variables to relevant clauses), we just had clause indices, but the indices were maps from possible variable
assignments to relevant clauses...

that is, rather than requiring clauses to exist in a single index depending on the current variable assignment,
we allowed variables to be in a super position of assignments?


HERE's an idea:  just change DedupingMap so that it is a Clauses instance, and that, as a Map/Index, it's keys
are (rep, avar), not rep alone. That should prevent any directly manipulation of the absProblems inside.
-}
--totalIxImpl :: (Ord k, AssignmentOrd v, Functor f, AsEmpty v) => k -> (v -> f v) -> DedupingMap k v -> f (DedupingMap k v)
--totalIxImpl k = lens g s where
--  g (DedupingMap m s) = v' where
--    v = fromMaybe Empty (M.lookup k m)
--    v' = getOriginal v s
--  s (DedupingMap m s) v = DedupingMap m' s' where
--    s' = decrementedOp . aoInsertAndIncrement v $ s -- must decrement AFTER incrementing to make sure nothing gets reset
--    m' = update k v m
--    decrementedOp =
--      case M.lookup k m of
--        Just prev -> aoDecrement prev
--        Nothing -> id

instance (AsEmpty v, Eq v, Eq (Index (Vars v))) => AsEmpty (DedupingMap k v) where
  _Empty = emptyDedupingMap

-- preview l (review l b) ≡ Just b
-- If preview l s ≡ Just a then review l a ≡ s
-- preview :: Getter s a  -> s -> Maybe a
-- review :: Prism' s a -> a -> s

-- obvious implementations for "setter"
-- everything maps to ()
-- singleton set maps to ()
-- singleton set with no clauses maps to ()
-- all of these satisfy the first law
-- the first violates the second law
-- the second might violate the second law depending on our choice of (==)... but we should probably assume
-- the strictest definition, and so only the 3rd satisfies both laws

emptyDedupingMap :: (AsEmpty v, Eq v, Eq (Index (Vars v))) => Prism' (DedupingMap k v) ()
emptyDedupingMap = prism bt seta where
  bt () = DedupingMap M.empty initialCounts
  initialCounts = M.singleton (M.empty) (1, Empty)
  seta dm@(DedupingMap nr counts)
    | M.null nr && counts == initialCounts = Right ()
    | otherwise = Left dm
--  seta dm@(DedupingMap _ s)
--    | toFstPair (M.toList s) == FstPair [(Empty, 1)] = Right ()
--    | otherwise = Left dm

instance (AsEmpty v, Eq v, Eq (Index (Vars v))) => AsEmpty (Vars (DedupingMap k v)) where
  _Empty = from dedupingMapVarsIso . _Empty

-- We don't need an At instance. The map is initialized with a default value
-- which is returned for new replicas.
-- Note that the default value can change, in the sense that it can accumulate
-- new clauses over time. It will always have an empty assignment, however.
--instance (Ord k, Ord v) => At (DedupingMap k v) where
--  at k = lens g s where
--    g (DedupingMap i _) = M.lookup k i
--    s (DedupingMap i d) Nothing = DedupingMap (M.delete k i) d -- todo: garbage collect when zero instances remain
--    s (DedupingMap i d) (Just v) = DedupingMap (M.insert k orig i) d' where
--      (orig, d') = insertReturningOriginal v d


-- most of these uses are probably wrong.
-- you almost never want to both insert and get the original
-- either you want to get the original, or you want to insert,
-- but not both
getOriginal :: (Ord (Index (Vars a))) => Agn a -> Counts a -> a
getOriginal elem set = answer where
  answer =
    case M.lookup elem set of
      Nothing -> error "corrupt DedupingMap"
      Just (_, orig) -> orig

aoInsertAndIncrement :: (Ord (Index (Vars a))) => Agn a -> a -> Counts a -> Counts a
aoInsertAndIncrement elem ap set =
--  let ac = AltComparator elem in
  case M.lookup elem set of
    Nothing -> M.insert elem (1, ap) set
    Just (!n, _) -> update elem (n + 1, ap) set

aoDecrement :: (Ord (Index (Vars a))) => Agn a -> Counts a -> Counts a
aoDecrement elem set =
--  let ac = AltComparator elem in
  case M.lookup elem set of
    Nothing -> error "decrement failed because DedupignMap is corrupt"
    Just (n, ap)
      | n < 1 -> error "somehow non positive number ended up in DedupingMap"
      | n == 1 -> M.delete elem set
      | otherwise -> update elem (n - 1, ap) set

-- todo: is there any point to this and not just using aoInsertandincrement and aoDecrement seperately?
aoSwap :: (Problem a, Ord (Index (Vars a)), Eq (Vars a), Eq (Clzs a)) => Agn a -> Agn a -> (a -> a) -> Counts a -> Counts a
aoSwap oldAgn newAgn _ _
  | oldAgn == newAgn = error "aoSwap called with identical assignments"
aoSwap oldAgn newAgn changeAP counts = if consistent then counts' else error "aoSwap provided invalid keys" where
  (oldN, oldAP) = case M.lookup oldAgn counts of { Nothing -> error "oldAgn not present"; Just z -> z }
--  (newN, newAP) =
  expectedOldAgn = assignmentToMap (oldAP ^. assignment)
  newAP = changeAP oldAP
  expectedNewAgn = assignmentToMap (newAP ^. assignment)
  consistent = oldAgn == expectedOldAgn && newAgn == expectedNewAgn
  counts' = decrement . increment $ counts
  decrement =
    case M.lookup oldAgn counts of
      Nothing -> error "aoSwap given old which was not present in map"
      Just (n, v)
        | n < 1 -> error "deduping map corrupt with non-positive number"
        | n == 1 -> M.delete oldAgn
        | otherwise -> update oldAgn (n - 1, v)
  increment =
    case M.lookup newAgn counts of
      Nothing -> M.insert newAgn (1, newAP)
      Just (!n, currentAP) -> update newAgn (n + 1, if eqProblem newAP currentAP then newAP else error "increment put in absProblem which differed from existing value")
--    case M.lookup oldAgn

eqProblem :: (Problem a, Eq (Vars a), Eq (Clzs a)) => a -> a -> Bool
eqProblem p1 p2 = (p1 ^. assignment) == (p2 ^. assignment) && (p1 ^. clzs) == (p2 ^. clzs)

{-
  | AltComparator old == AltComparator new = error "aoSwap called with absProblems identical by assignment"
aoSwap old new set =
  let acOld = AltComparator old
      acNew = AltComparator new
      decrement =
        case M.lookup acOld set of
          Nothing -> error "aoSwap given old which was not present in map"
          Just (n, v)
            | n < 1 -> error "deduping map corrupt with non-positive number"
            | n == 1 -> M.delete acOld
            | otherwise -> update acOld (n - 1, v)
      increment =
        case M.lookup acNew set of
          Nothing -> M.insert acNew (1, new)
          Just (n', _) -> update acNew (n' + 1, new)
  in decrement . increment $ set -}
--
--  case (M.lookup acOld set, M.lookup acNew set) of
--    (Nothing, _) -> error "aoSwap given old which was not present in map"
--    (Just (n, _), _)
--      | n < 1 -> error "deduping map corrupt with non-positive number"
--    (Just (n, v), Nothing)
--      | n == 1 -> M.delete acOld . M.insert acNew (1, new) $ set
--      | otherwise -> update acOld (n - 1, v) . M.insert acNew (1, new) $ set
--    (Just (n, v), Just (n', _))
--      | n == 1 -> M.delete acOld . update acNew (n' + 1, new) $ set
--      | otherwise -> update acOld (n - 1, v) . update acNew (n' + 1, new) $ set

-- alias for insert which checks to make sure that the key is already present
update :: (Ord k) => k -> v -> M.Map k v -> M.Map k v
update k v mp =
  case M.lookup k mp of
    Nothing -> error "attempted to update a non-existent entry in a map"
    Just _ -> M.insert k v mp

--instance (Problem problem) => AssignmentOrd problem where
--  compareAssignments = compare `on` mps where
--    mps p = assignmentToMap $ p ^. assignment

dmFalsyClauses :: (Show rep, Problem ap) => IndexedFold rep (DedupingMap rep ap) (Index (Clzs ap))
dmFalsyClauses = undefined -- ifolding fuseDM <. falsyClauses

fuseDM :: (Show rep, Show ap, Ord (Index (Vars ap))) => DedupingMap rep ap -> [(rep, ap)]
fuseDM (DedupingMap nr reps) = (\x -> trace ("fuseDM out: " ++ show x) x) . map fuse . M.toList $ nr where
  fuse (rep, agn) =
    case M.lookup agn reps of
      Nothing -> error "DedupingMap inconsistency; inner entry lacks corresponding count entry"
      Just (_, ap) -> (rep, ap)

dmUnitClauses :: (Show rep, Problem ap) => IndexedFold rep (DedupingMap rep ap) (Index (Clzs ap), Index (Vars ap), Bool)
dmUnitClauses = undefined -- ifolding fuseDM <. unitClauses

-- falsyClauses :: Fold problem (Index (Clzs problem))

-- and so on. Note that these could not be combined with Vars since the key for that
-- is (rep, avar), and here the (var) key is just avar

{-
data DedupingMap rep ap = DedupingMap {
  inner :: M.Map rep (Agn ap),
-}

-- the bug is maybe that DedupingMap inner is empty.. I think it should never be empty, but I'm not sure


{-
I've put all of this effort into making efficiently indexed data structures...
only to find out that there's something SERIOUSLY wrong with my algorithm, in that
it goes really slow on every odd-numbered replica.

Question: even if I had super slow data structures, could my algorithm still outperform the state of the
art with ONLY symmetric learning?

It sure seems that way... after the first replica, every TRUE assignment should be the result of a unit clause
propagation, and so the second and all subsequent replicas should be ENTIRELY unit propagations...
which could still be slow if we have no index... needing to scan for unit clauses for every propagation.


It occurs to me that there's a possible optimization not used by SAT solvers... any unit propagations
that occur at decision level 0 can be turned into their own learned clauses. Probably a pointless optimization
though.







Hm....
data DedupingMap rep ap = DedupingMap {
  inner :: M.Map rep (Agn ap),
  representatives :: Counts ap
  -- we can get defaultValue from AsEmpty
}

what if... perhaps naively... we just simplified this to
M.Map rep ap
?



The bug is clearly this:
when I'm adding clauses, the corresponding variables are not also being created.
Figuring out where the problem is is tricky because we have this stupid
separation between PartitionedProblem and DedupingMap.

So first...
Combine PartitionedProblem and DedupingMap into a single module.
Then (or maybe during the merging process)
figure out why adding clauses does not create variables


hang on... I guess the reason that PartitionedProblem is separated from DedupingMap is so that we can
have a place to put the constraints that don't belong anywhere...

-}



-}