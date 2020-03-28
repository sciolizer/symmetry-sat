{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Problem.Partitioned (
--  PartitionedProblem,
--  PP(..)
) where

import Debug.Trace

import Control.Lens
import Data.Bifunctor
import Data.List
import Data.Proxy

import Families.Assignment
import Families.Clauses
import Families.DedupingMap
import Families.Replica
import Problem

{-

newtype PartitionedProblem gvc aProblem = PartitionedProblem { partitions :: DedupingMap (Rep gvc) aProblem }

--deriving instance (Eq gvc, Eq (Rep gvc), Eq aProblem) => Eq (PartitionedProblem gvc aProblem)
--deriving instance (Ord gvc, Ord (Rep gvc), Ord aProblem) => Ord (PartitionedProblem gvc aProblem)
instance Eq (PartitionedProblem gvc aProblem) where (==) = undefined
instance Ord (PartitionedProblem gvc aProblem) where compare = undefined
deriving instance (PP gvc aProblem, Show aProblem) => Show (PartitionedProblem gvc aProblem)

{- Vars -}

newtype instance Vars (PartitionedProblem gvc ap) = PartitionedVars (PartitionedProblem gvc ap)

deriving instance (Eq gvc, Eq (Rep gvc), Eq aProblem) => Eq (Vars (PartitionedProblem gvc aProblem))
deriving instance (Ord gvc, Ord (Rep gvc), Ord aProblem) => Ord (Vars (PartitionedProblem gvc aProblem))
deriving instance (PP gvc aProblem, Show aProblem) => Show (Vars (PartitionedProblem gvc aProblem))

type instance Index (Vars (PartitionedProblem gvc ap)) = Index gvc
type instance IxValue (Vars (PartitionedProblem gvc ap)) = Bool
instance (PP gvc ap, At (Vars ap), IxValue (Vars ap) ~ Bool) => Ixed (Vars (PartitionedProblem gvc ap))
instance (PP gvc ap, At (Vars ap), IxValue (Vars ap) ~ Bool) => At (Vars (PartitionedProblem gvc ap)) where
  -- at :: Index gvc -> Lens' (Vars (PartitionedProblem gvc ap)) (Maybe Bool)
  at = atVPP

ppDMIso :: Iso' (PartitionedProblem gvc ap) (DedupingMap (Rep gvc) ap)
ppDMIso = iso (\(PartitionedProblem dm) -> dm) PartitionedProblem

dmIso :: Iso' (Vars (PartitionedProblem gvc ap)) (DedupingMap (Rep gvc) ap)
dmIso = iso (\(PartitionedVars (PartitionedProblem dm)) -> dm) (PartitionedVars . PartitionedProblem)

--atVPP :: forall gvc ap. (ReplicaClause gvc, Problem ap, Abs gvc ~ Index (Clzs ap), Eq (Vars ap), Eq (Clzs ap)) => Index gvc -> Lens' (Vars (PartitionedProblem gvc ap)) (Maybe Bool)
atVPP k = dmIso . dedupingMapVarsIso . at (replica, avar) where
  [(replica, avar)] = abstractions k
  p = Proxy :: Proxy gvc

-- instance (Ord rep, Show rep, Ord (Vars (DedupingMap rep ap)), Show (Vars (DedupingMap rep ap)), Problem ap) => Assignment (Vars (DedupingMap rep ap)) where
-- maybe this Ord constraint is the source of my problem???
instance (PP gvc ap, Show (Vars (DedupingMap (Rep (Index gvc)) ap)), Eq (Vars ap)) => Assignment (Vars (PartitionedProblem gvc ap)) where
  --   assignmentEntries :: IndexedFold (Index agn) agn Bool
  assignmentEntries = aeVPP -- assignmentLens . ifolded

aeVPP :: forall gvc ap. (PP gvc ap, Show (Vars (DedupingMap (Rep (Index gvc)) ap)), Eq (Vars ap), Eq (Clzs ap)) => IndexedFold (Index gvc) (Vars (PartitionedProblem gvc ap)) Bool
aeVPP = aeVPP1 .> reindexed (remap (Proxy :: Proxy gvc) (Proxy :: Proxy ap)) (aes' (Proxy :: Proxy gvc)) where

aes' :: (PP gvc ap) => Proxy gvc -> IndexedFold (Rep (Index gvc), Index (Vars ap)) (Vars (DedupingMap (Rep (Index gvc)) ap)) Bool
aes' _ = assignmentEntries

remap :: (PP gvc ap) => Proxy gvc -> Proxy ap -> (Rep gvc, Index (Vars ap)) -> Index gvc
remap _ _ (rep, av) = instantiate rep av

aeVPP1 :: (PP gvc ap) => Fold (Vars (PartitionedProblem gvc ap)) (Vars (DedupingMap (Rep gvc) ap))
aeVPP1 = dmIso . dedupingMapVarsIso

class (ReplicaClause gvc, Problem ap, Abs gvc ~ Index (Clzs ap), Abs (Index gvc) ~ Index (Vars ap), Eq (Vars ap), Eq (Clzs ap)) => PP gvc ap where

{- Clzs -}

newtype instance Clzs (PartitionedProblem gvc ap) = PartitionedClzs (PartitionedProblem gvc ap)

deriving instance (Eq gvc, Eq (Rep gvc), Eq aProblem) => Eq (Clzs (PartitionedProblem gvc aProblem))
deriving instance (Ord gvc, Ord (Rep gvc), Ord aProblem) => Ord (Clzs (PartitionedProblem gvc aProblem))
deriving instance (PP gvc aProblem, Show aProblem) => Show (Clzs (PartitionedProblem gvc aProblem))

type instance Index (Clzs (PartitionedProblem gvc ap)) = gvc
type instance IxValue (Clzs (PartitionedProblem gvc ap)) = ()
instance (ReplicaClause gvc, Abs gvc ~ Index (Clzs ap), Problem ap, Eq (Clzs ap)) => Ixed (Clzs (PartitionedProblem gvc ap))
instance (ReplicaClause gvc, Abs gvc ~ Index (Clzs ap), Problem ap, Eq (Clzs ap)) => At (Clzs (PartitionedProblem gvc ap)) where
  at = clzsPartAtAll

-- bug is here: when adding a clause, we should add to ALL replicas, not just the one to which the replica belongs
-- this is still a lens (it satisfies the laws)... even though a single set is adding "multiple"
-- clauses, this is really just ONE clause that is indexed multiple times
-- we should drop the totalIx, ignore the rep, and use dedupedTraversal instead

-- todo: use totalIx to "prime" the rep of each clause inserted
clzsPartAtAll :: forall gvc ap. (Show ap, ReplicaClause gvc, Abs gvc ~ Index (Clzs ap), Problem ap, Eq (Clzs ap)) => gvc -> Lens' (Clzs (PartitionedProblem gvc ap)) (Maybe ())
--clzsPartAtAll gvc = clzPartsIso . primeKey p rep . mapPartAt p avc where
clzsPartAtAll gvc = clzPartsIso . {- primeKey p rep . -} dedupingMapClzsIso . at avc where
  [(_, avc)] = abstractions gvc
  p = Proxy :: Proxy gvc

--primeKey :: (Problem ap, Ord (Rep gvc)) => Proxy gvc -> Rep gvc -> Iso' (DedupingMap (Rep gvc) ap) (DedupingMap (Rep gvc) ap)
--primeKey _ rep = iso prime prime where
--  prime = primeRep rep -- over (totalIx rep) id

--mapPartAt :: forall gvc ap repgvc. (Show ap, ReplicaClause gvc, Abs gvc ~ Index (Clzs ap), Problem ap, repgvc ~ Rep gvc
--  ) => Proxy gvc -> Index (Clzs ap) -> Lens' (DedupingMap repgvc ap) (Maybe ())
--mapPartAt p ac = lens g s where
--  g mp = getOne p ac (mp :: DedupingMap repgvc ap)
--  s _ Nothing = error "removing duplicated clauses is not currently supported"
--  s mp (Just()) = trace ("mp size: " ++ show (lengthOf dedupedTraversal mp)) $ insertAll p ac (mp :: DedupingMap repgvc ap)

--getOne :: (Ord (Rep gvc), Problem ap, Show (Rep gvc), Show ap) => Proxy gvc -> Index (Clzs ap) -> DedupingMap (Rep gvc) ap -> Maybe ()
--getOne _ ac mp = answer where
--  allVals = mp ^.. {- taking 1 -} dedupedTraversal -- . clzAt ac -- . to (:[])
--  answer =
--    case allVals of
--      [] -> error "dedupedTraversal was empty, even though its set should always have at least one element (the empty assignment)"
--      xx@(x:xs)
--        | clausesAreIdentical x xs -> x ^. clzAt ac -- todo: remove this check for efficiency once we've confirmed it works
--        | otherwise -> error $ "assignments fell out of sync with each other. Clause " ++ show ac ++ " is present in some replicas and missing from others. Keys: " ++ show (mp ^.. dmFold . asIndex) ++ ", values: " ++ show (map (\z -> (assignmentToMap (z ^. assignment), sort $ z ^.. allClauses)) xx) ++ ", dedupingMap: " ++ show mp


-- somehow the no-assignment abstract problem is LOSING it's learned clause. It learns it at some point, but then forgets
-- about it after moving into the next replica. Also, the first variable in the second replica is being decided
-- even though it should be implied by the learned unit clause

clausesAreIdentical :: (Problem ap) => ap -> [ap] -> Bool
clausesAreIdentical x xs = all (identicalClauses x) xs

identicalClauses :: (Problem ap) => ap -> ap -> Bool
identicalClauses p1 p2 = sort (p1 ^.. allClauses) == sort (p2 ^.. allClauses)

--insertAll :: (Problem ap, Show (Rep gvc), Ord (Rep gvc)) => Proxy gvc -> Index (Clzs ap) -> DedupingMap (Rep gvc) ap -> DedupingMap (Rep gvc) ap
--insertAll _ ac before = trace ("insertAll before: " ++ show before ++ ", after: " ++ show after) after where
--  after = over dedupedTraversal insertClause before
--  insertClause = set (clzAt ac) (Just ())

clzAt :: (Problem ap) => Index (Clzs ap) -> Lens' ap (Maybe ())
clzAt avc = clzs . at avc

instance
  ( PP gvc ap
  , Ord ap, Show ap
  , Eq (Clzs ap)
  ) => Clauses (Clzs (PartitionedProblem gvc ap)) where
--  clauses :: Fold clauses (Index clauses)
  clauses = error "why do you need to enumerate the clauses instead of just going through partitions?" -- folding getClauses

clzPartsIso :: Iso' (Clzs (PartitionedProblem gvc ap)) (DedupingMap (Rep gvc) ap)
clzPartsIso = iso down up where
  down (PartitionedClzs (PartitionedProblem parts)) = parts
  up = PartitionedClzs . PartitionedProblem

{- Problem -}

instance (Problem ap, Eq ap) => AsEmpty (PartitionedProblem gvc ap) where
  _Empty = empty'

empty' :: (Problem ap, Eq ap) => Prism' (PartitionedProblem gvc ap) ()
empty' = prism s g where
  s () = PartitionedProblem Empty
  g (PartitionedProblem Empty) = Right ()
  g z = Left z

-- previously needed Ord constraint because DedupingMap was keying off of the entire problem.
-- that should no longer be necessary, now that we've switched DedupingMap over to keying by assignments only
instance
  ( PP gvarClause aProblem
  , Ord aProblem, Show aProblem
  {- , Ord (Vars (DedupingMap (Rep (Index gvarClause)) aProblem)) -}, Show (Vars (DedupingMap (Rep (Index gvarClause)) aProblem))
  ) => Problem (PartitionedProblem gvarClause aProblem) where
--    clzs :: Iso' problem (Clzs problem)
  clzs = iso PartitionedClzs (\(PartitionedClzs z) -> z)
--    assignment :: Iso' problem (Vars problem)
  assignment = iso PartitionedVars (\(PartitionedVars z) -> z)

--  unassigned :: Fold problem (Index (Vars problem))
  unassigned = ppUnassigned -- delegateInst unassigned -- ppDMIso . dmFold . unassigned
--  unassigned = folding unassigned'
--
--  falsyClauses :: Fold problem (Index (Clzs problem))
  falsyClauses = ppFalsyClauses -- delegateInst falsyClauses
--
--  unitClauses :: Fold problem (Index (Clzs problem), Index (Vars problem), Bool)
  unitClauses = ppUnitClauses
--
--  allClauses :: Fold problem (Index (Clzs problem))
--  allClauses = delegateInst allClauses

ppUnassigned :: (PP gc ap) => Fold (PartitionedProblem gc ap) (Index gc)
ppUnassigned = showIt "ppDMIso" . ppDMIso . showIt "dmUnassigned" . dmUnassigned . showIt "withIndex" . withIndex . showIt "uncurryInstantiate" . to (uncurry instantiate)

showIt prefix = to (\x -> trace (prefix ++ ": " ++ show x) x)

ppUnitClauses :: (PP gc ap) => Fold (PartitionedProblem gc ap) (gc, Index gc, Bool)
ppUnitClauses = ppDMIso . dmUnitClauses . withIndex . to inst where
  inst (rep, (gc, var, b)) = (instantiate rep gc, instantiate rep var, b)

delegate :: (PP gc ap) => Fold ap z -> Fold (PartitionedProblem gc ap) (Rep gc, z)
delegate target = undefined -- (ppDMIso . dmFold <. target) . withIndex

delegateInst :: (PP gc ap, Replica z, Rep gc ~ Rep z) => Fold ap (Abs z) -> Fold (PartitionedProblem gc ap) z
delegateInst target = delegate target . to (uncurry instantiate)

-- of course, only at the very last error would I spot the original reason for the elegance of making
-- absProblem accessible as a problem...
-- you can't talk about falsy/unit unless assignments and clauses are combined.
-- if we try restrict DedupignMap access so that assignments and clauses are always accessed independently,
-- then we'll need to create extra "side channels" within DedupingMap for falsy/unit etc

-- if dmFold had somehow traversed ALL of the absProblems, instead of only the ones keyed by the current
-- assignments (i.e. not including the empty assignment one unless an empty assignment replica existed),
-- then a unit clause which was actually satisfied could have been incorrectly returned.
-- it does look like dmFold probably traversed only the absProblems which were keyed, but perhaps the
-- recombination with the replica id was done improperly, and so things go jumbled up somehow

ppFalsyClauses :: (PP gc ap) => Fold (PartitionedProblem gc ap) gc
ppFalsyClauses = ppDMIso . dmFalsyClauses . withIndex . to (uncurry instantiate)

-}