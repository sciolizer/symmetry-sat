{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Families.Clauses (
  Clause(..),
  Clauses(..),
  varsInClause,
  varsInClauses,
  singletonClause,
  tuplesToClause,
  satisfiesAll,
  satisfies,
  reduceClause,
  clauseToTuples,
  PPMap(..),
  AssignmentLookup
) where

import Control.Lens
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Families.Assignment

class
  ( Ord clause, Show clause
  , Ord (Index clause), Show (Index clause)
  , At clause
  , IxValue clause ~ Bool
  , AsEmpty clause
  ) => Clause clause where
  -- actually this could probably be a Traversal or IndexedTraversal
  literals :: IndexedFold (Index clause) clause Bool
  resolve :: Index clause -> clause -> clause -> Maybe clause

varsInClause :: (Clause clause) => Fold clause (Index clause)
varsInClause = literals . asIndex

instance (Ord var, Show var) => Clause (M.Map var Bool) where
  literals = ifolded
  resolve var c1 c2 = answer where
    vars = S.delete var (S.union (M.keysSet c1) (M.keysSet c2))
    allMergeable = all mergeable vars
    mergeable v =
      case (M.lookup v c1, M.lookup v c2) of
        (Just True, Just False) -> False
        (Just False, Just True) -> False
        _ -> True
    opposite = not . mergeable
    merged = M.union (M.delete var c1) (M.delete var c2)
    answer = if opposite var && allMergeable then Just merged else Nothing

class
  ( Ord (Index clauses), Show (Index clauses), Clause (Index clauses)
  , {- Ord clauses, -} Show clauses
  , At clauses, IxValue clauses ~ ())
--  AsEmpty clauses)
  => Clauses clauses where
  clauses :: Fold clauses (Index clauses)

varsInClauses :: (Clauses clauses) => Fold clauses (Index (Index clauses))
varsInClauses = clauses . varsInClause

singletonClause :: (Clause clause) => Index clause -> Bool -> clause
singletonClause v b = set (at v) (Just b) Empty

tuplesToClause :: (Clause clause) => [(Index clause, Bool)] -> clause
tuplesToClause = foldl add Empty where
  add c (v, b) = over (at v) (mod b) c
  mod b Nothing = Just b
  mod b (Just _) = error "duplicate var in tuples"

type AssignmentLookup clause = Index clause -> Maybe Bool

satisfiesAll :: (Clause clause) => AssignmentLookup clause -> [clause] -> Bool
satisfiesAll agn = all (satisfies agn)

satisfies :: (Clause clause) => (Index clause -> Maybe Bool) -> clause -> Bool
satisfies agn cl = reduceClause agn cl == Nothing

reduceClause :: (Clause clause) => (Index clause -> Maybe Bool) -> clause -> Maybe clause
reduceClause agn cl = loop cl (cl ^@.. literals) where
  loop c [] = Just c
  loop c ((v,b):vs) =
    case agn v of
      Nothing -> loop c vs
      Just b'
        | b == b' -> Nothing
        | otherwise -> loop (sans v c) vs

clauseToTuples :: (Clause clause) => clause -> [(Index clause, Bool)]
clauseToTuples c = c ^.. literals . withIndex

--type instance Index (S.Set clause) = clause
--type instance IxValue (Clzs (NaiveProblem clause)) = ()
--instance (Ord clause) => Ixed (Clzs (NaiveProblem clause))
--instance (Ord clause) => At (Clzs (NaiveProblem clause)) where
--  at k = clausesLens . at k

instance (
  Ord (Index clause), Show (Index clause),
  Ord clause, Show clause, Clause clause)
  => Clauses (S.Set clause) where
  clauses = folded

newtype PPMap var = PPMap { unPPMap :: M.Map var Bool }
  deriving (Eq, Ord)

ppMapIso :: Iso' (PPMap var) (M.Map var Bool)
ppMapIso = iso (\(PPMap v) -> v) PPMap

type instance Index (PPMap var) = var
type instance IxValue (PPMap var) = Bool

instance (Ord var) => Ixed (PPMap var) where
instance (Ord var) => At (PPMap var) where
  at k = ppMapIso . at k
instance AsEmpty (PPMap var) where
  _Empty = ppMapIso . _Empty
instance (Ord var, Show var) => Clause (PPMap var) where
--  literals :: IndexedFold (Index clause) clause Bool
  literals = ppMapIso . literals
--  resolve :: Index clause -> clause -> clause -> Maybe clause
  resolve var (PPMap c1) (PPMap c2) = PPMap <$> resolve var c1 c2

instance (Show var) => Show (PPMap var) where
  show (PPMap mp) = intercalate "+" . map showLit . M.toList $ mp where
    showLit (v, True) = " " ++ show v
    showLit (v, False) = "~" ++ show v