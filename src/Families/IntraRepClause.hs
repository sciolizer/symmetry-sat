{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Families.IntraRepClause (
  IntraRepClause
) where

import Control.Lens
import qualified Data.Map.Strict as M

import Families.Clauses
import Families.Replica

-- A clause which exists within a single replica and which
-- will never resolve with clauses from other replicas
data IntraRepClause rep ac = IntraRepClause rep ac
  deriving (Eq, Ord, Show, Functor)

type instance Index (IntraRepClause rep ac) = (rep, Index ac)
type instance IxValue (IntraRepClause rep ac) = Bool
instance (Ord rep, Show rep, Ord ac, Show ac) => Replica (IntraRepClause rep ac) where
  type Rep (IntraRepClause rep ac) = rep
  type Abs (IntraRepClause rep ac) = ac
--  abstractions :: ground -> [(Rep ground, Abs ground)]
  abstractions (IntraRepClause rep mp) = [(rep, mp)]
--  instantiate :: Rep ground -> Abs ground -> ground
  instantiate = IntraRepClause

--class (Ord (Index clause), Show (Index clause), At clause, IxValue clause ~ Bool) => Clause clause where
instance AsEmpty (IntraRepClause rep ac) where
  _Empty = undefined -- could be fixed by adding a Contradictory constructor

innerClauseFold :: IndexedFold rep (IntraRepClause rep ac) ac
innerClauseFold = innerClauseFold'

-- class Indexable rep p where indexed :: p ac (f ac) -> rep -> ac -> (f ac)
-- type IndexedFold i s a = forall p f. (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s
innerClauseFold' :: (Indexable rep p, Contravariant f, Applicative f) => p ac (f ac) -> (IntraRepClause rep ac) -> f (IntraRepClause rep ac)
innerClauseFold' pacfac irc@(IntraRepClause rep ac) = IntraRepClause rep <$> indexed pacfac rep ac

instance
  ( Ord rep, Show rep
  , Clause ac
  ) => Clause (IntraRepClause rep ac) where
--  literals :: IndexedFold (rep, Index ac) (IntraRepClause rep ac) Bool
  literals = innerClauseFold <.> literals -- repGetter . literals
--  resolve :: Index clause -> clause -> clause -> Maybe clause
  resolve (rep, avar) (IntraRepClause rep1 mp1) (IntraRepClause rep2 mp2)
    | rep /= rep1 || rep /= rep2 = Nothing
    | otherwise = IntraRepClause rep <$> resolve avar mp1 mp2

instance (Ord rep, Show rep, Clause ac) => ReplicaClause (IntraRepClause rep ac)

instance (Eq rep, Clause ac) => Ixed (IntraRepClause rep ac) where
instance (Eq rep, Clause ac) => At (IntraRepClause rep ac) where
  at (rep, avar) = lens g s where
    g (IntraRepClause rep' mp)
      | rep == rep' = mp ^. at avar
      | otherwise = Nothing
    s rc@(IntraRepClause rep' mp) Nothing
      | rep == rep' = IntraRepClause rep (set (at avar) Nothing mp)
      | otherwise = rc
    s rc@(IntraRepClause rep' mp) (Just v)
      | rep == rep' = IntraRepClause rep (set (at avar) (Just v) mp)
      | otherwise = error "writing literals from different replicas into ReplClause not currently supported"

