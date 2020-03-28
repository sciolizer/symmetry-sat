{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Families.Replica (
  Replica(..),
  ReplicaClause(..)
) where

import Control.Lens
import qualified Data.Map.Strict as M

import Families.Clauses

class
  ( Ord ground, Show ground
  , Ord (Rep ground), Show (Rep ground)
  , Ord (Abs ground), Show (Abs ground)
  ) => Replica ground where
  type Rep ground :: *
  type Abs ground :: *

  -- todo: this is not right. It's an appropriate signature for variables, since variables
  -- can appear in multiple replicas, but it is not an appropriate signature for clauses,
  -- which will always belong to a single replica
  abstractions :: ground -> [(Rep ground, Abs ground)]
  instantiate :: Rep ground -> Abs ground -> ground -- todo this could be a Getter

instance (Show a, Show b, Ord a, Ord b) => Replica (a, b) where
  type Rep (a, b) = a
  type Abs (a, b) = b
  abstractions (a, b) = [(a, b)]
  instantiate a b = (a, b)

--instance (Ord rep, Ord avar) => Replica (M.Map (rep, avar) Bool) where
--  type Rep (M.Map (rep, avar) Bool) = rep
--  type Abs (M.Map (rep, avar) Bool) = M.Map avar Bool
--  abstractions = undefined
--  instantiate rep mp = M.mapKeys (rep,) mp

class
  ( Replica gClause
  , Clause gClause, Clause (Abs gClause)
  , Replica (Index gClause)
  , Rep gClause ~ Rep (Index gClause)
  , Index (Abs gClause) ~ Abs (Index gClause)
  ) => ReplicaClause gClause