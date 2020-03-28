{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Families.Search (
  searchMap
) where

import Control.Lens
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Proxy

import Clause.ClauseIndex
import Families.Assignment
import Families.Clauses
import Families.DedupingMap
import Families.FamilySolver
import Families.IntraRepClause
import Families.MonadNotify
import Families.Replica
import Problem
import Problem.NaiveProblem
import Problem.Partitioned

searchGeneric :: (Problem p, Show p, Foldable f, MonadNotify m) => p -> (Solution p -> a) -> f (Clz p) -> m (Maybe a)
searchGeneric unconstrainedProblem assignmentMapper clauses = fmap assignmentMapper <$> search unconstrainedProblem clauses

searchMap :: (Problem p, Show p, Foldable f, MonadNotify m) => p -> f (Clz p) -> m (Maybe (M.Map (Var p) Bool))
searchMap unconstrainedProblem = searchGeneric unconstrainedProblem  assignmentToMap

--instance (Ord rep, Show rep, Ord avar, Show avar) => PP (IntraRepClause rep (M.Map avar Bool)) (NaiveProblem (M.Map avar Bool))
--
--instance (Ord rep, Show rep, Clause ac) => PP (IntraRepClause rep ac) (IndexedProblem ac)

