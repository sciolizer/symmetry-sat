{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Families.TemplatedProblem (

) where

import Control.Lens
import qualified Data.Map as M

import Families.Assignment
import Families.Clauses
import Families.Replica
import Problem

{-

-- constraints will always be disjoint...
-- variables will of course overlap
-- not enough to map variables to [(rep, av)] -- now we need TemplateId also
-- now... if all of the constraints being added to the original problem HAVE
-- a templateId

data TemplateId tid = TemplateId tid | GlobalTemplate
  deriving (Eq, Ord, Read, Show)

-- this actually throws into question the entire "newProblem" definition
-- ideally... we know the entire mapping between variables and templateIds+replicaIds ahead of time
--

class (Clause c) => TemplateClause c where
  type TID c :: *
  type Ground c :: *
  templateId :: c -> TemplateId (TID c)
  ground :: c -> [Ground c]
  fdjakl :: Index c -> Index (Ground c)

data TemplatedProblem problem c = TemplatedProblem {
  global :: problem (Ground c), -- has all assignments, but with zero constraints (initially, some learned constraints will be added here)
  byTemplate :: M.Map (TID c) (TemplateWithInstantiations (Rep gc) ap)
}

deriving instance (Show (Rep gc), Show ap, Show (Var ap)) => Show (TemplatedProblem gc ap)

type Agn ap = M.Map (Var ap) Bool
type Instantiations ap = M.Map (Agn ap) ap -- todo: clean up periodically

data TemplateWithInstantiations rep ap = TemplateWithInstantiations {
  assignmentsByReplica :: M.Map rep (Agn ap),
  instantiations :: Instantiations ap
}

deriving instance (Show rep, Show ap, Show (Var ap)) => Show (TemplateWithInstantiations rep ap)

instance (
--  Ord (Index gc), Ord gc,
--  Show (Index gc), Show gc,
  Problem ap,
  TemplatedClause gc,
  Show (Rep gc))
  => Problem (TemplatedProblem gc ap) where
  type Var (TemplatedProblem gc ap) = Index gc
  type Clz (TemplatedProblem gc ap) = gc

--  pickUnassigned :: PickOne problem (Var problem)
--  pickFalsyClz :: PickOne problem (Clz problem)
--  pickUnitClz :: PickOne problem (Clz problem, Var problem, Bool)
--
--  assignments :: IndexedFold (Var problem) problem VarAssignment
--
--  newProblem :: (Foldable f) => f (Var problem) -> problem
  newProblem vars = TemplatedProblem (newProblem avars) M.empty where

--
--  variable :: Var problem -> Lens' problem VarAssignment
--  relevantVariable :: Var problem -> problem -> Bool
--
--  -- must throw an error if clause contains any currently irrelevant variables (if fullyRelevnt c == False)
--  clause :: Clz problem -> Lens' problem ClzPresence

-}