{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module ProblemReplicator (
  replicateProblem
) where

import Control.Arrow
import Control.Monad.Random

import Families.Clauses
import Families.IntraRepClause
import Families.Replica

--newtype ComponentGroup a = ComponentGroup Int
--  deriving (Eq, Ord, Show)

--data RepClause var clause = RepClause Int clause
--  deriving (Eq, Ord, Show)

--class Ord var => Clause var clause | clause -> var where
--instance (Ord var, Clause var clause) => Clause (Int, var) (IntraRepClause var clause) where
----  resolve :: var -> clause -> clause -> Maybe clause
--  resolve (repVar, var) (RepClause rep1 c1) (RepClause rep2 c2)
--    | repVar == rep1 && repVar == rep2 = RepClause repVar <$> resolve var c1 c2
--    | otherwise = error "current prototype shouldn't even try to resolve separate replica clauses"
----  toList :: clause -> [(var, Bool)]
--  toList (RepClause rep c) = map (first (rep,)) (toList c)
----  toClause :: [(var, Bool)] -> clause
--  toClause lits = RepClause expectedRep . toClause . map stripRep $ lits where
--    stripRep ((rep, var), b)
--      | rep == expectedRep = (var, b)
--      | otherwise = error "cannot create RepClause from heterogenous lits"
--    expectedRep =
--      case lits of
--        [] -> error "cannot make RepClause out of contradiction clause"
--        (((rep, _), _):_) -> rep
--
--instance (Clause var clause) => Replica Int clause (RepClause var clause) where
--
----class (Replica replica avar gvar, Clause avar avarClause, Clause gvar gvarClause) => ReplicaClause replica avar gvar avarClause gvarClause | gvar -> avarClause, gvar -> replica, gvarClause -> replica where
--instance (Clause var clause) => ReplicaClause Int var (Int, var) clause (RepClause var clause) where
----  abstractClause :: gvarClause -> [(replica, avarClause)]
--  abstractClause (RepClause rep cl) = [(rep, cl)]
----  instantiateClause :: replica -> avarClause -> gvarClause
--  instantiateClause rep cl = RepClause rep cl

--instance Group ComponentGroup Int (ComponentGroup, Int) where
--  instantiate = (,)

replicateProblem :: (Clause clause) => Int -> [clause] -> [IntraRepClause Int clause]
replicateProblem replicas original = [instantiate i c | i <- [1..replicas], c <- original]
--  let groups = map ComponentGroup $ [1..replicas]
--  return . concatMap (flip fromWellFormedParametric groups) $ template

