{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gen.CNFReduce (
  toParametricClauses
) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import Data.Traversable

import Families.Clauses
import Families.Replica
import Gen.Circuit hiding (ManyOneCircuit)

type Constraint var = [(var, Bool)]

type ConstraintAccumulator var a = Writer [Constraint var] a

type GVar = (ReplicaId, Int)

type ManyOneCircuit = RepManyOneCircuit (ReplicaId, Int)

newtype Replica = Replica ReplicaId
  deriving (Eq, Ord, Show)

toParametricClauses :: ManyOneCircuit -> a -- [Clause Replica Int GVar]
toParametricClauses circuit = undefined {- grounded ++ parametric where -- todo: the generated clauses should be PARAMETERIC
  constraints = execWriter (pbReduce' circuit)
  (ungroupable, grouped) = groupConstraints id constraints
  grounded = map mkGrounded ungroupable
  mkGrounded = fromWellFormedGrounded . map (uncurry Literal)
  mkParametric constraint replicaIds = fromWellFormedParametric constraint (map Replica replicaIds)
  parametric = concatMap myMkParametric . M.toList $ grouped
  myMkParametric (constraint, ids) = fromWellFormedParametric constraint (adjust ids)
  adjust :: [ReplicaId] -> [Replica]
  adjust = map Replica
  numReplicaIds = length . nub . concatMap (map (fst . fst)) $ constraints

groupConstraints :: (Eq k, Ord v, Show var) => (var -> (k, v)) -> [Constraint var] -> ([Constraint var], M.Map (Constraint v) [k])
groupConstraints f = second (M.unionsWith (++)) . partitionEithers . map (groupConstraint f) where

groupConstraint :: (Eq k, Show var) => (var -> (k, v)) -> Constraint var -> Either (Constraint var) (M.Map (Constraint v) [k])
groupConstraint f lits = answer where
  ks = map (fst . f . fst) lits
  kHead = head ks
  lits' = map (\(v, b) -> (snd . f $ v, b)) lits
  answer =
    case all (== kHead) ks of
      True -> Right . M.singleton lits' $ [kHead]
      False -> Left lits

pbReduce' :: ManyOneCircuit -> ConstraintAccumulator GVar ()
pbReduce' (_ , output, gates) = do
  assert $ output
  mapM_ addGate gates

addGate :: (Show var) => OGate var var -> ConstraintAccumulator var ()
addGate gate = do
  case {- traceShowId $ -} gate of
    And is o -> do -- a /\ b = c
      horn o is -- c <= a /\ b
      sequence_ [horn i [o] | i <- is] -- a <= c /\ b <= c
    Or is o -> do -- a \/ b = c
      constrain $ (o, False) : [(i, True) | i <- is]-- c => a \/ b === ~c \/ a \/ b
      sequence_ [horn o [i] | i <- is] -- c <= a /\ c <= b
    Not i o -> exactlyOneOf i o

constrain :: Constraint var -> ConstraintAccumulator var ()
constrain = tell . (:[])

assert :: var -> ConstraintAccumulator var ()
assert = constrain . (:[]) . (, True)

horn :: var -> [var] -> ConstraintAccumulator var ()
horn true falses = constrain ((true, True) : [(false, False) | false <- falses])

exactlyOneOf :: var -> var -> ConstraintAccumulator var ()
exactlyOneOf v1 v2 = do
  constrain [(v1, True), (v2, True)]
  constrain [(v1, False), (v2, False)]

-}