{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
module PsuedoBoolean (
  Constraint(..),
  Literal,
  Relation(..),
  ConstraintAccumulator,
  exactlyOneOf,
  addConstraint,
  horn,
  assert,
  comment,
  execConstraintAccumulator
) where

import Control.Monad.Writer

type Literal var = (Int, var)
data Constraint var = Constraint [Literal var] Relation Int
  deriving (Eq, Ord, Foldable, Functor, Traversable)

instance (Show var) => Show (Constraint var) where
  show (Constraint lits rel i) = concatMap ((++ " ") . showLit) lits ++ show rel ++ " " ++ signedShow i ++ ";" where
    showLit (i, v) = signedShow i ++ "*" ++ show v

signedShow :: Int -> String
signedShow x
  | x > 0 = "+" ++ show x
  | x < 0 = show x
  | otherwise = error "0 coefficient"

data Relation = GTE | E
  deriving (Eq, Ord, Read, Bounded, Enum)

instance Show Relation where
  show GTE = ">="
  show E = "="

type ConstraintAccumulator var a = Writer [Either String (Constraint var)] a

execConstraintAccumulator :: ConstraintAccumulator var a -> [Either String (Constraint var)]
execConstraintAccumulator = execWriter

exactlyOneOf :: [var] -> ConstraintAccumulator var ()
exactlyOneOf vars = addConstraint (map (1,) vars) E 1

addConstraint :: [Literal var] -> Relation -> Int -> ConstraintAccumulator var ()
addConstraint lits rel num = tell . (:[]) . Right $ Constraint lits rel num

horn :: var -> [var] -> ConstraintAccumulator var ()
horn true falses = addConstraint ((1, true) : [(-1, false) | false <- falses]) GTE (1 - length falses)

assert :: var -> ConstraintAccumulator var ()
assert v = addConstraint [(1, v)] GTE 1

comment :: String -> ConstraintAccumulator var ()
comment = tell . (:[]) . Left

