{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Enlarger (
  enlarge,
  slightEnlarge,
  tracingSlightEnlarge,
  randomSingletonProblem,
  Replacement(..)
) where

-- ok, the enlargement rules here are useful for making sure that no ADDITIONAL solutions are introduced,
-- but they are not enough to make sure that the original solution is not removed. We must repeatedly
-- check the original solution as clauses are added to ensure this property is preserved

import Control.Lens
import Control.Monad.Random
import Control.Monad.RWS
import Data.Functor.Identity
import qualified Data.Heap as H
import qualified Data.Set as S

import Families.Assignment
import Families.Clauses

type Literal var = (var, Bool)

data Replacement clause = Replacement clause [clause]
  deriving (Show)

negated :: Literal var -> Literal var
negated (v, b) = (v, not b)

-- todo: check for dupes in the incoming var list
randomSingletonProblem
  :: (Clause clause, Assignment assignment, AsEmpty assignment, MonadRandom m, Index clause ~ Index assignment)
  => (Index clause -> Bool -> assignment -> assignment)
  -> [Index clause]
  -> m (assignment, [clause])
randomSingletonProblem uncheckedAssign = mapAccumM randomSingletonClause Empty where
  randomSingletonClause agn var = do
    b <- getRandomR (False, True)
    return (uncheckedAssign var b agn, singletonClause var b)

mapAccumM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumM f = loop where
  loop acc [] = return (acc, [])
  loop acc (x:xs) = do
    (acc', x') <- f acc x
    (acc'', xs') <- loop acc' xs
    return (acc'', x' : xs')

enlarge :: (Clause clause, MonadRandom m, Assignment assignment, At assignment, IxValue assignment ~ Bool, Index clause ~ Index assignment) => assignment -> [clause] -> m [clause]
enlarge agn clauses = slightEnlarge agn (round (targetRatio * fromIntegral (S.size vars))) clauses where
  vars = allVars clauses

allVars :: (Foldable f, Clause clause) => f clause -> S.Set (Index clause)
allVars = S.fromList . concatMap z where
  z clause = clause ^.. varsInClause

slightEnlarge :: (Clause clause, MonadRandom m, Assignment assignment, At assignment, IxValue assignment ~ Bool, Index clause ~ Index assignment) => assignment -> Int -> [clause] -> m [clause]
slightEnlarge agn targetNumClauses clauses = snd <$> tracingSlightEnlarge agn targetNumClauses clauses

tracingSlightEnlarge :: (Clause clause, MonadRandom m, Assignment assignment, At assignment, IxValue assignment ~ Bool, Index clause ~ Index assignment) => assignment -> Int -> [clause] -> m ([Replacement clause], [clause])
tracingSlightEnlarge agn targetNumClauses clauses = answer where
  vars = allVars clauses
  heap = H.fromList [(S.size (allVars (Identity clause)), clause) | clause <- clauses]
  agn' v =
    case agn ^. at v of
      Nothing -> error "provided assignment is incomplete"
      Just z -> z
  answer = do
    (replacements, heap) <- execEnlargerT enlarge' targetNumClauses vars agn' heap
    return (replacements, map snd . H.toList $ heap)

enlarge' :: (EnlargerMonad var clause m) => m ()
enlarge' = do
  b <- sufficientClauses
  if b then return () else do
    c <- removeSmallestClause
    case c ^.. literals . withIndex of
      [lit] -> enlargeUnit c lit >> enlarge'
      [lit1, lit2] -> enlarge2CNF c lit1 lit2 >> enlarge'
      [lit1, lit2, lit3] -> enlarge3CNF c lit1 lit2 lit3 >> enlarge'
      z -> error $ "unsupported clause length: " ++ show (length z)

enlargeUnit :: (EnlargerMonad var clause m) => clause -> Literal var -> m ()
enlargeUnit original lit = do
  b <- bridgeLiteral [lit]
  succ <- tryReplace original [
    [lit, b],
    [lit, negated b]]
  if succ then return () else enlargeUnit original lit

enlarge2CNF :: (EnlargerMonad var clause m) => clause -> Literal var -> Literal var -> m ()
enlarge2CNF original lit1 lit2 = do
  b <- bridgeLiteral [lit1, lit2]
  succ <- tryReplace original [
    [lit1, lit2, b],
    [lit1, lit2, negated b]]
  if succ then return () else enlarge2CNF original lit1 lit2

enlarge3CNF :: (EnlargerMonad var clause m) => clause -> Literal var -> Literal var -> Literal var -> m ()
enlarge3CNF original lit1 lit2 lit3 = do
  a <- bridgeLiteral [lit1, lit2, lit3]
  b <- bridgeLiteral [lit1, lit2, lit3, a]
  c <- bridgeLiteral [a, b, lit3]
  succ <- tryReplace original [
    [lit1, lit2, a],
    [negated a, lit3, c],
    [lit1, lit2, b],
    [negated b, lit3, negated c]]
  if succ then return () else enlarge3CNF original lit1 lit2 lit3

class (Monad m, Clause clause, var ~ Index clause) => EnlargerMonad var clause m | m -> clause, m -> var where
  sufficientClauses :: m Bool
  removeSmallestClause :: m clause
--  add :: [Literal var] -> m ()
  tryReplace :: clause -> [[Literal var]] -> m Bool
  bridgeLiteral :: [Literal var] -> m (Literal var)

data EnlargerContext var = EnlargerContext {
  requiredClauses :: Int,
  variables :: S.Set var,
  originalAssignment :: var -> Bool
}

data EnlargerState clause = EnlargerState {
  clauseHeap :: H.MinPrioHeap Int clause
}

newtype EnlargerT var clause m a = EnlargerT { unEnlargerT :: RWST (EnlargerContext var) [Replacement clause] (EnlargerState clause) m a }
  deriving (Functor, Applicative, Monad)

execEnlargerT :: (Monad m) => EnlargerT var clause m a -> Int -> S.Set var -> (var -> Bool) -> H.MinPrioHeap Int clause -> m ([Replacement clause], H.MinPrioHeap Int clause)
execEnlargerT (EnlargerT m) targetNumClauses vars oa clausesHeap = do
  (_, EnlargerState ch, w) <- runRWST m (EnlargerContext targetNumClauses vars oa) (EnlargerState clausesHeap)
  return (w, ch)

targetRatio :: (Floating a) => a
targetRatio = 4.3

instance (MonadRandom m, Clause clause, var ~ Index clause) => EnlargerMonad var clause (EnlargerT var clause m) where
  sufficientClauses = EnlargerT $ do
    required <- asks requiredClauses
    current <- gets (H.size . clauseHeap)
    return (current >= required)
  removeSmallestClause = EnlargerT $ do
    s <- get
    let ([(_, clause)], h') = H.splitAt 1 (clauseHeap s)
    put (s { clauseHeap = h' })
    return clause
  tryReplace original replacement = do
    agn <- EnlargerT $ asks originalAssignment
    if anyViolate replacement agn then return False else do
      mapM_ add replacement
      EnlargerT $ do
        tell [Replacement original (map tuplesToClause replacement)]
        return True
  bridgeLiteral exclusions = answer where
    answer = EnlargerT $ do
      vars <- asks variables
      let usableVars = S.difference vars . S.fromList . map fst $ exclusions
      let max = S.size usableVars
      when (max < 1) $ error "no usable vars"
      i <- getRandomR (0, max - 1)
      let var = S.elemAt i usableVars
      b <- getRandomR (False, True)
      return $ (var, b)



--add :: [Literal var] -> EnlargerT var clause ()
add literals = EnlargerT $ do
  modify (\s -> s { clauseHeap = H.insert (length literals, tuplesToClause literals) (clauseHeap s)})

anyViolate :: [[Literal var]] -> (var -> Bool) -> Bool
anyViolate clauses agn = any (violate agn) clauses

violate :: (var -> Bool) -> [Literal var] -> Bool
violate agn = all disagree where
  disagree (v, b) = b /= agn v

-- ok, rethinking this: the supply of variables is completely unnecessary... bridge variables can have ANY
-- assignment, so let's just reuse the variables from the original problem!
-- the size of the problem can be made arbitrarily large just be the size of the initial assignment...
-- we probably don't need TWO knobs for adjusting the size