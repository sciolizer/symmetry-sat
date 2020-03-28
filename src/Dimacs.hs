{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Dimacs (
  parseDimacs,
  showDimacsCl
) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import qualified Data.Map.Strict as M

import Families.Clauses
import Families.IntraRepClause
import Families.Replica

parseDimacs :: (Clause clause, Index clause ~ Int) => String -> [clause]
parseDimacs = map toClause . filter isInteresting. lines

isInteresting ('c':_) = False
isInteresting ('p':_) = False
isInteresting _ = True

toClause :: (Clause clause, Index clause ~ Int) => String -> clause
toClause = tuplesToClause . map (intToLiteral . read) . init . words where
  intToLiteral x | x > 0 = (x, True)
                 | x < 0 = ((negate x), False)
                 | otherwise = error "unexpected 0"

class (Monad m) => MonadPrinter m where
  putLn :: String -> m ()

instance MonadPrinter IO where
  putLn = putStrLn

toList :: (Clause clause) => clause -> [(Index clause, Bool)]
toList = clauseToTuples

showDimacsCl :: (Clause clause, MonadPrinter m) => [clause] -> m ()
--showDimacsCl clauses = unlines . (header:) . map showCommOrCon $ constraints where
showDimacsCl clauses = proc where
  (constraints, varCount) = intifyAll . map toList $ clauses
  showCommOrCon = (++ " 0") . intercalate " " . map showBySign
  showBySign (i, True) = show i
  showBySign (i, False) = show (negate i)
  header = "p cnf " ++ show varCount ++ " " ++ show (length constraints)
  proc = do
    putLn header
    mapM_ (putLn . showCommOrCon) constraints

intifyAll :: (Ord var) => [[(var, Bool)]] -> ([[(Int, Bool)]], Int)
intifyAll cs = (cs', nextId - 1) where
  (cs', (_, nextId)) = runState (traverse (traverse (\(v, b) -> (,b) <$> intify v)) cs) (M.empty, 1)
--  goodInitialOrdering = M.fromList . zip (nub (sort (concatMap (map fst) cs))) $ [1..]

type IntSupply var a = State (M.Map var Int, Int) a

intify :: (Ord var) => var -> IntSupply var Int
intify k = do
  (mp, !nextId) <- get
  case M.lookup k mp of
    Just v -> return v
    Nothing -> do
      put (M.insert k nextId mp, nextId + 1)
      return nextId

