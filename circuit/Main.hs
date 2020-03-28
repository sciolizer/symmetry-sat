module Main where

import Control.Monad.Random
import System.Environment
import System.TimeIt

import Dimacs
import Gen.Circuit
import Gen.CNFReduce

-- performance remains abysmall.
-- fix on 10 inputs, 1000 gates, scale up the number of replicas, time grows exponentially
-- it works well on a single replica but fails to generalize to multiple replicas
-- possible explanations:
--   variable selection is scattered across replicas instead of staying local to replica.... can check with some logging of variable ids
--   overhead of learned constraints is much too high... can check by logging how many watched literals trigger uselessly (no propagation) and how many trigger redundantly (propagtion of already assigned variable)
-- in general is probably worthwhile to add instrumentation throughout - tracking the stats that minisat does, such
--   as number of learnt clauses per second, number of decision variables per second, number of unit propagations per second, etc
--   don't need to push everything through to the IO monad... can just make a custom monad (transformer) that has some
--   'emit' funciton for tracking events... if implementation of the monad has IO underneath, then it can time the computations
--   and periodically print them out

-- really should add a quickcheck test confirming that random circuits can be both satisfiable and unsatisfiable

-- ok, yet another new plan
-- let's just generate random hard SAT problems (4.3 3CNF clauses per variable)
-- and then duplicate them many times
-- the problems don't even need to overlap in anyway
-- unrealistic, but if my solver can't win at an unfair game, then it certainly can't win at a fair games

main :: IO ()
main = undefined {- do
  [replicas, inputs, gates] <- map read <$> getArgs
  circuit <- genCircuit replicas inputs gates
--  putStrLn (pbReduce circuit)
  let cls = toParametricClauses circuit
--  let groundCls = map makeGround cls :: [Clause (Groupless Int) () Int]
  putStrLn (showDimacs cls) -- groundCls)
  putStrLn $ show (length cls) ++ " clauses" -- 'length' forces a parse to the end so we can start timing things
  r <- timeIt $ do
    case search cls of
      Nothing -> putStrLn "UNSAT" >> return Nothing
      Just ans -> putStr "SAT: " >> return (Just ans)
  case r of
    Just sol -> print $ all (satisfies sol) cls
    Nothing -> return ()

-}