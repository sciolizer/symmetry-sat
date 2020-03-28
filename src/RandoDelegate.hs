module RandoDelegate (
  main
) where

import Control.Lens
import Control.Monad.Random
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Proxy
import System.Environment
import System.IO
import System.TimeIt

import Clause.ClauseIndex
import Dimacs
import Enlarger
import Families.Clauses
import Families.IntraRepClause
import Families.Search
import Problem.Partitioned
import ProblemReplicator

main :: IO ()
main = do
  [replicas, replicaSize] <- map read <$> getArgs
  (assignment, simpleReplica) <- rsp [1..replicaSize]
--  putStr "assignment: " >> print assignment
  enlargedReplica <- enlarge assignment simpleReplica
  if not $ satisfiesAll (\k -> M.lookup k assignment) enlargedReplica then error "enlarged replica not satisfies" else return ()
  let cls' = replicateProblem replicas enlargedReplica
--  if not $ satisfiesAll assignment cls then error "replicated enlarged replica not satisfies" else return ()
  showDimacsCl cls'
  let cls = fmap (fmap PPMap) cls'
--  print cls
  putStrLn $ show (length cls) ++ " clauses" -- 'length' forces a parse to the end so we can start timing things
  hFlush stdout
  r <- timeIt $ do
--    res <- searchMap (Proxy :: Proxy (PartitionedProblem (IntraRepClause rep ac) (IndexedProblem ac))) cls
    res <- searchMap (newIndexedProblem (cls ^.. folded . varsInClause)) cls
    case res of
      Nothing -> putStrLn "UNSAT" >> return Nothing
      Just ans -> putStr "SAT: " >> return (Just ans)
--  print r
  case r of
    Just sol -> print $ satisfiesAll (\k -> M.lookup k sol) cls
    Nothing -> return ()

rsp :: [Int] -> IO (M.Map Int Bool, [M.Map Int Bool])
rsp = randomSingletonProblem {- M.empty -} M.insert
-- randomSingletonProblem
   --  :: (Clause clause, Assignment assignment, AsEmpty assignment, MonadRandom m, Index clause ~ Index assignment)
   --  => (Index clause -> Bool -> assignment -> assignment)
   --  -> [Index clause]
   --  -> m (assignment, [clause])

--toTupleClause :: RepClause Int (M.Map Int Bool) -> ReplClause Int Int
--toTupleClause (RepClause rep clause) = ReplClause rep clause

-- compiles, but incorrectly reports some generated problems as unsatisfiable