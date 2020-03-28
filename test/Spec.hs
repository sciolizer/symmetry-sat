import Control.Monad
import Control.Monad.Random
import Control.Monad.Writer
import Data.Functor.Identity
import Data.List
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Set as S
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Random

import Dimacs
import Enlarger
import Gen.Circuit
import Gen.CNFReduce
import Families.Clauses
import Families.MonadNotify
import Problem.NaiveProblem
import Families.Search
import Hamiltonian.Reduce
import ProblemReplicator

{-
Problem: satisfiable problems are being reported as unsatisfiable.
Possible explanations:
  - MonadSprout implementation (FamilySolverMonadT) is buggy
  - the core search algorithm is incomplete (prunes or skips the solution)
  - Problem implementation (NaiveProblem) is buggy

The core search algorithm has been tested many times, and NaiveProblem is probably bug free because of its simplicity.
Most likely the bug is in FamilySolverMonadT.

-}

main :: IO ()
main = do
--  quickCheck propRandomSingletonProblemIsSatisfiable
--  putStrLn "^propRandomSingletonProblemIsSatisfiable"
--  quickCheck propRandomSingletonProblemHasOneSolution
--  putStrLn "^propRandomSingletonProblemHasOneSolution"
  quickCheck propRandomEnlargedSingletonProblemIsSatisfiable
  putStrLn "^propRandomEnlargedSingletonProblemIsSatisfiable"
  quickCheck propRandomEnlargedSingletonProblemHasOneSolution
  putStrLn "^propRandomEnlargedSingletonProblemHasOneSolution"
--  quickCheck propReplicasPreserveSatisfiability
--  putStrLn "^propReplicasPreserveSatisfiability"
--  quickCheck propPropagate
--  putStrLn "^propPropagate"
  quickCheckWith stdArgs { maxSuccess = 500 } propSatisfiable
  putStrLn "^propSatisfiable"
--  quickCheck propUnsatisfiable
--  putStrLn "^propUnsatisfiable"
--  quickCheck propCompactingFindsSatisfiable
--  putStrLn "^propCompactingFindsSatisfiable"

--propCompactingFindsSatisfiable :: SatisfiableEnlargedSingletonProblem -> Int -> Bool
--propCompactingFindsSatisfiable (SatisfiableEnlargedSingletonProblem (EnlargedSingletonProblem _ clauses) _) replicas =
--  let cls = replicateProblem replicas clauses in
--  case searchCompacting cls of
--    Nothing -> error "compacting search said no solution"
--    Just sol -> if C.satisfiesAll sol cls then True else error $ "found invalid solution: " ++ show sol

-- type Rand g = RandT g Identity
-- runRand :: Rand g a -> g -> (a, g)
-- randomSingletonProblem :: (Clause var clause, Assignment var assignment, MonadRandom m) => [var] -> m (assignment, [clause])
propRandomSingletonProblemIsSatisfiable :: SingletonProblem -> Bool
--propRandomSingletonProblemIsSatisfiable gen numVars | numVars < 1 = True
propRandomSingletonProblemIsSatisfiable (SingletonProblem assignment clauses) = satisfiesAll assignment clauses

propRandomSingletonProblemHasOneSolution :: SingletonProblem -> Bool
propRandomSingletonProblemHasOneSolution (SingletonProblem assignment clauses) = hasOneSolution assignment clauses

propRandomEnlargedSingletonProblemIsSatisfiable :: SatisfiableEnlargedSingletonProblem -> Bool
propRandomEnlargedSingletonProblemIsSatisfiable (SatisfiableEnlargedSingletonProblem (EnlargedSingletonProblem solution clauses) _) = satisfiesAll solution clauses

propRandomEnlargedSingletonProblemHasOneSolution :: EnlargedSingletonProblem -> Bool
propRandomEnlargedSingletonProblemHasOneSolution (EnlargedSingletonProblem solution clauses) = hasOneSolution solution clauses

negated (v, b) = (v, not b)

hasOneSolution :: M.Map Int Bool -> [M.Map Int Bool] -> Bool
hasOneSolution assignment clauses = answer where
  prohibitAssignment = tuplesToClause . map negated . M.toList {- . trace ("assignment: " ++ show assignment) -} $ assignment
  s :: [M.Map Int Bool] -> Maybe (M.Map Int Bool)
  s = runIdentity . searchMap (Proxy :: Proxy (NaiveProblem C))
  answer =
    case s ({- trace ("prohibitAssignment: " ++ show prohibitAssignment) -} prohibitAssignment : clauses) of
      Nothing -> True
      Just z -> error $ "unexpected solution: " ++ show z


data EnlargedSingletonProblem = EnlargedSingletonProblem (M.Map Int Bool) [M.Map Int Bool]
  deriving (Show)

-- enlarge :: (Clause var clause, MonadRandom m) => [clause] -> m [clause]

instance Arbitrary EnlargedSingletonProblem where
  arbitrary = do
    SingletonProblem assignment initialClauses <- arbitrary `suchThat` (\(SingletonProblem a _) -> M.size a >= 6)
--    targetNumClauses <- (+ (length initialClauses)) . abs <$> arbitrary
    let targetNumClauses = round . (* 4.3) . fromIntegral . length $ initialClauses
    gen <- arbitrary
    let clauses = fst . runRand (slightEnlarge assignment targetNumClauses initialClauses) $ (gen :: QCGen)
    return $ EnlargedSingletonProblem assignment clauses

data SatisfiableEnlargedSingletonProblem = SatisfiableEnlargedSingletonProblem EnlargedSingletonProblem [Replacement (M.Map Int Bool)]
  deriving (Show)

instance Arbitrary SatisfiableEnlargedSingletonProblem where
  arbitrary =  do
    SingletonProblem assignment initialClauses <- arbitrary `suchThat` (\(SingletonProblem a _) -> M.size a >= 6)
    targetNumClauses <- (+ (length initialClauses)) . abs <$> arbitrary
    gen <- arbitrary
    let (replacements, clauses) = fst . runRand (tracingSlightEnlarge assignment targetNumClauses initialClauses) $ (gen :: QCGen)
    return $ SatisfiableEnlargedSingletonProblem (EnlargedSingletonProblem assignment clauses) replacements
  shrink (SatisfiableEnlargedSingletonProblem (EnlargedSingletonProblem assignment clauses) rep) =
    [SatisfiableEnlargedSingletonProblem (EnlargedSingletonProblem assignment (delete c clauses)) rep | c <- clauses]

data SingletonProblem = SingletonProblem (M.Map Int Bool) [M.Map Int Bool]
  deriving (Show)

instance Arbitrary SingletonProblem where
  arbitrary = do
    numVars <- (+1) . abs <$> arbitrary
    qcgen <- arbitrary
    let (assignment, clauses) = genProblem qcgen numVars
    return $ SingletonProblem assignment clauses

genProblem :: QCGen -> Int -> (M.Map Int Bool, [M.Map Int Bool])
genProblem gen numVars = fst . runRand (randomSingletonProblem M.insert [1..numVars]) $ gen
-- randomSingletonProblem
   --  :: (Clause clause, Assignment assignment, AsEmpty assignment, MonadRandom m, Index clause ~ Index assignment)
   --  => (Index clause -> Bool -> assignment -> assignment)
   --  -> [Index clause]
   --  -> m (assignment, [clause])

--  quickCheckWith stdArgs { maxSuccess = 20, maxSize = 10 } propHamiltonian
--  putStrLn "^propHamiltonian"

propReplicasPreserveSatisfiability :: (Int, Int) -> Property
propReplicasPreserveSatisfiability (inputs, gates) = monadicIO $ do
  b <- run (ioReplicasPreserveSatisfiability (inputs, gates))
  assert b

ioReplicasPreserveSatisfiability :: (Int, Int) -> IO Bool
ioReplicasPreserveSatisfiability (inputs, gates)
  | inputs <= 1 || gates <= 1 = return True -- todo generate better test cases
ioReplicasPreserveSatisfiability (inputs, gates) = undefined {- do
  c <- newCircuit inputs gates
  let one = replicateGate 1 c
  let two = replicateGate 2 c
  let problem = map makeGround . toParametricClauses :: RepManyOneCircuit (ReplicaId, Int) -> [Clause (Groupless (ReplicaId, Int)) () (ReplicaId, Int)]
  let (problemOne, problemTwo) = (problem one, problem two)
  case (search problemOne, search problemTwo) of
    (Nothing, Nothing) -> return True
    (Just _, Just _) -> return True
    _ -> do
      putStrLn "one replica"
      putStrLn (showDimacs problemOne)
      putStrLn "two replicas"
      putStrLn (showDimacs problemTwo)
      return False -}

-- todo: this only checks that no superfluous constraints are added
-- we still need to slve the hamiltonian problem to check that all necessary constraints are added
-- Satisfiable (fromList [(v3,False),(v4,True),(v5,True),(v7,True)]) [v3+v4]
-- violates generated constraint: ~v5+~v7
--propHamiltonian :: Satisfiable -> Bool
--propHamiltonian (Satisfiable sol cls) | M.size sol < 3 || null cls = True -- todo generate better test cases
--propHamiltonian (Satisfiable sol cls) = answer where
----  (mp, cls') = reduceToHamiltonianAndBack cls
--  cls' = reduceToHamiltonianAndBack cls
--  answer = True -- trace ("length constraints: (" ++ show (M.size sol) ++ "," ++ show (length cls) ++ ") => " ++ (id cls')) True
----    case find (\c -> relevant c && not (satisfies sol c)) cls' of
----      Nothing -> trace ("length constraints: (" ++ show (M.size sol) ++ "," ++ show (length cls) ++ ") => " ++ show (length cls')) True
----      Just c -> error $ show mp ++ "\nviolates generated constraint: " ++ show c
--  relevant c = all (flip M.member sol) (clauseVars c)

--propPropagate :: Satisfiable -> Property
--propPropagate (Satisfiable _ cls) = official === naivePropagate M.empty (map literals cls) where
--  official =
--    case propagate (newPartialAssignment cls) of
--      Left _ -> Nothing
--      Right r -> Just (asMap r)

{-
propReplicasPreserveSatisfiability (inputs, gates) = monadicIO $ do
  b <- run (ioReplicasPreserveSatisfiability (inputs, gates))
  assert b
-}


{-

Satisfiable (fromList [(3,True),(6,False),(17,True)]) [fromList [(3,True),(6,True)],fromList [(3,True),(17,True)],fromList [(6,False),(17,False)]]
["decide 3 False","Implied 6 True","Implied 17 True","learned new clause: fromList [(3,True)]","fail sprout: fromList [(3,True)]","fail sprout: fromList [(6,False),(17,False)]"]

ok, that looks very much like variable assignments are not being rewound... or perhaps the unit propagations are not being
enqueued

-}


propSatisfiable :: Satisfiable -> Property
propSatisfiable (Satisfiable _ cls) =
  case runLogging $ searchMap (Proxy :: Proxy (NaiveProblem C)) cls of
    (Nothing, ss) -> counterexample (show ss) False
    (Just sol, ss) ->
      case satisfiesAll sol cls of
        True -> label "propSatisfiable" True
        False -> counterexample (show ss) False
--  b <- run ({- putStrLn "next problem" >> -} search cls)
--  case b of
--    Nothing -> error "search found no solution"
--    Just sol -> assert (all (satisfies sol) cls || error ("search found invalid solution: " ++ show sol))

propUnsatisfiable :: Unsatisfiable -> Bool
propUnsatisfiable (Unsatisfiable cls) =
  case runIdentity $ searchMap (Proxy :: Proxy (NaiveProblem C))cls of
    Nothing -> True
    Just sol -> error $ "found invalid solution: " -- ++ show sol

cToMap :: C -> M.Map Var Bool
cToMap = id -- clauseToMap
--cToMap c = answer where
--  lits = literals c
--  answer = M.fromList . map mkTuple $ lits
--  mkTuple lit = (var lit, sign lit)

type Var = Int
type C = M.Map Int Bool -- Clause (Groupless Var) () Var

data Satisfiable = Satisfiable (M.Map Var Bool) [C]
  deriving (Show)

instance Arbitrary Satisfiable where
  arbitrary = do
    numVars <- (+1) . abs <$> arbitrary
    let vars = [1..numVars]
    solution <- M.fromList . zip vars <$> replicateM numVars arbitrary
    numClauses <- abs <$> arbitrary
    clauses <- replicateM numClauses (arbitraryClauseSatisfiedBy solution)
    return (Satisfiable solution clauses)
  shrink s@(Satisfiable sol cls) = map dropVar (M.keys sol) where -- todo: ++ map (Satisfiable sol) (shrinkList (:[]) cls) where
    dropVar v = Satisfiable (M.delete v sol) (filter (not . hasVar v) cls)
    hasVar v cls = S.member v (clauseVars cls)

clauseVars :: C -> S.Set Var
clauseVars = M.keysSet

arbitraryClauseSatisfiedBy :: M.Map Var Bool -> Gen C
arbitraryClauseSatisfiedBy sol = do
  let choices = M.keys sol
  numLiteralsMinusOne <- abs <$> arbitrary
  randomLiterals <- replicateM numLiteralsMinusOne (arbitraryLiteral choices)
  satisfiableVar <- elements choices
  case M.lookup satisfiableVar sol of
    Nothing -> error "M.keys lied!"
    Just v ->
      case simplifyGrounded (randomLiterals ++ [(satisfiableVar, v)]) of
        Nothing -> arbitraryClauseSatisfiedBy sol -- todo: something more efficient
        Just r -> return r

simplifyGrounded :: (Ord gvar) => [(gvar, Bool)] -> Maybe (M.Map gvar Bool)
simplifyGrounded lits = if length lits == M.size m then Just m else Nothing where
  m = M.fromList lits

arbitraryLiteral :: [Var] -> Gen (Var, Bool)
arbitraryLiteral vars = (,) <$> elements vars <*> arbitrary

data Unsatisfiable = Unsatisfiable [C]
  deriving (Show)

instance Arbitrary Unsatisfiable where
  arbitrary = do
    Satisfiable agns cls <- arbitrary
    let maxVar = maximum . M.keys $ agns
    unsatCoreSize <- min 5 . (+1) . round . logBase 2 . abs . asTypeOf (1 :: Float) <$> arbitrary
    let unsatVars = [maxVar+1..maxVar+unsatCoreSize]
    let unsatClauses = genUnsatClauses (map (\v -> ((v, True), (v, False))) unsatVars)
    return $ Unsatisfiable (cls ++ map tuplesToClause unsatClauses)

makePos :: Int -> Int
makePos = (+1)

genUnsatClauses :: [(a,a)] -> [[a]]
genUnsatClauses [] = [[]]
genUnsatClauses ((l,r):xs) = let ch = genUnsatClauses xs in map (l:) ch ++ map (r:) ch


