{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Gen.Circuit (
  RepManyOneCircuit,
--  RepWire,
  ReplicaId(..),
  OGate(..),
  genCircuit,
  newCircuit,
  replicateGate
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Random
import Data.Foldable
import Data.Functor.Identity
import Data.List

-- switching from watching all literals to watching two literals produced basically no performance gain
-- graphing performance, it's unclear if pcnf will ever overtake minisat, but it's clear that if it will,
-- then it will do so on timescales that are far too long for my patience.

-- 100% of the circuits I've generated have been satisfiable... which suggests
-- so my solver must be made faster somehow.... though it's unclear to me where the best place to start is
-- probably something like VSIDS, but that's a MAJOR refactor


-- perhaps before
-- todo: MAJOR PROBLEM! For most of the problems generated here, my solver calls them satisfiable,
-- but minisat calls them unsatisfiable. My unit tests and ad-hoc experiments suggest my solver is
-- correct for grounded clauses, and so the bug is probably in the learning logic for parametric clauses.
-- Another possible place for a bug is inconsistency between the parametric representation and the grounded
-- representation... we should remove the short-cut reduction from circuits to grouned cnf and make the grounded
-- cnf representations come out of the parametric representations.
-- We can also compare the output of my solver on parametric with my solver on grounded, although the
-- performance of grounded is likely to blow up.
-- If the discrepency persists, then more rigorous testing of my solver is probably necessary (e.g. by running
-- on the benchmark problems).

-- generates a random circuit, which ANDs together the single output of a tiled circuit that
-- has three inputs.

type Wire = Int

data OGate i o
  = And [i] o
  | Or [i] o
  | Not i o
  deriving (Eq, Ord, Show, Foldable)

newtype IGate o i = IGate { oGate :: OGate i o }
  deriving (Eq, Ord, Show)

instance Foldable (IGate o) where
  foldr acc seed (IGate gate) = foldr acc seed list where
    list =
      case gate of
        And is _ -> is
        Or is _ -> is
        Not i _ -> [i]

newtype SGate w = SGate { unSGate :: OGate w w }
  deriving (Eq, Ord, Show)

instance Foldable SGate where
  foldr acc seed (SGate gate) = foldr acc seed list where
    list =
      case gate of
        And is o -> o : is
        Or is o -> o : is
        Not i o -> [i, o]

instance Functor SGate where
  fmap f (SGate gate) = SGate gate' where
    gate' =
      case gate of
        And is o -> And (fmap f is) (f o)
        Or is o -> Or (fmap f is) (f o)
        Not i o -> Not (f i) (f o)

type Gate = OGate Wire Wire

type RepManyOneCircuit wire = ([wire], wire, [OGate wire wire])
type ManyOneCircuit = RepManyOneCircuit Wire

data ReplicaId = ReplicaId Int | Common
  deriving (Eq, Ord, Show)

genCircuit :: Int -> Int -> Int -> IO (RepManyOneCircuit (ReplicaId, Int))
genCircuit replicas inputsPerReplica gatesPerReplica = do
  c <- newCircuit inputsPerReplica gatesPerReplica
  return (replicateGate replicas c)

--something :: [Wire] -> Int -> [RepWire Int]
--something wires = \subst -> map (fmap (const (subst))) wires

--something' :: Int -> Wire -> RepWire Int
----something' subst (sec, str) = (fmap (const subst) sec, str)
----something' subst (RepWire sec) = (fmap (const subst) sec, str)
--something' subst wire = fmap (const subst) wire

replicateGate :: Int -> ManyOneCircuit -> RepManyOneCircuit (ReplicaId, Int)
replicateGate nReplicas (is, o, gates) = (is', o', andGate : gates') where
  replicas = map ReplicaId [1..nReplicas]
--  duplicate replicaId = map (fmap (const replicaId))
  is' = concatMap (\subst -> map (subst,) is) replicas
  o' = (Common, 1)
  gates' = concatMap (\replica -> map (unSGate . fmap (replica,) . SGate) gates) replicas
  os = map (,o) replicas
  andGate = And os o'

--something :: [OGate Wire Wire] -> Int -> [OGate (RepWire Int) (RepWire Int)]
--something gates = \replica -> map (unSGate . fmap (fmap (const replica)) . SGate) gates

--something' :: Int -> OGate Wire Wire -> OGate (RepWire Int) (RepWire Int)
--something' replica = unSGate . fmap (fmap (const replica)) . SGate

--something'' :: Int -> Wire -> RepWire Int
--something'' i = fmap (const i)

newCircuit :: Int -> Int -> IO ManyOneCircuit
newCircuit nInputs gates = result where
  build = do
    inputs <- replicateM nInputs newReplicatedWire
    replicateM_ gates addRandomGate
    output <- close
    return (inputs, output)
  result = do
    stdGen <- getStdGen
--    let inputs = [1..inputs] -- map (RepWire (Replica ()) . ('v':) . show) [1..3]
    case runIdentity $ evalRandT (runStateT build (CircuitState [] [] 1)) stdGen of -- todo: still not sure I'm handling randomization correctly
      ((inputs, output), cs) -> return (inputs, output, csGates cs)

addRandomGate :: Circuit ()
addRandomGate = join . pickOne $ [randomAggregate And, randomAggregate Or, randomNot]

randomAggregate :: ([Wire] -> Wire -> Gate) -> Circuit ()
randomAggregate agg = do
  ws <- wires
  num <- pickOne [(min 3 (length ws))..(min 5 (length ws))]
  ws'<- pick num ws
  o <- newReplicatedWire
  add $ agg ws' o

randomNot :: Circuit ()
randomNot = do
  ws <- wires
  w <- pickOne ws
  o <- newReplicatedWire
  add $ Not w o

close :: Circuit Wire
close = do
  is <- unused
  o <- newWire
--  agg <- pickOne [And, Or]
--  add $ agg is o
  add $ And is o
  return o

unused :: Circuit [Wire]
unused = do
  allWs <- wires
  usedWs <- usedWires
  return . filter (flip notElem usedWs) $ allWs -- todo: more efficient

--instance Applicative Circuit where
--instance Monad Circuit where

usedWires :: Circuit [Wire]
usedWires = do
  gs <- map IGate <$> gates
  let inputWires = map toList (gs :: [IGate Wire Wire])
  return . nub . concat $ inputWires -- todo efficiency

newReplicatedWire :: Circuit Wire -- todo: remove
newReplicatedWire = newWire

{- *Hamiltonian.AppendOnlyBiMap Control.Monad.Random Control.Monad.Writer Control.Monad.State> :t \m -> runRandT (runStateT m ()) undefined
   \m -> runRandT (runStateT m ()) undefined
     :: StateT () (RandT g m) a -> m ((a, ()), g)
   * -}

-- todo: more efficient implication
pick :: (Eq a) => Int -> [a] -> Circuit [a]
pick 1 xs = (:[]) <$> pickOne xs
pick n xs = do
  e <- pickOne xs
  es <- pick (n - 1) (delete e xs)
  return (e:es)

data CircuitState = CircuitState {
  csWires :: [Wire],
  csGates :: [Gate],
  nextId :: Int
}

type Circuit a = StateT CircuitState (RandT StdGen Identity) a

pickOne :: [a] -> Circuit a
pickOne [] = error "attempted to pickOne from an empty list"
pickOne xs = do
  i <- getRandomR (0, length xs - 1)
  return (xs !! ({- -trace ("!! " ++ show i) -} i))

newWire :: Circuit Wire
newWire = do
  CircuitState ws gs !wire <- get
--  let wire = RepWire sec ('v' : show i)
  put (CircuitState (wire : ws) gs (wire + 1))
  return wire

wires :: Circuit [Wire]
wires = gets csWires

add :: Gate -> Circuit ()
add g = modify (\cs -> cs { csGates = g : csGates cs })

gates :: Circuit [Gate]
gates = gets csGates