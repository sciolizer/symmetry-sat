module Gen.Reduce (
  pbReduce
) where

pbReduce = undefined

--import Control.Monad
--
--import Gen.Circuit
--import qualified PsuedoBoolean as PB
--
--type Constraint = PB.Constraint Int
--
--type ConstraintAccumulator a = PB.ConstraintAccumulator Wire a
--
--pbReduce :: ManyOneCircuit -> String
--pbReduce circuit = unlines . map showCommOrCon $ commentsAndConstraints where
--  commentsAndConstraints = PB.execConstraintAccumulator (pbReduce' circuit)
--  showCommOrCon (Left c) = show c
--  showCommOrCon (Right c) = show c
--
--pbReduce' :: ManyOneCircuit -> ConstraintAccumulator ()
--pbReduce' (_ , output, gates) = do
--  PB.assert $ output
--  mapM_ addGate gates
--
--addGate :: Gate -> ConstraintAccumulator ()
--addGate gate = do
--  case gate of
--    And is o -> do
--      PB.horn o is
--      sequence_ [PB.horn i [o] | i <- is]
--    Or is o -> do
--      PB.addConstraint ((-1, o) : [(1, i) | i <- is]) PB.GTE 0
--      sequence_ [PB.horn o [i] | i <- is]
--    Not i o -> PB.exactlyOneOf [i, o]
--
--
--{-
--Or:
--a => b \/ c: ~a \/ b \/ c: (1-a) + b + c >= 1: -a + b + c >= 0
--
--a <= b \/ c: (b => a) /\ (c => a)
--
--And:
--a = b /\ c
--
--a => b /\ c: (b \/ ~a) /\ (c \/ ~a): a => b /\ a => c
--
--a <= b /\ c: a \/ ~b \/ ~c
--
---}