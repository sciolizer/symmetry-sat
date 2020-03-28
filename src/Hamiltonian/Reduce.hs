{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
module Hamiltonian.Reduce (
--  reduceToHamiltonianAndBack
) where

{-
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Clause
import qualified Hamiltonian.AppendOnlyBiMap as AB
import Literal
import qualified PsuedoBoolean as PB
import Var

data DiamondNode = DNEntrance | DNLeft | DNRight | DNPreRight
  deriving (Eq, Ord)

instance Show DiamondNode where
  show DNEntrance = "de"
  show DNLeft = "dl"
  show DNRight = "dr"
  show DNPreRight = "dp"

data DiamondClauseNode = DCNLeft | DCNRight | DCNPreLeft
  deriving (Eq, Ord)

instance Show DiamondClauseNode where
  show DCNLeft = "cl"
  show DCNRight = "cr"
  show DCNPreLeft = "cp"

data Node d c
  = DiamondNode d DiamondNode
  | ClauseHubNode c
  | DiamondClauseNode d c DiamondClauseNode
  deriving (Eq, Ord)

instance (Show d, Show c) => Show (Node d c) where
  show (DiamondNode v dn) = show dn ++ show v
  show (ClauseHubNode c) = "ch" ++ show c
  show (DiamondClauseNode d c dcn) = show d ++ show dcn ++ show c

type ONode = Node DiamondGadget ClauseGadget

data OutVar d c s
  = EdgeInPath (Node d c) (Node d c)
  | NodeInSlot (Node d c) s
  deriving (Eq, Ord)

instance (Show d, Show c, Show s) => Show (OutVar d c s) where
  show (EdgeInPath n1 n2) = "e" ++ show n1 ++ "T" ++ show n2
  show (NodeInSlot n s) = "n" ++ show n ++ "A" ++ show s

type OVar = OutVar DiamondGadget ClauseGadget SlotGadget

type DiamondGadget = Var
type ClauseGadget = Clause
type SlotGadget = Int -- slot in path

type Flatten a = State (AB.ABMap OVar Var, Int) a

-- input variables become the (entrance -> left) edges in the reduction, so that variable ids are preserved
reduceToHamiltonianAndBack :: [Clause] -> String -- (AB.ABMap Var OrderedString, [PBConstraint OVar])
reduceToHamiltonianAndBack input = output where
  allVars = allClauseVars input
--  output = flatten allVars . convert $ input
--  output = (AB.fromList [],) . S.toList . convert $ input
  output = convert input
newtype OrderedString = OrderedString { unOrderedString :: OVar }
  deriving (Eq, Ord, Show)

--instance Show OrderedString where
--
--instance Ord OrderedString where


-- non-parametric reduction, just for testing
flatten :: [Var] -> S.Set (PB.Constraint OVar) -> (AB.ABMap Var OrderedString, [PB.Constraint Var])
flatten originalVars cs = (AB.fromList . map (\(ov, v) -> (v, OrderedString ov)) . AB.toList $ mp, cls) where
  (cls, (mp, _)) = runState (mapM flatten' (S.toList cs)) (initialMap, (+1) . maximum . map varToInt $ originalVars)
  initialMap = {- traceShowId $ -} AB.fromList [(EdgeInPath (DiamondNode var DNEntrance) (DiamondNode var DNLeft), var) | var <- originalVars]

flatten' :: PB.Constraint OVar -> Flatten (PB.Constraint Var)
flatten' = traverse literify
--flatten' literalSet = fromWellFormedGrounded <$> mapM literify (S.toList literalSet)

literify :: OVar -> Flatten Var
literify ovar = do
  (mp, !nextId) <- get
  case AB.lookupLeft ovar mp of
    Nothing -> do
      let var = varFromInt nextId
--      if any (== var) . map snd . AB.toList $ mp then error ("var already present: " ++ show var) else return () -- todo: remove
      put $ (AB.insert ovar var mp, nextId + 1)
      return var
    Just var -> return var

convert :: [Clause] -> String
convert cls = unlines . map show $ PB.execConstraintAccumulator (constrain (head (allClauseVars cls)) (buildRelations (edges cls)))

allClauseVars :: [Clause] -> [Var]
allClauseVars = S.toAscList . S.unions . map clauseVars

constrain :: Var -> [NodeRelations] -> ConstraintAccumulator ()
constrain firstVar ns = do
  PB.assert $ NodeInSlot (DiamondNode firstVar DNEntrance) 0 -- not strictly necessary, but breaks symmetry
  mapM_ (constrainNode (length ns)) ns

type NodeRelations = (ONode, [ONode], [ONode])

buildRelations :: S.Set (ONode, ONode) -> [NodeRelations]
buildRelations edges = map newRelation allNodes where
  edgesList = S.toList edges
  allNodes = S.toList . S.fromList . concatMap (\(n1, n2) -> [n1, n2]) $ edgesList -- todo: this is a good opportunity to ensure that every node has at least one in and one out
  buildMap = M.unionsWith S.union . map (\(n1, n2) -> M.singleton n1 (S.singleton n2))
  forwardMap = buildMap edgesList
  reverseMap = buildMap . map (\(n1, n2) -> (n2, n1)) $ edgesList
  newRelation node = (node, lkp node forwardMap, lkp node reverseMap)
  lkp node mp =
    case M.lookup node mp of
      Nothing -> error "map building failed"
      Just z -> S.toList z

--contextualize :: [a] -> [(Maybe a, a, Maybe a)]
--contextualize = undefined

contextualizeLoopy :: [a] -> [(a, a, a)]
contextualizeLoopy xs = zip3 (last xs : xs) xs (tail xs ++ [head xs])

edges :: [Clause] -> S.Set (ONode, ONode)
edges clauses = execWriter (mapM_ (\(pv,v,nv) -> diamondEdges pv v nv clauses clauseUsage) (contextualizeLoopy (allClauseVars clauses))) where
  clauseUsage v c = M.lookup v (clauseLookup c) -- todo: there should probably be a more direct function for this in Clause.hs

type EdgeAccumulator a = Writer (S.Set (ONode, ONode)) a

diamondEdges :: Var -> Var -> Var -> [Clause] -> (Var -> Clause -> Maybe Bool) -> EdgeAccumulator ()
diamondEdges prevVar var nextVar clauses clauseUsage = do
  let previousLeftNode = DiamondNode prevVar DNLeft
  let previousRightNode = DiamondNode prevVar DNRight
  let entranceNode = DiamondNode var DNEntrance
  let leftNode = DiamondNode var DNLeft
  let preRightNode = DiamondNode var DNPreRight
  let rightNode = DiamondNode var DNRight
  let exitNode = DiamondNode nextVar DNEntrance
  let firstClauseNode = DiamondClauseNode var (head clauses) DCNPreLeft
  let lastClauseNode = DiamondClauseNode var (last clauses) DCNRight
  oneWay [previousLeftNode, previousRightNode] [entranceNode]
  oneWay [entranceNode] [leftNode, rightNode]
  biWay leftNode firstClauseNode
  biWay lastClauseNode preRightNode >> biWay preRightNode rightNode
  oneWay [leftNode, rightNode] [exitNode]
  sequence_ [biWay left right | clause <- clauses, (left, right) <- adjacentPairs (map (DiamondClauseNode var clause) [DCNPreLeft, DCNLeft, DCNRight])]
  sequence_ [biWay (DiamondClauseNode var clause1 DCNRight) (DiamondClauseNode var clause2 DCNPreLeft) | (clause1, clause2) <- adjacentPairs clauses]
  sequence_ [hubEdges clauseUsage var clause | clause <- clauses]

hubEdges :: (Var -> Clause -> Maybe Bool) -> Var -> Clause -> EdgeAccumulator ()
hubEdges clauseUsage var clause = answer where
  leftNode = DiamondClauseNode var clause DCNLeft
  rightNode = DiamondClauseNode var clause DCNRight
  hubNode = ClauseHubNode clause
  answer =
    case clauseUsage var clause of
      Nothing -> return ()
      Just True -> oneWay [leftNode] [hubNode] >> oneWay [hubNode] [rightNode]
      Just False -> oneWay [rightNode] [hubNode] >> oneWay [hubNode] [leftNode]

oneWay :: [ONode] -> [ONode] -> EdgeAccumulator ()
oneWay from to = sequence_ [edge src dst | src <- from, dst <- to]

edge :: ONode -> ONode -> EdgeAccumulator ()
edge n1 n2 = tell . S.singleton $ (n1, n2)

biWay :: ONode -> ONode -> EdgeAccumulator ()
biWay n1 n2 = edge n1 n2 >> edge n2 n1

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = zip xs (tail xs)

type ConstraintAccumulator a = PB.ConstraintAccumulator OVar a

constrainNode :: Int -> NodeRelations -> ConstraintAccumulator ()
constrainNode numNodes (node, parents, children) = do
  let slots = [0..(numNodes-1)]
  PB.comment $ show node ++ " has one parent"
  PB.exactlyOneOf [EdgeInPath parent node | parent <- parents]
  PB.comment $ show node ++ " has one child"
  PB.exactlyOneOf [EdgeInPath node child | child <- children]
  PB.comment $ show node ++ " has one slot"
  PB.exactlyOneOf [NodeInSlot node slot | slot <- slots]
  PB.comment $ show node ++ " is adjacent to its child"
  sequence_ [PB.horn (NodeInSlot child ((slot + 1) `mod` numNodes)) [NodeInSlot node slot, EdgeInPath node child] | slot <- slots, child <- children] -- this is monstrous


-- todo: https://www.cs.cmu.edu/~wklieber/papers/2007_efficient-cnf-encoding-for-selecting-1.pdf
--naiveExactlyOneOf:: [OVar] -> ConstraintAccumulator ()
--naiveExactlyOneOf vars = do
--  -- at least one
--  addConstraint . map (,True) $ vars
--  -- at most one
--  sequence_ [addConstraint [(var1, False), (var2, False)] | var1 <- vars, var2 <- vars, var1 < var2]
--
--data Commander d c s = Commander [Int] [OutVar d c s]
--type OCommander = Commander DiamondGadget ClauseGadget SlotGadget
--
--exactlyOneOf :: [OVar] -> ConstraintAccumulator OCommander
--exactlyOneOf vars
--  | length vars < 6 = naiveExactlyOneOf vars
--  | otherwise = do
--
--
--chop3 :: [a] -> [[a]]
--chop3 = undefined
--
--exactlyOneOfByCommander :: [OVar] -> OCommander -> ConstraintAccumulator ()
--exactlyOneOfByCommander vars cmd = do
--  naiveExactlyOneOf vars
--  addConstraint $ (CommanderVar cmd, False) : (map (,True) vars)
--  sequence_ [addConstraint [(CommanderVar cmd, True), (var, False)] | var <- vars]

firstVariable :: ConstraintAccumulator Var
firstVariable = undefined

--parameterizations :: OutVar d c s -> (Maybe (d, OutVar () c s), Maybe (c, OutVar d () s), Maybe (s, OutVar d c ()))
--parameterizations :: OutVar -> (Maybe (DiamondGadget, DiamondlessOutVar), Maybe (ClauseGadget, ClauselessOutVar), Maybe (SlotGadget, SlotlessOutVar))
--parameterizations ov =
--  case ov of
--    EdgeInPath (DiamondNode dg1 dn1) (DiamondNode dg2 dn2)
--      | dg1 == dg2 -> (Just (dg1, EdgeWithinDiamond (DiamondExclusive dn1) (DiamondExclusive dn2)), Nothing, Nothing)
--      | otherwise -> (Nothing, Nothing, Nothing)
--    EdgeInPath (DiamondNode dg1 n1) (DiamondClauseNode dg2 cg n2)
--      | dg1 == dg2 -> (Just (dg1, EdgeWithinDiamond (DiamondExclusive dn1) (DiamondShared cg n2))

mergeOutVarD :: [OutVar d c s] -> Maybe (d, [OutVar () c s])
mergeOutVarD = undefined

-- could reuse mergeOutVarD if we rotate the type parameters

extractCommonGadget :: (Functor f, Foldable f) => [f a] -> Maybe (a, [f ()])
extractCommonGadget = undefined

-- parametric clauses
type PCs f a = M.Map (S.Set (f ())) (S.Set a) -- map from (abstract) clauses (set of literals) to their instantiations

newtype OutVar2 c s d = OutVar2 { unOutVar2 :: OutVar d c s }

newtype OutVar3 s d c = OutVar3 { unOutVar3 :: OutVar d c s }

combine :: [[(OutVar d c s, Bool)]] -> (PCs (OutVar d c) s, PCs (OutVar2 c s) d, PCs (OutVar3 s d) c)
combine = undefined


--data Accum a



-- write the constraint generating code before writing the constraint combining code
-- it's possible that once I see the constraint generating code, I'll realize that
-- combining the constraints by hand is not a bid deal
--oneInOneOut :: Int -> [Clause] -> Accum ()
--oneInOneOut numVars clauses = do
--  exactlyOne [EdgeInPath (DiamondNode v DNEntrance) (DiamondNode v DNRight), EdgeInPath (DiamondNode v DNPreRight) (DiamondNode v DNRight)]\
  -- etc

-- notice that a var can have multiple parameterizations
-- e.g. the edge going from the left clause connector to the right clause connector in a particular
-- diamond belongs both to the diamond gadget and to the clause gadget
-- e.g. most NodeInSlots belong to both a SlotGadget and a DiamondGadget

-- when a clause is generated, we check to see if if there is any overlap in the parameterizations
-- for each. If there is an intersection, then we abstract the outVars over the gadget.
-- then we check to see if we previously generated a constraint whos abstraction matches but whose
-- gadget is different... the matching is done over sets so that accidentally distinct permutations
-- are not overlooked. If we succeed, then the two clauses are merged together into a single
-- parametric clause.

-- the other way to do this is to allow overloaded representations of vars, and then shuffle between
-- them.

--unifyDiamond :: OutVar -> OutVar -> Maybe DiamondGadget
--unifyDiamond = undefined

--unifyClause :: OutVar -> OutVar -> Maybe ClauseGadget

--data ParametricOutVar
--  = ForAllDiamonds DiamondOutVars
--  | ForAllClauses ClauseOutVars
--  | ForAllSlots SlotOutVars
--  | ForAllDiamondClauses DiamondClauseOutVars -- this is pointless, since it is a singleton class, right??? I suppose not... there is still some symmetry... but the
--
--data DiamondOutVars
--  = DiamondEdgeInPath DiamondEdge
--
--data DiamondEdge
--  = EntranceInFromLeft
--  | EntranceInFromRight
--  | EntranceOutToLeft
--  | EntranceOutToRight
--  | LeftFromAbove
--  | LeftFromRight
--
--data ClauseOutVars
--  = HubFirstIn
--  | HubSecondIn
--  | HubThirdIn
--  | HubFirstOut
--
--diamondConstraints = do
--  exactlyOne [EntranceInFromLeft, EntranceInFromRight]
--
--clauseConstraints = do
--  exactlyOne [HubFirstIn, HubSecondIn, HubThirdIn]

-}