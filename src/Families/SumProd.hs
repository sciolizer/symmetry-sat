module Families.SumProd where

import qualified Data.Map as M
import qualified Data.Set as S

data SumProdProblem var val term prod sum = SumProdProblem {
  variableValues :: M.Map var (S.Set val),
  functions :: [([var], [val] -> term)], -- length var == length val
  multiply :: [term] -> prod,
  sum :: [prod] -> sum
}



{-

Variable elimination algorithm does not have a chart, but appears in section 2.4.1

for each variable in some elimination ordering over the set of variables:
  take all functions involving that variable, and combine them into a single
  new function, which takes as input the union of the inputs of the original
  functions, excepting the currently selected variable

Algorithm 1: RC-Space - Linear Space Recursive Conditioning

T is a hypertree of SumProd problems, where at every node, the functions in the
left sub-branch are disjoint from the functions in the right sub-branch.
Each leaf node uses a single function.

RC-Space (T: hypergraph, rho: partial-assignment initially empty)
begin
  if T is a leaf node then
    return LOOKUP(value of function labeling the leaf node)
  p = 0; r = root(T)
  x-vec = variables in label(left(r)) /\ label(right(r)) uninstantiated by rho
  forall a in {instantiations of x-vec} do
    p = p + RC-Space(leftChild(T), rho \/ a) * RC-Space(rightChild(T), rho \/ a)
  end
  return p
end

Algorithm 2: RC-Cache - Recursive conditioning with caching

RC-Cache(T: hypergraph, rho: partial-assignment initially empty)
begin
  if T is a leaf node then
    return LOOKUP(value of function labeling the leaf node)
  y = rho /\ label(root(T))
  if InCache(T, y) then
    return GetValue(T, y)
  p = 0; r = root(T)
  x-vec = variables in label(left(r)) /\ label(right(r)) uninstantiated by rho
  forall a in {instantiations of x-vec} do
    p = p + RC-Cache(leftChild(T), p \/ a) * RC-Cache(rightChild(T), rho \/ a)
 end
 AddToCache((T, y), p)
 return p
end

Algorithm 3: AND/OR-Space - Linear Space AND/OR Search

Unlike in the hypergraph, every node of a psuedo-tree is a SINGLE variable
from the original problem. The functions associated with a node in a psuedo-tree
are all functions which use that node's variable and for which all other variables
in the function are associated with ancestors of that node

AND/OR-Space(T: psuedo-tree, rho: partial assignment?)
begin
  p = 0; r = root(T)
  ST-r = set of subtrees below r
  forall d in {instantiations of r} do
    a = product $ forall f in fns(r) lookup(value of f on rho \/ {r = d})
    p = p + a * product ( forall T' in ST-r AND/OR-Space(T', rho \/ {r = d})
  end
  return p
end

To me, this seems mostly the same as RC-Space... it just works with a psuedo-tree
instead of a hypergraph/hypertree, and so instantiations are always on a single vairable.


optimal tree widths are NP-hard... but so what?! That's easier than #P, so it seems
worth it to spend the computation time!


Algorithm 8: #DPLL algorithm with component caching (#DPLL-Cache)
(the winning algorithm)
#DPLL-Cache(phi: set of disjoint formulas)
  // Returns the probability of the set of disjoint formulas phi
begin
  if InCache(phi)
    // Meaning at least one component = 0 or all components are known
    // Also detects obvious formulas.
    return GetValue(phi)
  else
    chi = RemoveCachedComponents(phi) -- leaving behind the components yet-to-be-computed
    choose a variable v that appears in some component c of chi
    chi-neg = ToComponents(c | v = 0) -- break-up components further if possible
    #DPLL-Cache((chi - {c}) \/ chi-neg)
    chi-pos = ToComponents(c | v = 1)
    #DPLL-Cache((chi - {c}) \/ chi-pos
    AddToCache(c, weight(pos(v)) * GetValue(chi-neg) + weight(neg(v)) * GetValue(chi-pos))
    RemoveFromCache(chi-neg \/ chi-pos)
    return GetValue(phi)
end
^ Initially called as #DPLL-Cache(ToComponents(formula))

Bayesian networks are encoded into WMC by basically converting every
row of a CPT into a horn clause, e.g.

Par -> Chi

Par | p(Chi)
 T  |  0.7
 F  |  0.2

we have the rules

Par & pChiT => Chi
Par & ~pChiT => ~Chi
~Par & pChiF => Chi
~Par & ~pChiF => ~Chi

where

weight(pChiT) = 0.7
weight(~pChiT) = 0.3
weight(pChiF) = 0.2
weight(pChiF) = 0.8
weight(Chi) = 1
weight(~Chi) = 1

(weight(Par) and weight(~Par) will depend on the probabilities for Par)

So in a learning problem, where we have

         +-----------+
Param -> | DataPoint |
         +-----------+

(^that's a plate model)

ergh... this is rather pointless since the values of the DataPoints will be known.


Finding the top model is a MAP query...
 (find the values for the meta-parameters (such as "is there an edge here?")
  such that the data is best explained)

question 1: weighted model counting is to bayes/MPE as what is to MAP queries?

as for entropy calculations...
question 2: what is the complexity class of experiment proposal, and
question 2a: what is its canonical problem?
  possible answer: http://reasoning.cs.ucla.edu/fetch.php?id=165&type=pdf



Algorithm 9: SumProd-DPLL-Cache algorithm for arbitrary SUMPROD problems
(the winning algorithm adapted for generic SumProd problems)
SumProd-DPLL-Cache(phi: set of components, rho: partial-assignment)
begin
  if InCache(phi) then
    -- true if either at least one component is 0 or all components are known
    return GetValue(phi)
  else
    chi = RemoveCachedComponents(phi) -- i.e. keep only the components whose value is unknown
    choose a variable x that appears in some component comp of chi
    p = 0
    foreach d in domain of x do
      phi-d = ToComponents(comp | x = d) -- i.e. assign x = d and break components down further if possible
      a = product $ forall f in functions over x: LOOKUP(value of f on rho \/ {x = d})
      p = p + a * SumProd-DPLL-Cache((phi - comp) \/ phi-d, rho \/ {x = d}) -- i.e. replace comp with phi-d
    end
    AddToCache(comp, p)
    -- optional optimization?: RemoveFromCache the subcomponents which are part of the larger component you just added
  return GetValue(phi)
end

I'm not sure what a "component" is in a SUM-PROD problem.
Obviously in DPLL it means a disjunct of conjuncts.

Seems even this algorithm could be made a bit faster by doing a binary decomposition of x, instead
of iterating over all possible values of x.


The check of whether at least one component is 0 seems like it is a good start toward a better optimization...  e.g. if the cumulative product of all of the components falls below some threshold, then you
can just immediately return "epsilon".

ideally the components would be sorted from smallest to largest, so that you "find epsilon" as early
as possible


Entailment means that if the premises are true, then the consequent must be true.
If any of the premises are false, the consequent can be whatever it wants.

resolution (a|b & ~b|c ==> a|c) is valid because
If b is true, then c must be true, and so a|c is true.
If b is false, then a must be true, and so a|c is true.

I'm not so sure it works the same with weighted model counting.

BWMC(a|b & ~b|c) =
  b * BMWC(c) +
  ~b * BMWC(a)
  = b * c + ~b * a

BWMC(a|c) =
  a * 1 + ~a * BWMC(c) = a + ~a * c

What is the relationship between
  b*c + ~b*a   and
  a + ~a*c
  ?

oh, actually I should be computing:

BWMC(a|b & ~b|c & a|c) =
  b * BWMC(c & a|c) +
  ~b * BWMC(a & a|c)

  = b * c + ~b * a

which is obviously the same.



So what does the exploitation of symmetry look like in this case?

I *think* it means pre-caching symmetric components.



-}

