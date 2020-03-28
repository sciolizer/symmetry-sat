{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Families.Relation (
  Relation,
  atLeft,
  atRight,
  collFold,
  foldLeft,
  foldRight,
  rawShow
) where

import Control.Lens
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

data Relation a b = Relation {
  leftIndex :: M.Map a (S.Set b),
  rightIndex :: M.Map b (S.Set a)
}

instance (Eq a, Eq b) => Eq (Relation a b) where (==) = (==) `on` leftIndex
instance (Ord a, Ord b) => Ord (Relation a b) where compare = compare `on` leftIndex

instance (Show a, Show b, Ord a, Ord b) => Show (Relation a b) where
  show rel = if consistent rel then show (leftIndex rel) else error "Relation is inconsistent!"

foldTuples :: (Ord a, Ord b) => Fold (Relation a b) (a, b)
foldTuples = folding (\(Relation li _) -> [(a, b) | a <- M.keys li, b <- S.toList $ fromMaybe S.empty (M.lookup a li)])

consistent :: (Ord a, Ord b) => Relation a b -> Bool
consistent rel = sort (rel ^.. foldTuples) == sort (rel ^.. flipIso . foldTuples . to (\(a, b) -> (b, a)))

type instance Index (Relation a b) = (a, b)
type instance IxValue (Relation a b) = ()
instance (Ord a, Ord b) => Ixed (Relation a b)
instance (Ord a, Ord b) => At (Relation a b) where
  at = relates

relates :: (Ord a, Ord b) => (a, b) -> Lens' (Relation a b) (Maybe ())
relates (a, b) = atLeft a . at b

mbBoolIso :: Iso' (Maybe ()) Bool
mbBoolIso = iso m2b b2m where
  m2b (Just ()) = True
  m2b Nothing = False
  b2m True = Just ()
  b2m False = Nothing

instance (Ord a, Ord b) => Contains (Relation a b) where
  contains ab = at ab . mbBoolIso

instance AsEmpty (Relation a b) where
  _Empty = prism s g where
    s () = Relation M.empty M.empty
    g r@(Relation li ri)
     | M.null li = if M.null ri then Right () else error "corrupt relation"
     | otherwise = Left r

newtype Coll x = Coll { unColl :: S.Set x }
  deriving (Foldable)

collIso :: Iso' (Coll x) (S.Set x)
collIso = iso (\(Coll s) -> s) Coll

type instance Index (Coll x) = x
type instance IxValue (Coll x) = ()
instance (Ord x) => Ixed (Coll x)
instance (Ord x) => At (Coll x) where
  at k = collIso . at k
instance (Ord x) => Contains (Coll x) where
  contains k = collIso . contains k
instance AsEmpty (Coll x) where
  _Empty = collIso . _Empty

-- todo: this is obviously not needed
collFold :: Fold (Coll x) x
collFold = folded

insertMS :: (Ord a, Ord b) => a -> b -> M.Map a (S.Set b) -> M.Map a (S.Set b)
insertMS k v mp =
  case M.lookup k mp of
    Nothing -> M.insert k (S.singleton v) mp
    Just z -> M.insert k (S.insert v z) mp

unsertMS :: (Ord a, Ord b) => a -> b -> M.Map a (S.Set b) -> M.Map a (S.Set b)
unsertMS k v mp =
  case M.lookup k mp of
    Nothing -> mp
    Just z ->
      let del = S.delete v z in
      if S.null del then M.delete k mp else M.insert k del mp

atLeft :: (Ord a, Ord b) => a -> Lens' (Relation a b) (Coll b)
atLeft a = lens g s where
  g (Relation li _) =
    case M.lookup a li of
      Nothing -> Empty
      Just z -> Coll z
  s r@(Relation li ri) (Coll z) = Relation li' ri' where
    Coll orig = g r
    added = S.difference z orig
    removed = S.difference orig z
    li' = if S.null z then M.delete a li else M.insert a z li
    ri' = addNew . removeOld $ ri
    addNew rr = foldl (\rr' new -> insertMS new a rr') rr added
    removeOld rr = foldl (\rr' old -> unsertMS old a rr') rr removed

flipIso :: Iso' (Relation a b) (Relation b a)
flipIso = iso flip flip where
  flip (Relation li ri) = Relation ri li

atRight :: (Ord a, Ord b) => b -> Lens' (Relation a b) (Coll a)
atRight b = flipIso . atLeft b

foldLeft :: Fold (Relation a b) a
foldLeft = to (M.keysSet . leftIndex) . folded

foldRight :: Fold (Relation a b) b
foldRight = flipIso . foldLeft

rawShow :: (Show a, Show b) => Relation a b -> String
rawShow (Relation li ri) = "Relation { li = " ++ show li ++ ", ri = " ++ show ri ++ " }"

-- all in a collection can be deleted by setting to Empty


-- one consistency not being caught is if a key maps to an empty set - this generates the same set of tuples, even though
-- foldRight returns invalid values