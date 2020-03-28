{-# LANGUAGE TypeFamilies #-}

module OrderedMap (
  StackMap
) where

import Control.Lens

-- traversal of map is in order from most recent assignment to oldest assignment
-- re-assigning is disallowed
-- unassigning must happen in the reverse order of assigning
-- new variables must be explicitly added BEFORE assigning them
-- added but unassigned variables can be queried
data StackMap k v = StackMap
  deriving (Show)

type instance Index (StackMap k v) = k
type instance IxValue (StackMap k v) = v
instance (Ord k) => Ixed (StackMap k v)
instance (Ord k) => At (StackMap k v) where
  at k = undefined