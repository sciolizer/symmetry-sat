{-# LANGUAGE TypeFamilies #-}

module Families.MultiMap (
  MultiMap
) where

import Control.Lens
import qualified Data.Map as M

newtype MultiMap k v = MultiMap (M.Map k (Int, v))

type instance Index (MultiMap k v) = k
--type instance IxValue (MultiMap k v) = S.Set v