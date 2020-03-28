{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Families.Assignment (
  Assignment(..),
  assignmentToMap
) where

import Control.Lens
import qualified Data.Map.Strict as M

class ( {-
  Ord (Index agn), Show (Index agn),
  {- Ord agn, -} Show agn,
  At agn, IxValue agn ~ Bool,
  AsEmpty agn -})
  => Assignment agn where
  assignmentEntries :: IndexedFold (Index agn) agn Bool

assignmentToMap :: (Ord (Index agn), Assignment agn) => agn -> M.Map (Index agn) Bool
assignmentToMap agn = M.fromList (agn ^@.. assignmentEntries)

instance (Ord var, Show var) => Assignment (M.Map var Bool) where
  assignmentEntries = ifolded
