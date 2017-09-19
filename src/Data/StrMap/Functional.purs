module Data.StrMap.Functional
  ( StrMap
  , module Export
  ) where

import Data.Map.Functional (Map)
import Data.Map.Functional as Export

type StrMap v = Map String v
