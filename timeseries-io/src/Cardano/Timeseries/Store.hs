{-# LANGUAGE FunctionalDependencies #-}

module Cardano.Timeseries.Store(Store(..), stalenessConstant) where

import           Cardano.Timeseries.Domain.Instant
import           Cardano.Timeseries.Domain.Types

import           Data.Word (Word64)
import Data.Set (Set)

-- | Milliseconds
stalenessConstant :: Word64
stalenessConstant = 5 * 60 * 1000

class Store s a | s -> a where
  -- | Insert an instant into the store under a metric name.
  insert :: s -> MetricIdentifier -> Instant a -> s
  -- | Compute a point vector of type `a` such that the timestamp of every point in the vector
  -- | lies within the staleness window of the target timestamp and is the most recent of all
  -- | points in the store sharing a series.
  evaluate :: s -> MetricIdentifier -> Timestamp -> InstantVector a

  new :: s

  metrics :: s -> Set MetricIdentifier

  -- | Total number of (<metric>, <labels>, <timestamp>, <value>) tuples.
  count :: s -> Int
