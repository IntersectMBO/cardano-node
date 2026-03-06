{-# LANGUAGE FunctionalDependencies #-}

module Cardano.Timeseries.Store(Store(..), stalenessConstant) where

import           Cardano.Timeseries.Domain.Instant
import           Cardano.Timeseries.Domain.Types

import           Data.Set (Set)
import           Data.Word (Word64)

-- | Milliseconds
stalenessConstant :: Word64
stalenessConstant = 5 * 60 * 1000

-- | A type-class witnessing that `s` is a metric-store of `a`.
class Store s a | s -> a where
  -- | Insert an instant into the store under a metric name.
  insert :: s -> MetricIdentifier -> Instant a -> s
  -- | Compute a point vector of type `a` such that the timestamp of every point in the vector
  -- | lies within the staleness window of the target timestamp and is the most recent of all
  -- | points in the store sharing a series.
  evaluate :: s -> MetricIdentifier -> Timestamp -> InstantVector a

  -- | Find the earliest occurrence of the metric in the store, if any.
  earliest :: s -> MetricIdentifier -> Maybe Timestamp

  -- | Find the latest occurrence of the metric in the store, if any.
  latest :: s -> MetricIdentifier -> Maybe Timestamp

  -- | An empty store.
  new :: s

  -- A set of metric identifies in the store.
  metrics :: s -> Set MetricIdentifier

  -- | Total number of (<metric>, <labels>, <timestamp>, <value>) tuples.
  count :: s -> Int
