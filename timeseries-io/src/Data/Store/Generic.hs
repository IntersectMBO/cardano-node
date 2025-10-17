{-# LANGUAGE FunctionalDependencies #-}

module Data.Store.Generic(Store(..)) where

import           Data.Store.Common
import           Data.Store.Point

class Store s a | s -> a where
  -- | Insert a point into the store.
  insert :: s -> Point a -> s
  -- | Get the most recent point with that metric name having that subset of labels, if any.
  recent :: s -> Metric -> [Labelled String] -> Maybe ([Labelled String], Timestamp, a)
  -- | Get all points with that metric name having that subset of labels within that interval.
  within :: s -> Metric -> [Labelled String] -> Interval -> [([Labelled String], Timestamp, a)]

-- TODO: Move this
data Series a where
  Recorded :: Store s a => s -> Series s
  Sum :: Series a -> Series a -> Series s



