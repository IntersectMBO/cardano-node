{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cardano.Timeseries.Store.Flat(Flat, Point(..)) where

import           Cardano.Timeseries.Domain.Instant (Instant (..), InstantVector, mostRecent, share)
import           Cardano.Timeseries.Domain.Types
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Util

import           Data.List (foldl')
import           Data.Set (fromList)

data Point a = Point {
  name :: MetricIdentifier,
  instant :: Instant a
} deriving (Show, Eq, Functor)

type Flat a = [Point a]

instance Store (Flat a) a where
  insert :: Flat a -> MetricIdentifier -> Instant a -> Flat a
  insert store metric instant = Point metric instant : store

  evaluate :: Flat a -> MetricIdentifier -> Timestamp -> InstantVector a
  evaluate store targetMetric targetTime = foldl' choose [] store where

    choose :: InstantVector a -> Point a -> InstantVector a
    choose acc p = accumulate acc (toMaybe (satisfies p) p) where

      -- | Does that point match target metric name?
      -- | Does that point lie within the staleness window?
      satisfies :: Point a -> Bool
      satisfies x = name x == targetMetric
                 && timestamp (instant x) + stalenessConstant >= targetTime
                 && timestamp (instant x) <= targetTime

      accumulate :: InstantVector a -> Maybe (Point a) -> InstantVector a
      accumulate acc Nothing = acc
      accumulate acc (Just p) = accumulate acc p where
        accumulate :: InstantVector a -> Point a -> InstantVector a
        accumulate [] p = [instant p]
        accumulate (x : xs) p | share x (instant p) = mostRecent x (instant p) : xs
        accumulate (x : xs) p = x : accumulate xs p


  new = []

  metrics store = fromList (map name store)

