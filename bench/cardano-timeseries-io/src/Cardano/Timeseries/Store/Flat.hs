{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Cardano.Timeseries.Store.Flat(Flat, Point(..)) where

import           Cardano.Timeseries.Domain.Instant (Instant (..), InstantVector, share)
import qualified Cardano.Timeseries.Domain.Instant as Instant
import           Cardano.Timeseries.Domain.Types
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Util (toMaybe)

import           Prelude hiding (Foldable (..))

import           Control.DeepSeq (NFData)
import           Data.Foldable (Foldable (..))
import           Data.Set (fromList)
import           GHC.Generics (Generic)

data Point a = Point {
  name    :: MetricIdentifier,
  instant :: Instant a
} deriving (Show, Eq, Functor, Generic)

instance NFData a => NFData (Point a)

type Flat a = [Point a]

instance Store (Flat a) a where
  insert :: Flat a -> MetricIdentifier -> Instant a -> Flat a
  insert store metric instant = Point metric instant : store

  evaluate :: Flat a -> MetricIdentifier -> Timestamp -> InstantVector a
  evaluate store targetMetric targetTime = updateTime $ foldl' choose [] store where

    updateTime :: InstantVector a -> InstantVector a
    updateTime = fmap (\i -> Instant i.labels targetTime i.value)

    choose :: InstantVector a -> Point a -> InstantVector a
    choose acc point = accumulate acc (toMaybe (satisfies point) point) where

      -- | Does that point match target metric name?
      -- | Does that point lie within the staleness window?
      satisfies :: Point a -> Bool
      satisfies x = x.name == targetMetric
                 && x.instant.timestamp + stalenessConstant >= targetTime
                 && x.instant.timestamp <= targetTime

      accumulate :: InstantVector a -> Maybe (Point a) -> InstantVector a
      accumulate acc' Nothing = acc'
      accumulate acc' (Just p_) = accumulateInt acc' p_ where
        accumulateInt :: InstantVector a -> Point a -> InstantVector a
        accumulateInt [] p = [p.instant]
        accumulateInt (x : xs) p | share x p.instant = Instant.mostRecent x p.instant : xs
        accumulateInt (x : xs) p = x : accumulateInt xs p

  new = []

  metrics store = fromList (map (.name) store)

  count = length

  earliest [] _        = Nothing
  earliest store ident = Just $ minimum [timestamp instant | Point{..} <- store, name == ident]

  latest [] _          = Nothing
  latest store ident   = Just $ maximum [timestamp instant | Point{..} <- store, name == ident]
