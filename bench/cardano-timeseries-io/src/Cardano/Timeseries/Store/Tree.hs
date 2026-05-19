{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Cardano.Timeseries.Store.Tree(Point(..), Tree, fromFlat) where

import           Cardano.Timeseries.Domain.Instant (Instant (..), InstantVector)
import           Cardano.Timeseries.Domain.Types (MetricIdentifier, SeriesIdentifier, Timestamp)
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Store.Flat (Flat)
import qualified Cardano.Timeseries.Store.Flat as Flat
import           Cardano.Timeseries.Util (range)

import           Prelude hiding (lookup)

import           Control.DeepSeq (NFData)
import           Data.Functor
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import           GHC.Generics (Generic)

data Point a = Point {
  labels :: SeriesIdentifier,
  value  :: a
} deriving (Show, Ord, Eq, Foldable, Functor, Generic)

instance NFData a => NFData (Point a)

type Tree a = Map MetricIdentifier (Map Timestamp [Point a])

instance Store (Tree a) a where
  insert :: Tree a -> MetricIdentifier -> Instant a -> Tree a
  insert store x (Instant ls t v) = flip (Map.insert x) store $ uncurry (Map.insert t) $
    case Map.lookup x store of
      Nothing    -> ([Point ls v], Map.empty)
      Just inner -> (Point ls v : fromMaybe [] (Map.lookup t inner), inner)

  truncate store cutoff = fmap (Map.dropWhileAntitone (< cutoff)) store

  evaluate :: Tree a -> MetricIdentifier -> Timestamp -> InstantVector a
  evaluate store x t = case Map.lookup x store of
    Just inner ->
      updateTime $ convert $ Map.foldlWithKey accumulate Map.empty $
        range
          rangeStartExc
          rangeEndExc
          inner

      where
        -- | (-) wraps around, so make sure we do not in case `t` is too small
        rangeStartExc = max 0 (t - stalenessConstant)

        rangeEndExc = t + 1

        updateTime :: InstantVector a -> InstantVector a
        updateTime = fmap (\i -> Instant i.labels t i.value)

        accumulate :: Map SeriesIdentifier (Timestamp, a) -> Timestamp -> [Point a] -> Map SeriesIdentifier (Timestamp, a)
        accumulate closest tAcc = List.foldl' accumulateInt closest where
          accumulateInt closest' (Point ls v) = flip (Map.insert ls) closest' $
            case Map.lookup ls closest' of
              Just (t', v') | t' > tAcc -> (t', v')
              _                         -> (tAcc, v)

        convert :: Map SeriesIdentifier (Timestamp, a) -> InstantVector a
        convert = map (\(ls, (tConv, v)) -> Instant ls tConv v) . Map.toList
    Nothing -> []

  new :: Tree a
  new = Map.empty

  metrics :: Tree a -> Set MetricIdentifier
  metrics = Map.keysSet

  count = sum . (sum . Map.map length <$>)

  earliest store key = Map.lookup key store >>= Map.lookupMin <&> fst

  latest   store key = Map.lookup key store >>= Map.lookupMax <&> fst

fromFlat :: Flat a -> Tree a
fromFlat []                          = new
fromFlat (Flat.Point x instant : ps) = insert (fromFlat ps) x instant
