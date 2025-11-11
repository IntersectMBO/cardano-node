{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cardano.Timeseries.Store.Tree(Point(..), Tree, fromFlat) where

import           Cardano.Timeseries.Domain.Instant (Instant (..), InstantVector)
import           Cardano.Timeseries.Domain.Types (MetricIdentifier, SeriesIdentifier, Timestamp)
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Store.Flat (Flat)
import qualified Cardano.Timeseries.Store.Flat as Flat
import           Cardano.Timeseries.Util (range)

import           Prelude hiding (lookup)

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

data Point a = Point {
  labels :: !SeriesIdentifier,
  value :: !a
} deriving (Show, Ord, Eq, Foldable, Functor)

type Tree a = Map MetricIdentifier (Map Timestamp [Point a])

instance Store (Tree a) a where
  insert :: Tree a -> MetricIdentifier -> Instant a -> Tree a
  insert store x (Instant ls t v) = flip (Map.insert x) store $ uncurry (Map.insert t) $
    case Map.lookup x store of
      Nothing -> (Point ls v : [], Map.empty)
      Just inner -> (Point ls v : fromMaybe [] (Map.lookup t inner), inner)

  evaluate :: Tree a -> MetricIdentifier -> Timestamp -> InstantVector a
  evaluate store x t = case Map.lookup x store of
    Just inner ->
      convert $ Map.foldlWithKey accumulate Map.empty (range (t - stalenessConstant) (t + 1) inner) where

      accumulate :: Map SeriesIdentifier (Timestamp, a) -> Timestamp -> [Point a] -> Map SeriesIdentifier (Timestamp, a)
      accumulate closest t = List.foldl' (accumulate t) closest where
        accumulate :: Timestamp -> Map SeriesIdentifier (Timestamp, a) -> Point a -> Map SeriesIdentifier (Timestamp, a)
        accumulate t closest (Point ls v) = flip (Map.insert ls) closest $
          case Map.lookup ls closest of
            Just (t', v') | t' > t -> (t', v')
            _ -> (t, v)

      convert :: Map SeriesIdentifier (Timestamp, a) -> InstantVector a
      convert = map (\(ls, (t, v)) -> Instant ls t v) . Map.toList
    Nothing -> []

  new :: Tree a
  new = Map.empty

  metrics :: Tree a -> Set MetricIdentifier
  metrics = Set.fromList . Map.keys

  count = Map.foldl (Map.foldl (\acc ps -> acc + length ps)) 0

fromFlat :: Flat a -> Tree a
fromFlat [] = new
fromFlat (Flat.Point x instant : ps) = insert (fromFlat ps) x instant
