{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Timeseries.Domain.Timeseries(Timeseries(..), TimeseriesVector,
  transpose, toVector, oldest, newest, eachOldest, eachNewest, superseries) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Domain.Instant (Instant (Instant), InstantVector)
import qualified Cardano.Timeseries.Domain.Instant as Instant
import           Cardano.Timeseries.Domain.Types

import           Control.DeepSeq (NFData)
import           Data.Function (on)
import           Data.List (find, maximumBy, minimumBy)
import           Data.Set
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           GHC.Generics (Generic)

-- | A collection of datapoints sharing a series.
data Timeseries a = Timeseries {
  labels :: SeriesIdentifier,
  dat :: [(Timestamp, a)]
} deriving (Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (Timeseries a)

oldest :: Timeseries a -> Maybe (Instant a)
oldest Timeseries{..} | Prelude.null dat = Nothing
oldest Timeseries{..} =
  let (t, x) = minimumBy (compare `on` fst) dat in
  Just (Instant labels t x)

newest :: Timeseries a -> Maybe (Instant a)
newest Timeseries{..} | Prelude.null dat = Nothing
newest Timeseries{..} =
  let (t, x) = maximumBy (compare `on` fst) dat in
  Just (Instant labels t x)

-- | Every two elements in the list must have distinct series identifiers (set of labels),
-- | i.e. the series in the list must be distinct.
type TimeseriesVector a = [Timeseries a]

eachOldest :: TimeseriesVector a -> Maybe [Instant a]
eachOldest = traverse oldest

eachNewest :: TimeseriesVector a -> Maybe [Instant a]
eachNewest = traverse newest

-- | Given a list of range vectors, forms up a timeseries vector.
-- | This operation is, in some sense, transposition:
-- |
-- |                   ⎴  ⎴  ⎴  ⎴
-- | series1:   ...    ◯     ◯       ...
-- | series2:   ...    ◯  ◯          ...
-- | series3:   ...    ◯  ◯     ◯    ...
-- | ...                 ...
-- |                   ⎵  ⎵  ⎵  ⎵
-- | -------------------------------> t
-- |                   t₀ t₁ t₂ t₃
-- |
-- |       =====>
-- |
-- |
-- |
-- | series1: [ ...    ◯     ◯       ... ]
-- | series2: [ ...    ◯  ◯          ... ]
-- | series3: [ ...    ◯  ◯     ◯    ... ]
-- | ...                ...
-- |
-- | ----------------------------------------> t
--                     t₀ t₁ t₂ t₃
transpose :: [InstantVector a] -> TimeseriesVector a
transpose instants =
  Set.foldl' (\vector ls -> form ls instants : vector) [] (setOfLabels instants) where

  -- | Given a set of labels (identifying a series) form up a series from a list of instant vectors.
  form :: SeriesIdentifier -> [InstantVector a] -> Timeseries a
  form ls insts_ = Timeseries ls (formInt insts_) where
    -- | Extract the data pertaining to the series (identified by the given `SeriesIdentifier`) from the list of
    -- | ranges vectors.
    formInt :: [InstantVector a] -> [(Timestamp, a)]
    formInt [] = []
    formInt (inst : insts) =
      case find (\i -> Instant.labels i == ls) inst of
        Just i -> (Instant.timestamp i, Instant.value i) : formInt insts
        Nothing -> formInt insts

  setOfLabels' :: InstantVector a -> Set SeriesIdentifier
  setOfLabels' [] = Set.empty
  setOfLabels' (i : is) = Set.insert (Instant.labels i) (setOfLabels' is)

  setOfLabels :: [InstantVector a] -> Set SeriesIdentifier
  setOfLabels [] = Set.empty
  setOfLabels (v : vs) = setOfLabels' v `Set.union` setOfLabels vs

toVector :: Timeseries Double -> Vector Double
toVector = Vector.fromList . fmap snd . dat

-- Widen the series by the given set of labels.
superseries :: Set Label -> SeriesIdentifier -> SeriesIdentifier
superseries ls = Set.filter (\(k, _) -> k `elem` ls)

instance Show a => AsText (Timeseries a) where
  asText (Timeseries ls ps) =
    showT (Set.toList ls)
      <> "\n"
      <> Text.unlines (fmap (\(t, v) -> showT t <> " " <> showT v) ps)

instance Show a => AsText (TimeseriesVector a) where
  asText = Text.unlines . fmap asText
