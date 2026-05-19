{-# LANGUAGE RecordWildCards #-}
module Cardano.Timeseries.Interp.Statistics where
import           Cardano.Timeseries.Domain.Instant
import qualified Cardano.Timeseries.Domain.Instant as Domain
import qualified Cardano.Timeseries.Domain.Instant as Instant
import           Cardano.Timeseries.Domain.Timeseries
import qualified Cardano.Timeseries.Domain.Timeseries as Timeseries
import           Cardano.Timeseries.Domain.Types

import           Data.Maybe (fromJust)

import           Statistics.Quantile (cadpw, quantile)
import           Statistics.Sample (mean)

avgOverTime :: Timestamp -> TimeseriesVector Double -> InstantVector Double
avgOverTime at = fmap compute where
  compute :: Timeseries Double -> Instant Double
  compute series = Domain.Instant (Timeseries.labels series) at (mean $ Timeseries.toVector series)

sumOverTime :: Timestamp -> TimeseriesVector Double -> InstantVector Double
sumOverTime at = fmap compute where
  compute :: Timeseries Double -> Instant Double
  compute series = Domain.Instant (Timeseries.labels series) at (sum $ Timeseries.toVector series)

quantileTimeseries :: Double -> Timeseries Double -> Instant Double
quantileTimeseries k v@Timeseries{..} =
  let value = quantile cadpw (floor (k * 100)) 100 (Timeseries.toVector v) in
  Instant labels (Instant.timestamp $ fromJust (Timeseries.newest v)) value

quantileRangeVector :: Double -> TimeseriesVector Double -> InstantVector Double
quantileRangeVector k = map (quantileTimeseries k)
