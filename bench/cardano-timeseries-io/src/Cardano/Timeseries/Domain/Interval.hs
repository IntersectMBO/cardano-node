module Cardano.Timeseries.Domain.Interval(Interval(..), duration) where

import           Cardano.Timeseries.Domain.Types (Timestamp)

-- | A time interval. Assumption: `start` ≤ `end`
data Interval = Interval {
  start :: Timestamp,
  end :: Timestamp
} deriving (Show, Eq)

duration :: Interval -> Double
duration (Interval s e) = fromIntegral (e - s) / 2
