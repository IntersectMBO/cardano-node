module Cardano.Timeseries.Domain.Interval(Interval(..), length) where

import           Cardano.Timeseries.Domain.Types (Timestamp)

import           Prelude hiding (length)

data Interval = Interval {
  start :: Timestamp,
  end :: Timestamp
} deriving (Show, Eq)

length :: Interval -> Double
length (Interval s e) = fromIntegral (e - s) / 2
