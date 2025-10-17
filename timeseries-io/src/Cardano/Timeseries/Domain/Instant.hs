module Cardano.Timeseries.Domain.Instant(Instant(..), InstantVector, mostRecent, share, toVector) where

import           Cardano.Timeseries.Domain.Types (SeriesIdentifier, Timestamp)

import           Data.Vector

data Instant a = Instant {
  labels :: SeriesIdentifier,
  timestamp :: Timestamp,
  value :: a
} deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Do the instant vectors share a series?
share :: Instant a -> Instant b -> Bool
share a b = labels a == labels b

type InstantVector a = [Instant a]

mostRecent :: Instant a -> Instant a -> Instant a
mostRecent u v = if timestamp u < timestamp v then v else u

toVector :: InstantVector Double -> Vector Double
toVector = fromList . fmap value
