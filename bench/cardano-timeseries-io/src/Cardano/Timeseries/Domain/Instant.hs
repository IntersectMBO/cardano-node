{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Timeseries.Domain.Instant(Instant(..), InstantVector, mostRecent, share, toVector) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Domain.Types (SeriesIdentifier, Timestamp)

import           Control.DeepSeq (NFData)
import qualified Data.Set as Set
import           Data.Text as Text (pack, unlines)
import           Data.Vector
import           GHC.Generics (Generic)

-- | One datapoint in a series.
data Instant a = Instant {
  labels :: SeriesIdentifier,
  timestamp :: Timestamp,
  value :: a
} deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (Instant a)

-- | Do the instant vectors share a series?
share :: Instant a -> Instant b -> Bool
share a b = labels a == labels b

-- | Datapoints from different series. The vector must not contain datapoints sharing a series.
type InstantVector a = [Instant a]

mostRecent :: Instant a -> Instant a -> Instant a
mostRecent u v = if timestamp u < timestamp v then v else u

toVector :: InstantVector Double -> Vector Double
toVector = fromList . fmap value

instance Show a => AsText (Instant a) where
  asText (Instant ls t v) =
    pack (show (Set.toList ls)) <> " " <> pack (show t) <> " " <> pack (show v)

instance Show a => AsText (InstantVector a) where
  asText = Text.unlines . fmap asText
