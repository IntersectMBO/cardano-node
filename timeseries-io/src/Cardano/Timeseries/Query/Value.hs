module Cardano.Timeseries.Query.Value(Error, Value(..), fromBool) where

import           Cardano.Timeseries.Domain.Instant

import           Data.Word (Word64)
import Cardano.Timeseries.Domain.Timeseries (TimeseriesVector)

type Error = String

-- | A model of values that queries interpret into.
data Value where
  -- | A scalar.
  Scalar :: Double -> Value
  -- | A range vector.
  RangeVector :: TimeseriesVector Value -> Value
  -- | An instant vector.
  InstantVector :: InstantVector Value -> Value
  -- | A pair.
  Pair :: Value -> Value -> Value
  -- | Truth.
  Truth :: Value
  -- | Falsity.
  Falsity :: Value
  -- | Duration (milliseconds)
  Duration :: Word64 -> Value
  -- | Timestamp (milliseconds since epoch)
  Timestamp :: Word64 -> Value
  -- | Function
  Function :: (Value -> Either Error Value) -> Value

fromBool :: Bool -> Value
fromBool Prelude.True = Truth
fromBool Prelude.False = Falsity
