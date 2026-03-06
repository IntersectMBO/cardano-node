{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Cardano.Timeseries.Interp.Value(Value(..), FunctionValue, fromBool) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Domain.Instant
import           Cardano.Timeseries.Domain.Timeseries (TimeseriesVector)
import           Cardano.Timeseries.Interp.Types (QueryM)

import           Control.DeepSeq (NFData)
import           Data.Text (unpack)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

type FunctionValue = Value -> QueryM Value

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
  Function :: FunctionValue -> Value deriving Generic

instance NFData Value

instance Show Value where
  show (Scalar x)        = show x
  show (RangeVector x)   = unpack (asText x)
  show (InstantVector x) = unpack (asText x)
  show (Pair x y)        = "(" <> show x <> ", " <> show y <> ")"
  show Truth             = "true"
  show Falsity           = "false"
  show (Duration d)      = show d <> "ms"
  show (Timestamp t)     = show t
  show (Function _)      = "<function>"

fromBool :: Bool -> Value
fromBool Prelude.True  = Truth
fromBool Prelude.False = Falsity
