module Cardano.Timeseries.Query.Value(Error, Value(..), FunctionValue, fromBool) where

import           Cardano.Timeseries.Domain.Instant
import           Cardano.Timeseries.Domain.Timeseries (TimeseriesVector)

import           Control.Monad.Except (ExceptT)
import           Control.Monad.State.Strict (State)
import           Data.Text (unpack)
import           Data.Word (Word64)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

type Error = String
type FunctionValue = Value -> ExceptT Error (State Int) Value

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
  show (Scalar x) = show x
  show (RangeVector x) = show x
  show (InstantVector x) = unpack (prettyInstantVector x)
  show (Pair x y) = "(" <> show x <> ", " <> show y <> ")"
  show Truth = "True"
  show Falsity = "False"
  show (Duration d) = show d <> "ms"
  show (Timestamp t) = show t
  show (Function t) = "<function>"

fromBool :: Bool -> Value
fromBool Prelude.True = Truth
fromBool Prelude.False = Falsity
