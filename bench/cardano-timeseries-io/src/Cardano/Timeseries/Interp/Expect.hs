module Cardano.Timeseries.Interp.Expect where

import           Cardano.Timeseries.AsText (showT)
import           Cardano.Timeseries.Domain.Instant (Instant, InstantVector)
import           Cardano.Timeseries.Domain.Timeseries (Timeseries, TimeseriesVector)
import           Cardano.Timeseries.Interp.Types
import           Cardano.Timeseries.Interp.Value as Value

import           Data.Word (Word64)

expectInstantVector :: Value -> InterpM (InstantVector Value)
expectInstantVector (Value.InstantVector v) = pure v
expectInstantVector _ = throwInterpError "Unexpected expression type: expected an instant vector"

expectRangeVector :: Value -> InterpM (TimeseriesVector Value)
expectRangeVector (Value.RangeVector v) = pure v
expectRangeVector _ = throwInterpError "Unexpected expression type: expected a range vector"

expectTimeseriesScalar :: Timeseries Value -> InterpM (Timeseries Double)
expectTimeseriesScalar = traverse expectScalar

expectRangeVectorScalar :: Value -> InterpM (TimeseriesVector Double)
expectRangeVectorScalar v = expectRangeVector v >>= traverse expectTimeseriesScalar

expectInstantScalar :: Instant Value -> InterpM (Instant Double)
expectInstantScalar = traverse expectScalar

expectInstantBool :: Instant Value -> InterpM (Instant Bool)
expectInstantBool = traverse expectBool

expectInstantVectorScalar :: Value -> InterpM (InstantVector Double)
expectInstantVectorScalar v = expectInstantVector v >>= traverse expectInstantScalar

expectInstantVectorBool :: Value -> InterpM (InstantVector Bool)
expectInstantVectorBool v = expectInstantVector v >>= traverse expectInstantBool

expectPair :: Value -> InterpM (Value, Value)
expectPair (Value.Pair a b) = pure (a, b)
expectPair _ = throwInterpError "Unexpected expression type: expected a pair"

expectScalar :: Value -> InterpM Double
expectScalar (Value.Scalar x) = pure x
expectScalar _ = throwInterpError "Unexpected expression type: expected a scalar"

expectBool :: Value -> InterpM Bool
expectBool Value.Truth = pure Prelude.True
expectBool Value.Falsity = pure Prelude.False
expectBool _ = throwInterpError "Unexpected expression type: expected a bool"

expectBoolean :: Value -> InterpM Bool
expectBoolean Truth = pure Prelude.True
expectBoolean Falsity = pure Prelude.False
expectBoolean _ = throwInterpError "Unexpected expression type: expected a boolean"

expectDuration :: Value -> InterpM Word64
expectDuration (Value.Duration x) = pure x
expectDuration _ = throwInterpError "Unexpected expression type: expected a duration"

expectTimestamp :: Value -> InterpM Word64
expectTimestamp (Value.Timestamp x) = pure x
expectTimestamp _ = throwInterpError "Unexpected expression type: expected a timestamp"

expectFunction :: Value -> InterpM FunctionValue
expectFunction (Value.Function f) = pure f
expectFunction _ = throwInterpError "Unexpected expression type: expected a function"

expectWord64 :: Double -> InterpM Word64
expectWord64 x
  | snd pf == 0.0 = pure $ fst pf
  | otherwise     = throwInterpError $ "Expected a whole number, got: " <> showT x
  where
    pf = properFraction x
