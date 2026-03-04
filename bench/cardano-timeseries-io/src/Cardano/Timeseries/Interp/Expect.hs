module Cardano.Timeseries.Interp.Expect where

import           Cardano.Timeseries.AsText (showT)
import           Cardano.Timeseries.Domain.Instant (Instant, InstantVector)
import           Cardano.Timeseries.Domain.Timeseries (Timeseries, TimeseriesVector)
import           Cardano.Timeseries.Interp.Types
import           Cardano.Timeseries.Interp.Value as Value

import           Data.Word (Word64)

expectInstantVector :: Value -> QueryM (InstantVector Value)
expectInstantVector (Value.InstantVector v) = pure v
expectInstantVector _ = throwQueryError "Unexpected expression type: expected an instant vector"

expectRangeVector :: Value -> QueryM (TimeseriesVector Value)
expectRangeVector (Value.RangeVector v) = pure v
expectRangeVector _ = throwQueryError "Unexpected expression type: expected a range vector"

expectTimeseriesScalar :: Timeseries Value -> QueryM (Timeseries Double)
expectTimeseriesScalar = traverse expectScalar

expectRangeVectorScalar :: Value -> QueryM (TimeseriesVector Double)
expectRangeVectorScalar v = expectRangeVector v >>= traverse expectTimeseriesScalar

expectInstantScalar :: Instant Value -> QueryM (Instant Double)
expectInstantScalar = traverse expectScalar

expectInstantBool :: Instant Value -> QueryM (Instant Bool)
expectInstantBool = traverse expectBool

expectInstantVectorScalar :: Value -> QueryM (InstantVector Double)
expectInstantVectorScalar v = expectInstantVector v >>= traverse expectInstantScalar

expectInstantVectorBool :: Value -> QueryM (InstantVector Bool)
expectInstantVectorBool v = expectInstantVector v >>= traverse expectInstantBool

expectPair :: Value -> QueryM (Value, Value)
expectPair (Value.Pair a b) = pure (a, b)
expectPair _ = throwQueryError "Unexpected expression type: expected a pair"

expectScalar :: Value -> QueryM Double
expectScalar (Value.Scalar x) = pure x
expectScalar _ = throwQueryError "Unexpected expression type: expected a scalar"

expectBool :: Value -> QueryM Bool
expectBool Value.Truth = pure Prelude.True
expectBool Value.Falsity = pure Prelude.False
expectBool _ = throwQueryError "Unexpected expression type: expected a bool"

expectBoolean :: Value -> QueryM Bool
expectBoolean Truth = pure Prelude.True
expectBoolean Falsity = pure Prelude.False
expectBoolean _ = throwQueryError "Unexpected expression type: expected a boolean"

expectDuration :: Value -> QueryM Word64
expectDuration (Value.Duration x) = pure x
expectDuration _ = throwQueryError "Unexpected expression type: expected a duration"

expectTimestamp :: Value -> QueryM Word64
expectTimestamp (Value.Timestamp x) = pure x
expectTimestamp _ = throwQueryError "Unexpected expression type: expected a timestamp"

expectFunction :: Value -> QueryM FunctionValue
expectFunction (Value.Function f) = pure f
expectFunction _ = throwQueryError "Unexpected expression type: expected a function"

expectWord64 :: Double -> QueryM Word64
expectWord64 x
  | snd pf == 0.0 = pure $ fst pf
  | otherwise     = throwQueryError $ "Expected a whole number, got: " <> showT x
  where
    pf = properFraction x
