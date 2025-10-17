{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Timeseries.Query(interp) where
import           Cardano.Timeseries.Domain.Instant (Instant (Instant), InstantVector, share)
import qualified Cardano.Timeseries.Domain.Instant as Domain
import qualified Cardano.Timeseries.Domain.Instant as Instant
import           Cardano.Timeseries.Domain.Interval
import           Cardano.Timeseries.Domain.Timeseries (Timeseries (Timeseries), TimeseriesVector, transpose, eachOldest, eachNewest)
import qualified Cardano.Timeseries.Domain.Timeseries as Timeseries
import           Cardano.Timeseries.Domain.Types (Labelled, MetricIdentifier, Timestamp)
import           Cardano.Timeseries.Query.Expr as Expr
import           Cardano.Timeseries.Query.Value as Value
import           Cardano.Timeseries.Store (Store)
import qualified Cardano.Timeseries.Store as Store
import           Cardano.Timeseries.Util (maybeToEither, safeToDouble, safeToWord64)

import           Control.Monad ((<=<), filterM)
import           Data.List (find)
import           Data.Set (isSubsetOf)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Statistics.Function (minMax)
import           Statistics.Quantile (cadpw, quantile)
import           Statistics.Sample (mean)
import Data.Maybe (fromJust)

join :: (a -> b -> c) -> InstantVector a -> InstantVector b -> Either Error (InstantVector c)
join _ [] _ = Right []
join f (inst@(Domain.Instant ls t v) : xs) other = do
  Domain.Instant _ _ v' <- maybeToEither ("No matching label: " <> show ls) $ find (share inst) other
  rest <- join f xs other
  Right (Domain.Instant ls t (f v v') : rest)

avgOverTime :: Timestamp -> TimeseriesVector Double -> InstantVector Double
avgOverTime at = fmap compute where
  compute :: Timeseries Double -> Instant Double
  compute series = Domain.Instant (Timeseries.labels series) at (mean $ Timeseries.toVector series)

sumOverTime :: Timestamp -> TimeseriesVector Double -> InstantVector Double
sumOverTime at = fmap compute where
  compute :: Timeseries Double -> Instant Double
  compute series = Domain.Instant (Timeseries.labels series) at (sum $ Timeseries.toVector series)

expectInstantVector :: Value -> Either Error (InstantVector Value)
expectInstantVector (Value.InstantVector v) = Right v
expectInstantVector _ = Left "Unexpected expression type: expected an instant vector"

expectRangeVector :: Value -> Either Error (TimeseriesVector Value)
expectRangeVector (Value.RangeVector v) = Right v
expectRangeVector _ = Left "Unexpected expression type: expected a range vector"

expectTimeseriesScalar :: Timeseries Value -> Either Error (Timeseries Double)
expectTimeseriesScalar = traverse expectScalar

expectRangeVectorScalar :: Value -> Either Error (TimeseriesVector Double)
expectRangeVectorScalar v = expectRangeVector v >>= traverse expectTimeseriesScalar

expectInstantScalar :: Instant Value -> Either Error (Instant Double)
expectInstantScalar = traverse expectScalar

expectInstantVectorScalar :: Value -> Either Error (InstantVector Double)
expectInstantVectorScalar v = expectInstantVector v >>= traverse expectInstantScalar

expectPair :: Value -> Either Error (Value, Value)
expectPair (Value.Pair a b) = Right (a, b)
expectPair _ = Left "Unexpected expression type: expected a pair"

expectScalar :: Value -> Either Error Double
expectScalar (Value.Scalar x) = Right x
expectScalar _ = Left "Unexpected expression type: expected a scalar"

expectBoolean :: Value -> Either Error Bool
expectBoolean Truth = Right Prelude.True
expectBoolean Falsity = Right Prelude.False
expectBoolean _ = Left "Unexpected expression type: expected a boolean"

expectDuration :: Value -> Either Error Word64
expectDuration (Value.Duration x) = Right x
expectDuration e = Left "Unexpected expression type: expected a duration"

expectTimestamp :: Value -> Either Error Word64
expectTimestamp (Value.Timestamp x) = Right x
expectTimestamp e = Left "Unexpected expression type: expected a timestamp"

expectFunction :: Value -> Either Error (Value -> Either Error Value)
expectFunction (Value.Function f) = Right f
expectFunction e = Left "Unexpected expression type: expected a function"

toWord64 :: Integer -> Either Error Word64
toWord64 x = maybeToEither ("Integer is to big to fit into a 64-bit unsigned integer: " <> show x) (safeToWord64 x)

toDouble :: Integer -> Either Error Double
toDouble x = maybeToEither ("Integer is to big to fit into an IEEE 64-bit floating point" <> show x) (safeToDouble x)

interpRange :: (Value -> Either Error Value) -> Interval -> Word64 -> Either Error (TimeseriesVector Value)
interpRange f Interval{..} rate = transpose <$> sample start end where

  sample :: Timestamp -> Timestamp -> Either Error [InstantVector Value]
  sample t max | t > max = Right []
  sample t max = (:) <$> (expectInstantVector <=< f) (Value.Timestamp t) <*> sample (t + rate) max

interpVariable :: Store s Double => s -> MetricIdentifier -> Value -> Either Error Value
interpVariable store x t = do
  t <- expectTimestamp t
  Right (Value.InstantVector (fmap (fmap Value.Scalar) (Store.evaluate store x t)))

interpLabel :: Expr -> Either Error (Labelled String)
interpLabel (Application Label [Expr.Str k, Expr.Str v]) = Right (k, v)
interpLabel _ = Left "Unexpected expression: expected a label"

interpLabels :: [Expr] -> Either Error [Labelled String]
interpLabels = traverse interpLabel

interpFilter :: (Value -> Either Error Value) -> InstantVector Value -> Either Error (InstantVector Value)
interpFilter f = filterM pred where
  pred :: Instant Value -> Either Error Bool
  pred inst = (expectBoolean <=< f) (Instant.value inst)

interpMap :: (Value -> Either Error Value) -> InstantVector Value -> Either Error (InstantVector Value)
interpMap f = traverse (traverse f)

interpRate :: TimeseriesVector Double -> Either Error (InstantVector Double)
interpRate v = do
  min <- maybeToEither "Can't compute rate" (eachOldest v)
  max <- maybeToEither "Can't compute rate" (eachNewest v)
  Right $ zipWith compute min max where

  compute :: Instant Double -> Instant Double -> Instant Double
  compute min max =
    let v = (Instant.value max - Instant.value min) / fromIntegral (Instant.timestamp max - Instant.timestamp min) in
    Instant (Instant.labels min) (Instant.timestamp max) v

interpIncrease :: TimeseriesVector Double -> Either Error (InstantVector Double)
interpIncrease v = do
  min <- maybeToEither "Can't compute rate" (eachOldest v)
  max <- maybeToEither "Can't compute rate" (eachNewest v)
  Right $ zipWith compute min max where

  compute :: Instant Double -> Instant Double -> Instant Double
  compute min max =
    let v = Instant.value max - Instant.value min in
    Instant (Instant.labels min) (Instant.timestamp max) v

quantileTimeseries :: Int -> Timeseries Double -> Instant Double
quantileTimeseries k v@Timeseries{..} =
  let value = quantile cadpw k 100 (Timeseries.toVector v) in
  Instant labels (Instant.timestamp $ fromJust (Timeseries.newest v)) value

quantileRangeVector :: Int -> TimeseriesVector Double -> InstantVector Double
quantileRangeVector k = map (quantileTimeseries k)

interp :: Store s Double => s -> Expr -> Timestamp -> Either Error Value
interp _ (Expr.Number x) _ = do
  x <- toDouble x
  Right (Value.Scalar x)
interp store (Expr.Variable x) _ = Right $ Value.Function (interpVariable store x)
interp _ (Application Now []) now = Right (Timestamp (fromIntegral now))
interp store (Application Range [s, a, b]) now = do
  s <- interp store s now >>= expectFunction
  a <- interp store a now >>= expectTimestamp
  b <- interp store b now >>= expectTimestamp
  RangeVector <$> interpRange s (Interval a b) (15 * 1000)
interp store (Application FilterByLabel (s : rest)) now = do
  s <- interp store s now >>= expectInstantVector
  ls <- interpLabels rest
  Right (Value.InstantVector (filter (\i -> Set.fromList ls `isSubsetOf` Instant.labels i) s))
interp store (Application Filter [f, t]) now = do
  f <- interp store f now >>= expectFunction
  t <- interp store t now >>= expectInstantVector
  Value.InstantVector <$> interpFilter f t
interp store (Application Join [a, b]) now = do
  a <- interp store a now >>= expectInstantVector
  b <- interp store b now >>= expectInstantVector
  Value.InstantVector <$> join Value.Pair a b
interp store (Application Map [f, x]) now = do
  f <- interp store f now >>= expectFunction
  x <- interp store x now >>= expectInstantVector
  Value.InstantVector <$> interpMap f x
interp store (Application RangeWithRate [s, a, b, r]) now = do
  s <- interp store s now >>= expectFunction
  a <- interp store a now >>= expectTimestamp
  b <- interp store b now >>= expectTimestamp
  r <- interp store r now >>= expectDuration
  RangeVector <$> interpRange s (Interval a b) r
interp store (Application Rewind [t, d]) now = do
  t <- interp store t now >>= expectTimestamp
  d <- interp store d now >>= expectDuration
  Right (Timestamp (t - d))
interp store (Application BoolToScalar [t]) now = do
  t <- interp store t now >>= expectBoolean
  Right (Scalar (if t then 1 else 0))
interp store (Application TimestampToScalar [t]) now = do
  t <- interp store t now >>= expectTimestamp
  Right (Scalar (fromIntegral t))
interp store (Application DurationToScalar [t]) now = do
  t <- interp store t now >>= expectDuration
  Right (Scalar (fromIntegral t))
interp _ (Application Millis [Expr.Number t]) _ =
  Duration <$> toWord64 t
interp _ (Application Seconds [Expr.Number t]) _ =
  Duration . (1000 *) <$> toWord64 t
interp _ (Application Mins [Expr.Number t]) _ =
  Duration . (60 * 1000 *) <$> toWord64 t
interp _ (Application Hours [Expr.Number t]) _ =
  Duration . (60 * 60 * 1000 *) <$> toWord64 t
interp store (Application Add [a, b]) now = do
  va <- interp store a now >>= expectInstantVectorScalar
  vb <- interp store b now >>= expectInstantVectorScalar
  v <- join (+) va vb
  Right (Value.InstantVector (fmap (fmap Value.Scalar) v))
interp store (Application Quantile [Expr.Number k, expr]) now = do
  v <- interp store expr now >>= expectInstantVectorScalar
  Right $ Value.Scalar $ quantile cadpw (fromIntegral k) 100 (Instant.toVector v)
interp store (Application QuantileOverTime [Expr.Number k, expr]) now = do
  v <- interp store expr now >>= expectRangeVectorScalar
  Right $ Value.InstantVector (fmap Value.Scalar <$> quantileRangeVector (fromIntegral k) v)
interp store (Application Rate [r]) now = do
  r <- interp store r now >>= expectRangeVectorScalar
  -- TODO: PromQL's rate() performs linear regression to extrapolate the samples to the bounds
  r <- interpRate r
  Right (Value.InstantVector (fmap (fmap Value.Scalar) r))
interp store (Application Increase [r]) now = do
  r <- interp store r now >>= expectRangeVectorScalar
  -- TODO: PromQL's increase() performs linear regression to extrapolate the samples to the bounds
  r <- interpIncrease r
  Right (Value.InstantVector (fmap (fmap Value.Scalar) r))
interp store (Application Avg [expr]) now = do
  v <- interp store expr now >>= expectInstantVectorScalar
  Right $ Value.Scalar $ mean (Instant.toVector v)
interp store (Application Max [expr]) now = do
  v <- interp store expr now >>= expectInstantVectorScalar
  Right $ Value.Scalar $ snd $ minMax (Instant.toVector v)
interp store (Application Min [expr]) now = do
  v <- interp store expr now >>= expectInstantVectorScalar
  Right $ Value.Scalar $ fst $ minMax (Instant.toVector v)
interp store (Application AvgOverTime [series]) now = do
  v <- interp store series now >>= expectRangeVectorScalar
  Right $ Value.InstantVector (fmap Value.Scalar <$> avgOverTime now v)
interp store (Application MkPair [a, b]) now = do
  va <- interp store a now
  vb <- interp store b now
  Right $ Value.Pair va vb
interp store (Application Fst [t]) now = do
  (a, _) <- interp store t now >>= expectPair
  Right a
interp store (Application Snd [t]) now = do
  (_, b) <- interp store t now >>= expectPair
  Right b
interp store (Application Expr.True []) now = do
  Right Truth
interp store (Application Expr.False []) now = do
  Right Falsity
interp store (Application Expr.And [a, b]) now = do
  va <- interp store a now >>= expectBoolean
  vb <- interp store b now >>= expectBoolean
  Right (fromBool (va && vb))
interp store (Application Expr.Or [a, b]) now = do
  va <- interp store a now >>= expectBoolean
  vb <- interp store b now >>= expectBoolean
  Right (fromBool (va || vb))
interp store (Application Expr.Not [t]) now = do
  vt <- interp store t now >>= expectBoolean
  Right (fromBool (not vt))
interp store (Application Expr.EqBool [a, b]) now = do
  va <- interp store a now >>= expectBoolean
  vb <- interp store b now >>= expectBoolean
  Right (fromBool (va == vb))
interp store (Application Expr.EqScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (fromBool (va == vb))
interp store (Application Expr.LtScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (fromBool (va < vb))
interp store (Application Expr.LteScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (fromBool (va <= vb))
interp store (Application Expr.GtScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (fromBool (va > vb))
interp store (Application Expr.GteScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (fromBool (va >= vb))
interp store (Application Expr.AddScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (Value.Scalar (va + vb))
interp store (Application Expr.MulScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (Value.Scalar (va * vb))
interp store (Application Expr.SubScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (Value.Scalar (va - vb))
interp store (Application Expr.DivScalar [a, b]) now = do
  va <- interp store a now >>= expectScalar
  vb <- interp store b now >>= expectScalar
  Right (Value.Scalar (va / vb))
interp store (Application Expr.Abs [x]) now = do
  x <- interp store x now >>= expectScalar
  Right (Value.Scalar (abs x))
interp _ expr _ = Left $ "Can't interpret expression: " <> show expr
