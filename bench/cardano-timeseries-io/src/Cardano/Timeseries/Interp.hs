{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Timeseries.Interp(interp) where
import           Cardano.Timeseries.AsText (showT)
import           Cardano.Timeseries.Domain.Identifier (Identifier (..))
import           Cardano.Timeseries.Domain.Instant (Instant (Instant), InstantVector, share)
import qualified Cardano.Timeseries.Domain.Instant as Domain
import qualified Cardano.Timeseries.Domain.Instant as Instant
import           Cardano.Timeseries.Domain.Interval
import           Cardano.Timeseries.Domain.Timeseries (TimeseriesVector, eachNewest, eachOldest,
                   superseries, transpose)
import           Cardano.Timeseries.Domain.Types (Label, Labelled, MetricIdentifier, Timestamp)
import           Cardano.Timeseries.Interp.Config (Config (..))
import           Cardano.Timeseries.Interp.Expect
import           Cardano.Timeseries.Interp.Statistics
import           Cardano.Timeseries.Interp.Types
import           Cardano.Timeseries.Interp.Value as Value
import           Cardano.Timeseries.Query.BinaryArithmeticOp (BinaryArithmeticOp)
import qualified Cardano.Timeseries.Query.BinaryArithmeticOp as BinaryArithmeticOp
import           Cardano.Timeseries.Query.BinaryRelation (BinaryRelation, embedScalar,
                   mbBinaryRelationInstantVector, mbBinaryRelationScalar)
import qualified Cardano.Timeseries.Query.BinaryRelation as BinaryRelation
import           Cardano.Timeseries.Query.Expr as Expr
import           Cardano.Timeseries.Store (Store (metrics))
import qualified Cardano.Timeseries.Store as Store
import           Cardano.Timeseries.Util (maybeToEither)

import           Prelude hiding (max, min, pred)

import           Control.Monad (filterM, (<=<))
import           Control.Monad.Except (liftEither)
import           Control.Monad.State (get, put)
import           Control.Monad.Trans (lift)
import           Data.Function (on)
import           Data.List (find, groupBy)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set, isSubsetOf, member)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Word (Word64)

import           Statistics.Function (minMax)
import           Statistics.Quantile (cadpw, quantile)
import           Statistics.Sample (mean)


interpJoin :: (a -> b -> c) -> InstantVector a -> InstantVector b -> Either QueryError (InstantVector c)
interpJoin _ [] _ = Right []
interpJoin f (inst@(Domain.Instant ls t v) : xs) other = do
  Domain.Instant _ _ v' <- maybeToEither (ErrorMessage $ "No matching label: " <> showT ls) $ find (share inst) other
  rest <- interpJoin f xs other
  Right (Domain.Instant ls t (f v v') : rest)

interpRange :: FunctionValue -> Interval -> Word64 -> QueryM (TimeseriesVector Value)
interpRange f Interval{..} rate = transpose <$> sample start end where

  sample :: Timestamp -> Timestamp -> QueryM [InstantVector Value]
  sample t max | t > max = pure []
  sample t max = (:) <$> (expectInstantVector <=< f) (Value.Timestamp t) <*> sample (t + rate) max

interpLabelInst :: LabelConstraint -> Labelled (Bool, Text)
interpLabelInst (LabelConstraintEq (k, v))    = (k, (Prelude.True, v))
interpLabelInst (LabelConstraintNotEq (k, v)) = (k, (Prelude.False, v))

interpLabelInsts :: Set LabelConstraint -> (Set (Labelled Text), Set (Labelled Text))
interpLabelInsts ls =
  let (sub, notsub) = List.partition cond $ fmap interpLabelInst (Set.toList ls) in
  (Set.fromList $ fmap extract sub, Set.fromList $ fmap extract notsub) where
    cond (_, (b, _)) = b
    extract (x, (_, y)) = (x, y)

interpVariable :: Store s Double => s -> MetricIdentifier -> Value -> QueryM Value
interpVariable store x t_ = do
  t <- expectTimestamp t_
  pure (Value.InstantVector (fmap (fmap Value.Scalar) (Store.evaluate store x t)))

interpQuantileBy :: Set Label -> Double -> InstantVector Double -> Timestamp -> QueryM (InstantVector Double)
interpQuantileBy ls k vs now =
  let groups = groupBy (on (==) (superseries ls . (.labels))) vs
      quantiles = fmap (\g -> (superseries ls (head g).labels, quantile cadpw (floor (k * 100)) 100 (Instant.toVector g)))
                       groups in
  pure $ fmap (\(idx, v) -> Instant idx now v) quantiles

interpFilter :: FunctionValue -> InstantVector Value -> QueryM (InstantVector Value)
interpFilter f = filterM pred where
  pred :: Instant Value -> QueryM Bool
  pred inst = (expectBoolean <=< f) inst.value

interpMap :: FunctionValue -> InstantVector Value -> QueryM (InstantVector Value)
interpMap f = traverse (traverse f)

interpRate :: TimeseriesVector Double -> QueryM (InstantVector Double)
interpRate v = do
  min <- liftEither $ maybeToEither (ErrorMessage "Can't compute rate") (eachOldest v)
  max <- liftEither $ maybeToEither (ErrorMessage "Can't compute rate") (eachNewest v)
  pure $ zipWith compute min max where

  compute :: Instant Double -> Instant Double -> Instant Double
  compute min max =
    let x = (max.value - min.value) / fromIntegral (max.timestamp - min.timestamp) in
    Instant min.labels max.timestamp x

interpIncrease :: TimeseriesVector Double -> QueryM (InstantVector Double)
interpIncrease v = liftEither $ do
  min <- maybeToEither (ErrorMessage "Can't compute rate") (eachOldest v)
  max <- maybeToEither (ErrorMessage "Can't compute rate") (eachNewest v)
  Right $ zipWith compute min max where

  compute :: Instant Double -> Instant Double -> Instant Double
  compute min max =
    let x = max.value - min.value in
    Instant min.labels max.timestamp x

-- | (v `op` s) ≡ map (\x -> x `op` s) v
-- | where v : InstantVector Scalar
-- |       s : Scalar
interpBinaryArithmeticOp :: Store s Double
                         => Config
                         -> s
                         -> Map Identifier Value
                         -> Expr
                         -> BinaryArithmeticOp
                         -> Expr
                         -> Timestamp
                         -> QueryM Value
interpBinaryArithmeticOp cfg store env v op k now = do
  nextVarIdx <- lift get
  lift (put (1 + nextVarIdx))
  interp cfg store env
    (Map
      (
        Lambda
          (Machine nextVarIdx)
          (BinaryArithmeticOp.embedScalar op (Variable (Machine nextVarIdx)) k)
      )
      v
    )
    now

-- | (v `R` s) ≡ filter (\x -> x `R` s) v
-- | where v : InstantVector Scalar
-- |       s : Scalar
interpFilterBinaryRelation :: Store s Double
                           => Config
                           -> s
                           -> Map Identifier Value
                           -> Expr
                           -> BinaryRelation
                           -> Expr
                           -> Timestamp
                           -> QueryM Value
interpFilterBinaryRelation cfg store env v rel k now = do
  nextVarIdx <- lift get
  lift (put (1 + nextVarIdx))
  interp cfg store env
    (Filter
      (
        Lambda
          (Machine nextVarIdx)
          (embedScalar rel (Variable (Machine nextVarIdx)) k)
      )
      v
    )
    now

-- | Given a metric store, an assignment of values to local variables, a query expression and a timestamp "now",
--    interpret the `Expr` into a `Value`.
interp :: Store s Double => Config -> s -> Map Identifier Value -> Expr -> Timestamp -> QueryM Value
interp _ _ _ (Expr.Number x) _ = do
  pure (Value.Scalar x)
interp _ store env (Expr.Variable x) _ =
  case Map.lookup x env of
    Just v -> pure v
    Nothing ->
      case x of
        User u | member u (metrics store) ->
          pure $ Value.Function (interpVariable store u)
        _ ->
          throwQueryError $ "Undefined variable: " <> showT x
interp _ _ _ Now now = pure (Timestamp (fromIntegral now))
interp _ _ _ Epoch _ = pure (Timestamp 0)
interp cfg store env (Lambda x body) now = pure $ Value.Function $ \v ->
  interp cfg store (Map.insert x v env) body now
interp cfg store env (Let x rhs body) now = do
  v <- interp cfg store env rhs now
  interp cfg store (Map.insert x v env) body now
interp cfg store env (FastForward t_ d_) now = do
  t <- interp cfg store env t_ now >>= expectTimestamp
  d <- interp cfg store env d_ now >>= expectDuration
  pure (Value.Timestamp (t + d))
interp cfg store env (FilterByLabel cs s_) now = do
  s <- interp cfg store env s_ now >>= expectInstantVector
  let (mustBe, mustNotBe) = interpLabelInsts cs
  pure $
    Value.InstantVector $
     flip filter s $ \i ->
       (&&)
         (mustBe `isSubsetOf` i.labels)
         (Set.null (mustNotBe `Set.intersection` i.labels))
interp cfg store env (Unless u_ v_) now = do
  u <- interp cfg store env u_ now >>= expectInstantVector
  v <- interp cfg store env v_ now >>= expectInstantVector
  let vls = Set.fromList (map (.labels) v)
  pure (Value.InstantVector (filter (\i -> not (member i.labels vls)) u))
interp cfg store env (Filter f_ t_) now = do
  f <- interp cfg store env f_ now >>= expectFunction
  t <- interp cfg store env t_ now >>= expectInstantVector
  Value.InstantVector <$> interpFilter f t
interp cfg store env (Join a_ b_) now = do
  a <- interp cfg store env a_ now >>= expectInstantVector
  b <- interp cfg store env b_ now >>= expectInstantVector
  Value.InstantVector <$> liftEither (interpJoin Value.Pair a b)
interp cfg store env (Map f_ x_) now = do
  f <- interp cfg store env f_ now >>= expectFunction
  x <- interp cfg store env x_ now >>= expectInstantVector
  Value.InstantVector <$> interpMap f x
interp cfg store env (Range s_ a_ b_ r_) now = do
  s <- interp cfg store env s_ now >>= expectFunction
  a <- interp cfg store env a_ now >>= expectTimestamp
  b <- interp cfg store env b_ now >>= expectTimestamp
  r <- traverse (\r' -> interp cfg store env r' now >>= expectDuration) r_
  RangeVector <$> interpRange s (Interval a b) (fromMaybe cfg.defaultRangeSamplingRateMillis r)
interp cfg store env (Rewind t_ d_) now = do
  t <- interp cfg store env t_ now >>= expectTimestamp
  d <- interp cfg store env d_ now >>= expectDuration
  pure (Timestamp (t - d))
interp cfg store env (BoolToScalar t_) now = do
  t <- interp cfg store env t_ now >>= expectBoolean
  pure (Scalar (if t then 1 else 0))
interp cfg store env (InstantVectorToScalar t_) now = do
  t <- interp cfg store env t_ now >>= expectInstantVectorBool
  pure (Value.InstantVector (fmap (\x -> Value.Scalar (if x then 1.0 else 0.0)) <$> t))
interp cfg store env (TimestampToScalar t_) now = do
  t <- interp cfg store env t_ now >>= expectTimestamp
  pure (Scalar (fromIntegral t))
interp cfg store env (DurationToScalar t_) now = do
  t <- interp cfg store env t_ now >>= expectDuration
  pure (Scalar (fromIntegral t))
interp _ _ _ (Milliseconds t) _ = pure $ Duration t
interp _ _ _ (Seconds t) _ = pure $ Duration (1000 * t)
interp _ _ _ (Minutes t) _ = pure $ Duration (60 * 1000 * t)
interp _ _ _ (Hours t) _ = pure $ Duration (60 * 60 * 1000 * t)
interp cfg store env (BinaryArithmeticOp.mbBinaryArithmeticOpInstantVectorScalar -> Just (v, op, k)) now = do
  interpBinaryArithmeticOp cfg store env v op k now
interp cfg store env (Quantile k_ expr) now = do
  k <- interp cfg store env k_ now >>= expectScalar
  v <- interp cfg store env expr now >>= expectInstantVectorScalar
  pure $ Value.Scalar $ quantile cadpw (floor (k * 100)) 100 (Instant.toVector v)
interp cfg store env (QuantileBy ls k_ expr) now = do
  k <- interp cfg store env k_ now >>= expectScalar
  v <- interp cfg store env expr now >>= expectInstantVectorScalar
  Value.InstantVector . fmap (fmap Value.Scalar) <$> interpQuantileBy ls k v now
interp cfg store env (QuantileOverTime k_ expr) now = do
  k <- interp cfg store env k_ now >>= expectScalar
  v <- interp cfg store env expr now >>= expectRangeVectorScalar
  pure $ Value.InstantVector (fmap Value.Scalar <$> quantileRangeVector k v)
interp cfg store env (Rate r_) now = do
  r <- interp cfg store env r_ now >>= expectRangeVectorScalar
  -- TODO: PromQL's rate() performs linear regression to extrapolate the samples to the bounds
  r' <- interpRate r
  pure (Value.InstantVector (fmap (fmap Value.Scalar) r'))
interp cfg store env (Increase r_) now = do
  r <- interp cfg store env r_ now >>= expectRangeVectorScalar
  -- TODO: PromQL's increase() performs linear regression to extrapolate the samples to the bounds
  r' <- interpIncrease r
  pure (Value.InstantVector (fmap (fmap Value.Scalar) r'))
interp cfg store env (Avg expr) now = do
  v <- interp cfg store env expr now >>= expectInstantVectorScalar
  pure $ Value.Scalar $ mean (Instant.toVector v)
interp cfg store env (Max expr) now = do
  v <- interp cfg store env expr now >>= expectInstantVectorScalar
  pure $ Value.Scalar $ snd $ minMax (Instant.toVector v)
interp cfg store env (Min expr) now = do
  v <- interp cfg store env expr now >>= expectInstantVectorScalar
  pure $ Value.Scalar $ fst $ minMax (Instant.toVector v)
interp cfg store env (AvgOverTime expr) now = do
  v <- interp cfg store env expr now >>= expectRangeVectorScalar
  pure $ Value.InstantVector (fmap Value.Scalar <$> avgOverTime now v)
interp cfg store env (SumOverTime expr) now = do
  v <- interp cfg store env expr now >>= expectRangeVectorScalar
  pure $ Value.InstantVector (fmap Value.Scalar <$> sumOverTime now v)
interp cfg store env (MkPair a b) now = do
  va <- interp cfg store env a now
  vb <- interp cfg store env b now
  pure $ Value.Pair va vb
interp cfg store env (Fst t) now = do
  (a, _) <- interp cfg store env t now >>= expectPair
  pure a
interp cfg store env (Snd t) now = do
  (_, b) <- interp cfg store env t now >>= expectPair
  pure b
interp _ _ _ Expr.True _ = do
  pure Truth
interp _ _ _ Expr.False _ = do
  pure Falsity
interp cfg store env (Expr.And a b) now = do
  va <- interp cfg store env a now >>= expectBoolean
  vb <- interp cfg store env b now >>= expectBoolean
  pure (fromBool (va && vb))
interp cfg store env (Expr.Or a b) now = do
  va <- interp cfg store env a now >>= expectBoolean
  vb <- interp cfg store env b now >>= expectBoolean
  pure (fromBool (va || vb))
interp cfg store env (Expr.Not t_) now = do
  vt <- interp cfg store env t_ now >>= expectBoolean
  pure (fromBool (not vt))
interp cfg store env (Expr.EqBool a b) now = do
  va <- interp cfg store env a now >>= expectBoolean
  vb <- interp cfg store env b now >>= expectBoolean
  pure (fromBool (va == vb))
interp cfg store env (Expr.NotEqBool a b) now = do
  va <- interp cfg store env a now >>= expectBoolean
  vb <- interp cfg store env b now >>= expectBoolean
  pure (fromBool (va /= vb))
interp cfg store env (mbBinaryRelationScalar -> Just (a, rel, b)) now = do
  va <- interp cfg store env a now >>= expectScalar
  vb <- interp cfg store env b now >>= expectScalar
  pure (fromBool (BinaryRelation.materializeScalar rel va vb))
interp cfg store env (BinaryArithmeticOp.mbBinaryArithmeticOpScalar -> Just (a, op, b)) now = do
  va <- interp cfg store env a now >>= expectScalar
  vb <- interp cfg store env b now >>= expectScalar
  pure (Value.Scalar (BinaryArithmeticOp.materializeScalar op va vb))
interp cfg store env (Expr.Abs x_) now = do
  x <- interp cfg store env x_ now >>= expectScalar
  pure (Value.Scalar (abs x))
interp cfg store env (Expr.RoundScalar x_) now = do
  x <- interp cfg store env x_ now >>= expectScalar
  pure (Value.Scalar (fromIntegral (round x :: Int)))
interp cfg store env (Application f_ e_) now = do
  f <- interp cfg store env f_ now >>= expectFunction
  e <- interp cfg store env e_ now
  f e
interp cfg store env (Expr.AddDuration a_ b_) now = do
  a <- interp cfg store env a_ now >>= expectDuration
  b <- interp cfg store env b_ now >>= expectDuration
  pure (Value.Duration (a + b))
interp cfg store env (mbBinaryRelationInstantVector -> Just (v, rel, k)) now =
  interpFilterBinaryRelation cfg store env v rel k now
interp _ _ _ expr _ = throwQueryError $ "Can't interpret expression: " <> showT expr
