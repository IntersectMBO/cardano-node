module Cardano.Timeseries.Interp.Suite (interpTests) where

import           Cardano.Timeseries.API           (Config (..), ExecutionError (..), execute)
import           Cardano.Timeseries.Domain.Instant (Instant (..))
import           Cardano.Timeseries.Interp.Value  (Value (..))
import           Cardano.Timeseries.Store.Flat    (Flat, Point (..))

import           Data.List                        (sort)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Word                        (Word64)
import           Test.Tasty
import           Test.Tasty.HUnit

-- ---------------------------------------------------------------------------
-- Stores
-- ---------------------------------------------------------------------------

emptyStore :: Flat Double
emptyStore = []

-- One metric "m", single series, value 42.0 at t=0.
simpleStore :: Flat Double
simpleStore =
  [ Point { name = "m", instant = Instant { labels = Set.empty, timestamp = 0, value = 42.0 } }
  ]

-- Three series for metric "m" with values 10, 20, 30 and distinct labels.
multiStore :: Flat Double
multiStore =
  [ Point { name = "m", instant = Instant { labels = Set.fromList [("k", "1")], timestamp = 0, value = 10.0 } }
  , Point { name = "m", instant = Instant { labels = Set.fromList [("k", "2")], timestamp = 0, value = 20.0 } }
  , Point { name = "m", instant = Instant { labels = Set.fromList [("k", "3")], timestamp = 0, value = 30.0 } }
  ]

-- One metric "m", one series, with two time points for range-based tests.
-- Points at t=0 (value=0.0) and t=1000 (value=1.0).
rateStore :: Flat Double
rateStore =
  [ Point { name = "m", instant = Instant { labels = Set.empty, timestamp = 0,    value = 0.0 } }
  , Point { name = "m", instant = Instant { labels = Set.empty, timestamp = 1000, value = 1.0 } }
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

defaultCfg :: Config
defaultCfg = Config { defaultRangeSamplingRateMillis = 1000 }

run :: Text -> Either ExecutionError Value
run = execute emptyStore defaultCfg 0

runAt :: Flat Double -> Word64 -> Text -> Either ExecutionError Value
runAt store t = execute store defaultCfg t

assertScalar :: Double -> Either ExecutionError Value -> Assertion
assertScalar expected (Right (Scalar actual)) = actual @?= expected
assertScalar _        (Left err)              = assertFailure $ "Expected Scalar, got error: " <> show err
assertScalar _        (Right other)           = assertFailure $ "Expected Scalar, got: "       <> show other

assertBoolVal :: Bool -> Either ExecutionError Value -> Assertion
assertBoolVal True  (Right Truth)   = pure ()
assertBoolVal False (Right Falsity) = pure ()
assertBoolVal b     (Left err)      = assertFailure $ "Expected Bool " <> show b <> ", got error: " <> show err
assertBoolVal b     (Right other)   = assertFailure $ "Expected Bool " <> show b <> ", got: "       <> show other

assertDuration :: Word64 -> Either ExecutionError Value -> Assertion
assertDuration expected (Right (Duration actual)) = actual @?= expected
assertDuration _        (Left err)               = assertFailure $ "Expected Duration, got error: " <> show err
assertDuration _        (Right other)            = assertFailure $ "Expected Duration, got: "       <> show other

assertTimestamp :: Word64 -> Either ExecutionError Value -> Assertion
assertTimestamp expected (Right (Timestamp actual)) = actual @?= expected
assertTimestamp _        (Left err)                = assertFailure $ "Expected Timestamp, got error: " <> show err
assertTimestamp _        (Right other)             = assertFailure $ "Expected Timestamp, got: "       <> show other

-- | Assert the set of scalar values in an instant vector (order-insensitive).
assertInstantVectorScalars :: [Double] -> Either ExecutionError Value -> Assertion
assertInstantVectorScalars expected (Right (InstantVector actual)) =
  let vs = sort [ x | Instant { value = Scalar x } <- actual ]
  in  sort expected @?= vs
assertInstantVectorScalars _ (Left err)    = assertFailure $ "Expected InstantVector, got error: " <> show err
assertInstantVectorScalars _ (Right other) = assertFailure $ "Expected InstantVector, got: "       <> show other

assertSucceeds :: Either ExecutionError Value -> Assertion
assertSucceeds (Right _)  = pure ()
assertSucceeds (Left err) = assertFailure $ "Expected success, got error: " <> show err

-- ---------------------------------------------------------------------------
-- Test groups
-- ---------------------------------------------------------------------------

interpTests :: TestTree
interpTests = testGroup "Interpretation"
  [ scalarArithmeticTests
  , scalarComparisonTests
  , booleanLiteralTests
  , booleanOperationTests
  , boolComparisonTests
  , durationLiteralTests
  , timestampTests
  , typeConversionTests
  , pairTests
  , letLambdaTests
  , instantVectorLookupTests
  , instantVectorAggregationTests
  , instantVectorOperationTests
  , instantVectorScalarArithTests
  , instantVectorScalarRelTests
  , rangeVectorTests
  , metricsTests
  , errorCaseTests
  ]

-- ---------------------------------------------------------------------------
-- Scalar arithmetic
-- ---------------------------------------------------------------------------

scalarArithmeticTests :: TestTree
scalarArithmeticTests = testGroup "Scalar arithmetic"
  [ testCase "1 + 2 = 3"    $ assertScalar 3    (run "1 + 2")
  , testCase "5 - 3 = 2"    $ assertScalar 2    (run "5 - 3")
  , testCase "2 * 3 = 6"    $ assertScalar 6    (run "2 * 3")
  , testCase "10 / 4 = 2.5" $ assertScalar 2.5  (run "10 / 4")
  , testCase "chained: 1 + 2 * 3 = 7" $ assertScalar 7 (run "1 + 2 * 3")
  , testCase "negation in expression: 0 - 5 = -5" $ assertScalar (-5) (run "0 - 5")
  ]

-- ---------------------------------------------------------------------------
-- Scalar comparison
-- ---------------------------------------------------------------------------

scalarComparisonTests :: TestTree
scalarComparisonTests = testGroup "Scalar comparison"
  [ testCase "1 == 1 = true"  $ assertBoolVal True  (run "1 == 1")
  , testCase "1 == 2 = false" $ assertBoolVal False (run "1 == 2")
  , testCase "1 != 2 = true"  $ assertBoolVal True  (run "1 != 2")
  , testCase "1 != 1 = false" $ assertBoolVal False (run "1 != 1")
  , testCase "1 < 2 = true"   $ assertBoolVal True  (run "1 < 2")
  , testCase "2 < 1 = false"  $ assertBoolVal False (run "2 < 1")
  , testCase "1 <= 1 = true"  $ assertBoolVal True  (run "1 <= 1")
  , testCase "2 <= 1 = false" $ assertBoolVal False (run "2 <= 1")
  , testCase "2 > 1 = true"   $ assertBoolVal True  (run "2 > 1")
  , testCase "1 > 2 = false"  $ assertBoolVal False (run "1 > 2")
  , testCase "1 >= 1 = true"  $ assertBoolVal True  (run "1 >= 1")
  , testCase "1 >= 2 = false" $ assertBoolVal False (run "1 >= 2")
  ]

-- ---------------------------------------------------------------------------
-- Boolean literals
-- ---------------------------------------------------------------------------

booleanLiteralTests :: TestTree
booleanLiteralTests = testGroup "Boolean literals"
  [ testCase "true" $
      case run "true" of
        Right Truth  -> pure ()
        other        -> assertFailure $ "Expected Truth, got: " <> show other
  , testCase "false" $
      case run "false" of
        Right Falsity -> pure ()
        other         -> assertFailure $ "Expected Falsity, got: " <> show other
  ]

-- ---------------------------------------------------------------------------
-- Boolean operations
-- ---------------------------------------------------------------------------

booleanOperationTests :: TestTree
booleanOperationTests = testGroup "Boolean operations"
  [ testCase "true && true = true"    $ assertBoolVal True  (run "true && true")
  , testCase "true && false = false"  $ assertBoolVal False (run "true && false")
  , testCase "false && true = false"  $ assertBoolVal False (run "false && true")
  , testCase "false && false = false" $ assertBoolVal False (run "false && false")
  , testCase "false || true = true"   $ assertBoolVal True  (run "false || true")
  , testCase "true || false = true"   $ assertBoolVal True  (run "true || false")
  , testCase "false || false = false" $ assertBoolVal False (run "false || false")
  , testCase "!true = false"          $ assertBoolVal False (run "!true")
  , testCase "!false = true"          $ assertBoolVal True  (run "!false")
  ]

-- ---------------------------------------------------------------------------
-- Bool comparison
-- ---------------------------------------------------------------------------

boolComparisonTests :: TestTree
boolComparisonTests = testGroup "Bool comparison"
  [ testCase "true == true = true"   $ assertBoolVal True  (run "true == true")
  , testCase "true == false = false" $ assertBoolVal False (run "true == false")
  , testCase "true != false = true"  $ assertBoolVal True  (run "true != false")
  , testCase "false != false = false"$ assertBoolVal False (run "false != false")
  ]

-- ---------------------------------------------------------------------------
-- Duration literals
-- ---------------------------------------------------------------------------

durationLiteralTests :: TestTree
durationLiteralTests = testGroup "Duration literals"
  [ testCase "1ms = 1"       $ assertDuration 1       (run "1ms")
  , testCase "1s = 1000"     $ assertDuration 1000    (run "1s")
  , testCase "1m = 60000"    $ assertDuration 60000   (run "1m")
  , testCase "1h = 3600000"  $ assertDuration 3600000 (run "1h")
  , testCase "2s = 2000"     $ assertDuration 2000    (run "2s")
  , testCase "60s = 60000"   $ assertDuration 60000   (run "60s")
  ]

-- ---------------------------------------------------------------------------
-- Timestamps
-- ---------------------------------------------------------------------------

timestampTests :: TestTree
timestampTests = testGroup "Timestamps"
  [ testCase "now at t=0"      $ assertTimestamp 0    (runAt emptyStore 0    "now")
  , testCase "now at t=5000"   $ assertTimestamp 5000 (runAt emptyStore 5000 "now")
  , testCase "epoch at t=0"    $ assertTimestamp 0    (runAt emptyStore 0    "epoch")
  , testCase "now - 1s at t=5000" $ assertTimestamp 4000 (runAt emptyStore 5000 "now - 1s")
  , testCase "now + 1s at t=5000" $ assertTimestamp 6000 (runAt emptyStore 5000 "now + 1s")
  , testCase "now - 1m at t=60000" $ assertTimestamp 0 (runAt emptyStore 60000 "now - 1m")
  , testCase "now + 1h at t=0" $ assertTimestamp 3600000 (runAt emptyStore 0 "now + 1h")
  ]

-- ---------------------------------------------------------------------------
-- Type conversions
-- ---------------------------------------------------------------------------

typeConversionTests :: TestTree
typeConversionTests = testGroup "Type conversions"
  [ testCase "to_scalar true = 1.0"   $ assertScalar 1.0    (run "to_scalar true")
  , testCase "to_scalar false = 0.0"  $ assertScalar 0.0    (run "to_scalar false")
  , testCase "to_scalar 1s = 1000.0"  $ assertScalar 1000.0 (run "to_scalar 1s")
  , testCase "to_scalar now at t=5000" $
      assertScalar 5000.0 (runAt emptyStore 5000 "to_scalar now")
  , testCase "abs (-3) = 3.0"          $ assertScalar 3.0  (run "abs (-3)")
  , testCase "abs 3 = 3.0"             $ assertScalar 3.0  (run "abs 3")
  , testCase "round 2.3 = 2.0"         $ assertScalar 2.0  (run "round 2.3")
  , testCase "round 2.7 = 3.0"         $ assertScalar 3.0  (run "round 2.7")
  , testCase "round 2.5 = 2.0 (banker's rounding)" $ assertScalar 2.0 (run "round 2.5")
  , testCase "round 3.5 = 4.0 (banker's rounding)" $ assertScalar 4.0 (run "round 3.5")
  ]

-- ---------------------------------------------------------------------------
-- Pairs and unit
-- ---------------------------------------------------------------------------

pairTests :: TestTree
pairTests = testGroup "Pairs and unit"
  [ testCase "()" $
      case run "()" of
        Right Unit -> pure ()
        other      -> assertFailure $ "Expected Unit, got: " <> show other
  , testCase "fst (1, 2) = 1" $ assertScalar 1.0 (run "fst (1, 2)")
  , testCase "snd (1, 2) = 2" $ assertScalar 2.0 (run "snd (1, 2)")
  , testCase "fst (fst ((1, 2), 3)) = 1" $ assertScalar 1.0 (run "fst (fst ((1, 2), 3))")
  , testCase "snd ((1, 2), 3) = 3"       $ assertScalar 3.0 (run "snd ((1, 2), 3)")
  , testCase "pair structure" $
      case run "(1, 2)" of
        Right (Pair (Scalar 1.0) (Scalar 2.0)) -> pure ()
        other -> assertFailure $ "Expected Pair, got: " <> show other
  ]

-- ---------------------------------------------------------------------------
-- Let and lambda
-- ---------------------------------------------------------------------------

letLambdaTests :: TestTree
letLambdaTests = testGroup "Let and lambda"
  [ testCase "let x = 2 in x * 3 = 6"   $ assertScalar 6 (run "let x = 2 in x * 3")
  , testCase "nested let"                $ assertScalar 3 (run "let x = 1 in let y = 2 in x + y")
  , testCase "let shadowing"             $ assertScalar 2 (run "let x = 1 in let x = 2 in x")
  , testCase "let with lambda rhs"       $ assertScalar 3 (run "let f = \\x -> x + 1 in f 2")
  , testCase "lambda identity"           $ assertScalar 7 (run "(\\x -> x) 7")
  , testCase "lambda arithmetic"         $ assertScalar 5 (run "(\\x -> x + 1) 4")
  , testCase "higher-order: apply twice" $
      assertScalar 5 (run "let f = \\x -> x + 1 in let g = \\x -> f (f x) in g 3")
  ]

-- ---------------------------------------------------------------------------
-- Instant vector lookup
-- ---------------------------------------------------------------------------

instantVectorLookupTests :: TestTree
instantVectorLookupTests = testGroup "Instant vector lookup"
  [ testCase "single-series metric" $
      assertInstantVectorScalars [42.0] (runAt simpleStore 0 "m now")
  , testCase "multi-series metric" $
      assertInstantVectorScalars [10.0, 20.0, 30.0] (runAt multiStore 0 "m now")
  , testCase "points outside staleness window yield empty vector" $
      -- staleness constant = 300000ms; point at t=0 is stale when queried at t=600001
      assertInstantVectorScalars [] (runAt simpleStore 600001 "m now")
  , testCase "point exactly at staleness boundary is visible" $
      assertInstantVectorScalars [42.0] (runAt simpleStore 300000 "m now")
  ]

-- ---------------------------------------------------------------------------
-- Instant vector aggregation
-- ---------------------------------------------------------------------------

instantVectorAggregationTests :: TestTree
instantVectorAggregationTests = testGroup "Instant vector aggregation"
  [ testCase "max [10, 20, 30] = 30" $ assertScalar 30 (runAt multiStore 0 "max (m now)")
  , testCase "min [10, 20, 30] = 10" $ assertScalar 10 (runAt multiStore 0 "min (m now)")
  , testCase "avg [10, 20, 30] = 20" $ assertScalar 20 (runAt multiStore 0 "avg (m now)")
  , testCase "max of singleton = 42" $ assertScalar 42 (runAt simpleStore 0 "max (m now)")
  , testCase "min of singleton = 42" $ assertScalar 42 (runAt simpleStore 0 "min (m now)")
  , testCase "avg of singleton = 42" $ assertScalar 42 (runAt simpleStore 0 "avg (m now)")
  ]

-- ---------------------------------------------------------------------------
-- Instant vector operations (filter, map, label filter, unless, join)
-- ---------------------------------------------------------------------------

instantVectorOperationTests :: TestTree
instantVectorOperationTests = testGroup "Instant vector operations"
  [ testCase "filter: keeps elements satisfying predicate" $
      assertInstantVectorScalars [20.0, 30.0]
        (runAt multiStore 0 "filter (\\x -> x > 15) (m now)")
  , testCase "filter: empty result when nothing matches" $
      assertInstantVectorScalars []
        (runAt multiStore 0 "filter (\\x -> x > 100) (m now)")
  , testCase "filter: all retained when all match" $
      assertInstantVectorScalars [10.0, 20.0, 30.0]
        (runAt multiStore 0 "filter (\\x -> x > 0) (m now)")
  , testCase "map: doubles each value" $
      assertInstantVectorScalars [20.0, 40.0, 60.0]
        (runAt multiStore 0 "map (\\x -> x * 2) (m now)")
  , testCase "map: identity" $
      assertInstantVectorScalars [10.0, 20.0, 30.0]
        (runAt multiStore 0 "map (\\x -> x) (m now)")
  , testCase "filter_by_label eq: keeps matching series" $
      assertInstantVectorScalars [10.0]
        (runAt multiStore 0 "(m now){\"k\" = \"1\"}")
  , testCase "filter_by_label neq: removes matching series" $
      assertInstantVectorScalars [20.0, 30.0]
        (runAt multiStore 0 "(m now){\"k\" != \"1\"}")
  , testCase "unless self: removes all (disjoint result is empty)" $
      assertInstantVectorScalars []
        (runAt multiStore 0 "unless (m now) (m now)")
  , testCase "join self: produces pairs" $
      case runAt multiStore 0 "join (m now) (m now)" of
        Right (InstantVector vs) -> length vs @?= 3
        Left err   -> assertFailure $ "join failed: " <> show err
        Right other -> assertFailure $ "Expected InstantVector, got: " <> show other
  ]

-- ---------------------------------------------------------------------------
-- InstantVector-Scalar arithmetic
-- ---------------------------------------------------------------------------

instantVectorScalarArithTests :: TestTree
instantVectorScalarArithTests = testGroup "Instant vector scalar arithmetic"
  [ testCase "vector + scalar" $
      assertInstantVectorScalars [43.0] (runAt simpleStore 0 "m now + 1")
  , testCase "vector - scalar" $
      assertInstantVectorScalars [40.0] (runAt simpleStore 0 "m now - 2")
  , testCase "vector * scalar" $
      assertInstantVectorScalars [84.0] (runAt simpleStore 0 "m now * 2")
  , testCase "vector / scalar" $
      assertInstantVectorScalars [21.0] (runAt simpleStore 0 "m now / 2")
  ]

-- ---------------------------------------------------------------------------
-- InstantVector-Scalar relations (filtering)
-- ---------------------------------------------------------------------------

instantVectorScalarRelTests :: TestTree
instantVectorScalarRelTests = testGroup "Instant vector scalar relations"
  [ testCase "vector > scalar filters elements" $
      assertInstantVectorScalars [20.0, 30.0] (runAt multiStore 0 "m now > 15")
  , testCase "vector < scalar filters elements" $
      assertInstantVectorScalars [10.0, 20.0] (runAt multiStore 0 "m now < 25")
  , testCase "vector == scalar: exact match" $
      assertInstantVectorScalars [20.0] (runAt multiStore 0 "m now == 20")
  , testCase "vector != scalar: excludes exact match" $
      assertInstantVectorScalars [10.0, 30.0] (runAt multiStore 0 "m now != 20")
  , testCase "vector >= scalar: boundary inclusive" $
      assertInstantVectorScalars [20.0, 30.0] (runAt multiStore 0 "m now >= 20")
  , testCase "vector <= scalar: boundary inclusive" $
      assertInstantVectorScalars [10.0, 20.0] (runAt multiStore 0 "m now <= 20")
  ]

-- ---------------------------------------------------------------------------
-- Range vector operations
-- ---------------------------------------------------------------------------
--
-- rateStore has metric "m", one series, points at t=0 (0.0) and t=1000 (1.0).
-- Query time is 1000. The range expression "m[now - 1s; now : 1s]" produces
-- a two-sample RangeVector: t=0 → 0.0, t=1000 → 1.0.

rangeVectorTests :: TestTree
rangeVectorTests = testGroup "Range vector aggregations"
  [ testCase "avg_over_time of [0.0, 1.0] = 0.5" $
      assertInstantVectorScalars [0.5]
        (runAt rateStore 1000 "avg_over_time (m[now - 1s; now : 1s])")
  , testCase "sum_over_time of [0.0, 1.0] = 1.0" $
      assertInstantVectorScalars [1.0]
        (runAt rateStore 1000 "sum_over_time (m[now - 1s; now : 1s])")
  , testCase "rate of linear increase: 1.0/1000ms = 0.001" $
      assertInstantVectorScalars [0.001]
        (runAt rateStore 1000 "rate (m[now - 1s; now : 1s])")
  , testCase "increase of linear series = 1.0" $
      assertInstantVectorScalars [1.0]
        (runAt rateStore 1000 "increase (m[now - 1s; now : 1s])")
  , testCase "quantile_over_time 0.5 succeeds" $
      assertSucceeds
        (runAt rateStore 1000 "quantile_over_time 0.5 (m[now - 1s; now : 1s])")
  , testCase "range without explicit sampling rate uses default" $
      assertInstantVectorScalars [0.5]
        (runAt rateStore 1000 "avg_over_time (m[now - 1s; now])")
  ]

-- ---------------------------------------------------------------------------
-- metrics
-- ---------------------------------------------------------------------------

metricsTests :: TestTree
metricsTests = testGroup "metrics"
  [ testCase "empty store yields Nil" $
      case run "metrics" of
        Right Nil -> pure ()
        other     -> assertFailure $ "Expected Nil, got: " <> show other
  , testCase "store with one metric yields Cons" $
      case runAt simpleStore 0 "metrics" of
        Right (Cons (Text "m") Nil) -> pure ()
        other                       -> assertFailure $ "Expected Cons (Text \"m\") Nil, got: " <> show other
  ]

-- ---------------------------------------------------------------------------
-- Error cases
-- ---------------------------------------------------------------------------

errorCaseTests :: TestTree
errorCaseTests = testGroup "Error cases"
  [ testCase "undefined name gives Undefined-name elab error" $
      case run "no_such_metric now" of
        Left (ElabErrorWhileExecuting msg) ->
          assertBool "error message mentions the name" ("Undefined name" `Text.isInfixOf` msg)
        other -> assertFailure $ "Expected ElabErrorWhileExecuting, got: " <> show other
  ]
