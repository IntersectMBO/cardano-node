module Cardano.Timeseries.Interp.Suite (interpTests) where

import           Cardano.Timeseries.API           (ExecutionError, execute)
import           Cardano.Timeseries.Interp.Config (Config (..))
import           Cardano.Timeseries.Interp.Value  (Value (..))
import           Cardano.Timeseries.Store.Flat    (Flat)

import           Data.Text                        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

emptyStore :: Flat Double
emptyStore = []

defaultCfg :: Config
defaultCfg = Config { defaultRangeSamplingRateMillis = 1000 }

-- | Run a query at timestamp 0 against an empty metric store.
run :: Text -> Either ExecutionError Value
run = execute emptyStore defaultCfg 0

assertScalar :: Double -> Either ExecutionError Value -> Assertion
assertScalar expected (Right (Scalar actual)) = actual @?= expected
assertScalar _        (Left err)              = assertFailure $ "Expected Scalar, got error: " <> show err
assertScalar _        (Right other)           = assertFailure $ "Expected Scalar, got: "       <> show other

interpTests :: TestTree
interpTests = testGroup "Interpretation"
  [ testGroup "Scalar arithmetic"
      [ testCase "1 + 2 = 3"    $ assertScalar 3   (run "1 + 2")
      , testCase "5 - 3 = 2"    $ assertScalar 2   (run "5 - 3")
      , testCase "2 * 3 = 6"    $ assertScalar 6   (run "2 * 3")
      , testCase "10 / 4 = 2.5" $ assertScalar 2.5 (run "10 / 4")
      ]
  , testGroup "Boolean literals"
      [ testCase "true" $
          case run "true" of
            Right Truth  -> pure ()
            other        -> assertFailure $ "Expected Truth, got: " <> show other
      , testCase "false" $
          case run "false" of
            Right Falsity -> pure ()
            other         -> assertFailure $ "Expected Falsity, got: " <> show other
      ]
  , testGroup "Timestamps"
      [ testCase "epoch evaluates to 0" $
          case run "epoch" of
            Right (Timestamp t) -> t @?= 0
            other               -> assertFailure $ "Expected Timestamp 0, got: " <> show other
      , testCase "now evaluates to query time" $
          case run "now" of
            Right (Timestamp t) -> t @?= 0
            other               -> assertFailure $ "Expected Timestamp 0, got: " <> show other
      ]
  , testGroup "Let bindings"
      [ testCase "let x = 2 in x * 3" $ assertScalar 6 (run "let x = 2 in x * 3")
      ]
  ]
