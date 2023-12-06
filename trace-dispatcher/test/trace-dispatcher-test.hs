{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

import           Prelude hiding (readFile, writeFile)

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text, breakOn, replace, stripEnd)
import           Data.Text.Encoding
import           Data.Text.IO (readFile)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Oracles
import           Cardano.Logging.Test.Script
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Unit.Aggregation
import           Cardano.Logging.Test.Unit.Configuration
import           Cardano.Logging.Test.Unit.DataPoint
import           Cardano.Logging.Test.Unit.Documentation
import           Cardano.Logging.Test.Unit.EKG
import           Cardano.Logging.Test.Unit.FrequencyLimiting
import           Cardano.Logging.Test.Unit.Routing
import           Cardano.Logging.Test.Unit.Trivial


main :: IO ()
main = defaultMain tests

-- Add unitTests to the main test group
tests :: TestTree
tests = testGroup "Tests"
  [ unitTests
  , localTests
  ]

unitTests :: TestTree
unitTests = testGroup "trace-dispatcher-unit-tests"
    [
        testCase "testTrivial1" $ do
        res <- test1
        bres <- testLoggingMessagesEq res test1Res
        assertBool "testTrivial1" bres
    , testCase "testTrivial2" $ do
        res <- test2
        bres <- testLoggingMessagesEq res test2Res
        assertBool "testTrivial2" bres
    , testCase "testAggregation" $ do
        res <- testAggregation
        bres <- testLoggingMessagesEq res testAggResult
        assertBool "testAggregation" bres
    , testCase "testRouting" $ do
        res <- testRouting
        bres <- testLoggingMessagesEq res testRoutingResult
        assertBool "testRouting" bres
    , testCase "testConfig" $ do
        res <- testConfig
        bres <- testLoggingMessagesEq res testConfigResult
        assertBool "testConfig" bres
#ifdef linux_HOST_OS
    , testCase "testDocGeneration" $ do
        actual <- docTracers
        expected <- readFile "test/data/docGeneration.md"
        let actual' = fst $ breakOn "Configuration:" actual
        assertEqual "testDocGeneration"
            (stripEnd expected)
            (stripEnd actual')
#endif
    , testCase "testEKG" $ do
        res <- testEKG
        assertBool "testEKG" (res == 1000)
    , testCase "testDatapoint" $ do
        res <- testDataPoint
        assertBool "testDatapoint" (show res == testDataPointResult)
    , testCase "testLimiting" $ do
        _res <- testLimiting
        assertBool "testLimiting" True -- currently not verified
    ]

localTests :: TestTree
localTests = localOption (QuickCheckTests 10) $ testGroup "trace-dispatcher"
    [ testProperty "single-threaded send tests" $
        runScriptSimple 1.0 oracleMessages
    , testProperty "multi-threaded send tests" $
        runScriptMultithreaded 1.0 oracleMessages
    --  , testProperty "multi-threaded send tests with reconfiguration" $
    --      runScriptMultithreadedWithReconfig 1.0 oracleMessages
    , testProperty "reconfiguration stress test" $
        runScriptMultithreadedWithConstantReconfig 1.0 (\ _ _ -> property True)
    ]



