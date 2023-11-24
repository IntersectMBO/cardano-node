{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text, breakOn, stripEnd)
import           Data.Text.Encoding (decodeUtf8)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Oracles
import           Cardano.Logging.Test.Script
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Unit.Aggregation
import           Cardano.Logging.Test.Unit.Documentation
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
    [ testCase "test1" $ do
        res <- test1
        assertBool "trivial1" (testLoggingMessagesEq res test1Res)
    , testCase "test2" $ do
        res <- test2
        assertBool "trivial2" (testLoggingMessagesEq res test2Res)
    , testCase "testAggregation" $ do
        res <- testAggregation
        assertBool "testAggregation" (testLoggingMessagesEq res testAggResult)
    , testCase "testRouting" $ do
        res <- testRouting
        assertBool "testRouting" (testLoggingMessagesEq res testRoutingResult)
    -- , testCase "testLimiting" $ do
    --     res <- testLimiting
    --     assertBool "testLimiting" (testLoggingMessagesEq res testLimitingResult)
    , testCase "testDocGeneration" $ do
        res <- docTracers
        let res' = fst $ breakOn "Generated at" res
            docuFile = "test/data/docGeneration.md"
        testDocResult <- BS.readFile docuFile
        assertEqual "testDocGeneration"
            (stripEnd res')
            (stripEnd (decodeUtf8 testDocResult))
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



