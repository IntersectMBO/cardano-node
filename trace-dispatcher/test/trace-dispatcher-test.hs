{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

import           Data.Aeson
import           Data.Text (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Oracles
import           Cardano.Logging.Test.Script
import           Cardano.Logging.Test.Tracer
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
        res2 <- test2
        assertBool "trivial2" (testLoggingMessagesEq res2 test2Res)

    -- , testCase "multi-threaded send tests" $
    --     runScriptMultithreaded 1.0 oracleMessages
    -- , testCase "reconfiguration stress test" $
    --     runScriptMultithreadedWithConstantReconfig 1.0 (\ _ _ -> assertBool "Always true" True)
    -- , testGroup "Unit tests" $
    --     [ testCase "some unit test" someUnitTestFunction
    --     -- Add more testCase here for each test case you want to include
    --     ]
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



