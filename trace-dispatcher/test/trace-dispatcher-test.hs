{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

import           Data.Text (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Oracles
import           Cardano.Logging.Test.Script

import           Cardano.Logging.Test.Unit.Trivial


main :: IO ()
main = defaultMain tests

-- Add unitTests to the main test group
tests :: TestTree
tests = testGroup "Tests"
  [ localTests
  , unitTests
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


unitTests :: TestTree
unitTests = testGroup "trace-dispatcher-unit-tests"
    [ testCase "test1" $ do
        res <- test1
        assertEqual "" res test1Res
    -- , testCase "multi-threaded send tests" $
    --     runScriptMultithreaded 1.0 oracleMessages
    -- , testCase "reconfiguration stress test" $
    --     runScriptMultithreadedWithConstantReconfig 1.0 (\ _ _ -> assertBool "Always true" True)
    -- , testGroup "Unit tests" $
    --     [ testCase "some unit test" someUnitTestFunction
    --     -- Add more testCase here for each test case you want to include
    --     ]
    ]

test1Res :: [Text]
test1Res = ["{\"at\":\"2023-11-22T15:04:24.03979674Z\",\"ns\":\"Outer1.Inner2.Inner3\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":2002},\"sev\":\"Critical\",\"thread\":\"76\",\"host\":\"deusXmachina\"}","{\"at\":\"2023-11-22T15:04:24.039794343Z\",\"ns\":\"Outer1.Inner1\",\"data\":{\"current slot\":4400,\"kind\":\"TraceBlockFromFuture\",\"tip\":300},\"sev\":\"Info\",\"thread\":\"76\",\"host\":\"deusXmachina\"}","{\"at\":\"2023-11-22T15:04:24.039793997Z\",\"ns\":\"Outer1.Inner2\",\"data\":{\"kind\":\"TraceSlotIsImmutable\",\"slot\":3333,\"tip\":\"Origin\",\"tipBlockNo\":1},\"sev\":\"Warning\",\"thread\":\"76\",\"host\":\"deusXmachina\"}","{\"at\":\"2023-11-22T15:04:24.039787567Z\",\"ns\":\"Outer1.Inner1\",\"data\":{\"kind\":\"TraceStartLeadershipCheck\",\"slot\":1001},\"sev\":\"Error\",\"thread\":\"76\",\"host\":\"deusXmachina\"}"]
