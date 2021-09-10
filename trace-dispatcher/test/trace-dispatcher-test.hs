
{-# OPTIONS_GHC -Wno-unused-imports  #-}

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Oracles
import           Cardano.Logging.Test.Script


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = localOption (QuickCheckTests 10) $ testGroup "trace-dispatcher"
    [ testProperty "single-threaded send tests" $
        runScriptSimple 1.0 oracleMessages
    , testProperty "multi-threaded send tests" $
        runScriptMultithreaded 1.0 oracleMessages
    -- , testProperty "multi-threaded send tests with reconfiguration" $
    --     runScriptMultithreadedWithReconfig 1.0 oracleMessages
    , testProperty "reconfiguration stress test" $
        runScriptMultithreadedWithConstantReconfig 1.0 (\ _ _ -> property True)
    ]
