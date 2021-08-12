
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
    [ testProperty "not-filtered" $
        runScriptSimple 1.0 oracleFiltering
    , testProperty "not-filtered multithreaded" $  
        runScriptMultithreaded 1.0 oracleFiltering
    ]
