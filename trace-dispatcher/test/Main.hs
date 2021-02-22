module Main where

import           Cardano.Logging
import           Examples.Aggregation
import           Examples.Configuration
import           Examples.EKG
import           Examples.Routing
import           Examples.Trivial



main :: IO ()
main = do
    test1
    test2
    testAggregation
    testRouting
    testConfig
    testEKG
