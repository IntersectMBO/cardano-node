module Main where

import           Examples.Aggregation
import           Examples.EKG
import           Examples.Routing
import           Examples.Trivial
import           Examples.Configuration
import           Cardano.Logging



main :: IO ()
main = do
    test1
    test2
    testAggregation
    testRouting
    testConfig
--    testEKG
