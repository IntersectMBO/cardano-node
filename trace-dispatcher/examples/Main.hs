module Main (
    main
) where

import           Examples.Aggregation
import           Examples.Configuration
import           Examples.DataPoint
import           Examples.Documentation
import           Examples.EKG
import           Examples.FrequencyLimiting
import           Examples.Routing
import           Examples.Trivial


main :: IO ()
main = do
    putStrLn "test1"
    test1
    -- putStrLn "test2"
    -- test2
    putStrLn "testAggregation"
    testAggregation
    putStrLn "testRouting"
    testRouting
    putStrLn "testConfig"
    testConfig
    putStrLn "testLimiting"
    testLimiting
    putStrLn "docTracers"
    docTracers
    putStrLn "testEKG"
    testEKG
    putStrLn "testDataPoint"
    testDataPoint
