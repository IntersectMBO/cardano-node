{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

module Test.PullFiction.GeneratorTest
  ( generatorTests
  ) where

--------------------------------------------------------------------------------

----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
-----------
-- tasty --
-----------
import Test.Tasty qualified as Tasty
-----------------
-- tasty-hunit --
-----------------
import Test.Tasty.HUnit qualified as HUnit
---------------------
-- pull-fiction --
---------------------
import Paths_tx_centrifuge qualified as Paths
import Test.PullFiction.Harness qualified as Harness

--------------------------------------------------------------------------------

generatorTests :: Double -> Tasty.TestTree
generatorTests duration = Tasty.testGroup "TPS"
  [ -- A "shared" global rate limiter.
    tpsTestGroup "Shared-limiter mode (50 targets, 10 TPS"
      "data/config-shared-10.json"
      (Map.fromList
        [ (Harness.targetName "default" (Harness.nodeName i), 0.2)
        | i <- [1..50]
        ]
      )
      duration
      0.05
      0.15
  , tpsTestGroup "Shared-limiter mode (50 targets, 100k TPS"
      "data/config-shared-100k.json"
      (Map.fromList
        [ (Harness.targetName "default" (Harness.nodeName i), 2_000)
        | i <- [1..50]
        ]
      )
      duration
      0.05
      0.15
    -- A "per_target" scoped rate limiter. Lower per-target tolerance.
  , tpsTestGroup "Per-target-limiter mode (50 targets, 0.2 TPS/target"
      "data/config-per-target-0_2.json"
      (Map.fromList
        [ (Harness.targetName "default" (Harness.nodeName i), 0.20)
        | i <- [1..50]
        ]
      )
      duration
      0.05
      0.05
  , tpsTestGroup "Per-target-limiter mode (50 targets, 2k TPS/target"
      "data/config-per-target-2k.json"
      (Map.fromList
        [(Harness.targetName "default" (Harness.nodeName i), 2_000)
        | i <- [1..50]
        ]
      )
      duration
      0.05
      0.05
  ]

tpsTestGroup
  :: String                -- ^ Test group label (duration is appended).
  -> String                -- ^ Data-file config name.
  -> Map.Map String Double -- ^ Expected TPS per target, keyed by name.
  -> Double                -- ^ Test duration in seconds.
  -> Double                -- ^ Global TPS tolerance.
  -> Double                -- ^ Per-target fairness tolerance.
  -> Tasty.TestTree
tpsTestGroup label configName expectedMap duration globalTol fairnessTol =
  Tasty.withResource
    (do path <- Paths.getDataFileName configName
        Harness.runTpsTest path duration
    )
    (const $ pure ())
    $ \getResult ->
      Tasty.testGroup
        (label ++ ", " ++ Harness.formatDuration duration ++ ")")
        [ -- Total elapsed time.
          HUnit.testCase
            ("Elapsed time within "
              ++ show (round (globalTol * 100) :: Int) ++ "% of target"
            ) $ do
              result <- getResult
              case Harness.checkElapsedTolerance globalTol duration result of
                Nothing  -> pure ()
                Just err -> HUnit.assertFailure err
          -- Total TPS.
        , HUnit.testCaseInfo
            ("Global TPS within "
              ++ show (round (globalTol * 100) :: Int) ++ "% tolerance"
            ) $ do
              result <- getResult
              let metrics = Harness.formatMetrics duration expectedMap result
              case Harness.checkTpsTolerance globalTol expectedMap result of
                Nothing  -> pure metrics
                Just err -> HUnit.assertFailure
                    (err ++ "\n" ++ metrics)
          -- TPS per target.
        , Tasty.testGroup
            ("Per-target TPS within "
              ++ show (round (fairnessTol * 100) :: Int) ++ "% of expected"
            )
            [ HUnit.testCase name $ do
                result <- getResult
                case Harness.checkTargetFairness fairnessTol expectedMap result name of
                  Nothing  -> pure ()
                  Just err -> HUnit.assertFailure err
            | name <- Map.keys expectedMap
            ]
        ]
