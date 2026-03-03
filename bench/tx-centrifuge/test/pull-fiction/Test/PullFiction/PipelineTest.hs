{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

module Test.PullFiction.PipelineTest
  ( pipelineTests
  ) where

--------------------------------------------------------------------------------

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

pipelineTests :: Double -> Tasty.TestTree
pipelineTests dur = Tasty.testGroup "pipeline"
  [
  -- Pipeline test 1: single-group, per-group input queue.
  -- -------------------------------------------------
  --
  -- 1 workload with 50 targets sharing one input queue. Recycled inputs
  -- return to the same queue. Exercises Runtime.resolve with 1 workload,
  -- verifying that closed-loop recycling delivers tokens to every target and
  -- inputs stay within the workload.

    HUnit.testCase
      ("Single-group pipeline (50 targets, " ++ Harness.formatDuration dur ++ ")") $ do
      path <- Paths.getDataFileName "data/config-per-target-0_2.json"
      Harness.runPipelineIsolationTest path 1 dur

  -- Pipeline test 2: multi-group, per-group input queues.
  -- ----------------------------------------------------
  --
  -- 50 workloads, each with 1 target at 1 TPS (50 TPS aggregate).
  -- Each workload has its own input queue; recycled inputs must return to the
  -- originating workload's queue and never leak to another group.
  --
  -- Inputs are tagged with (workloadIndex, inputIndex) tuples. If any worker
  -- ever observes an input with a foreign workload tag, the test fails
  -- immediately. This also exercises resolve's partition logic.

  , HUnit.testCase
      "Multi-group pipeline isolation (50 groups x 1 TPS, 10s)" $ do
      path <- Paths.getDataFileName "data/config-multi-group.json"
      Harness.runPipelineIsolationTest path 50 10

  ]
