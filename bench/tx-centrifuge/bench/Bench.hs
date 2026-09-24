{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

---------------
-- criterion --
---------------
import Criterion.Main qualified as Criterion
-------------
-- deepseq --
-------------
import Control.DeepSeq (NFData (..), deepseq)
---------------------
-- tx-centrifuge --
---------------------
import Paths_tx_centrifuge qualified as Paths
import Test.PullFiction.Harness qualified as Harness

--------------------------------------------------------------------------------

-- | Local wrapper so Criterion can force benchmark results without requiring an
-- NFData instance in the test-harness library.
newtype BenchResult = BenchResult Harness.TestResult

instance NFData BenchResult where
  rnf (BenchResult result) =
    Harness.elapsedSeconds result `seq`
    Harness.targetCounts result `deepseq` ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
  sharedPath    <- Paths.getDataFileName "data/config-shared-100k.json"
  perTargetPath <- Paths.getDataFileName "data/config-per-target-200.json"
  Criterion.defaultMain
    [ Criterion.bgroup "generator-throughput"
        [ Criterion.bench
            "shared-limiter-100k-tps-50-targets"
            $ Criterion.nfIO
            $ BenchResult <$> Harness.runTpsTest sharedPath    5.0
        , Criterion.bench
            "per-target-limiter-200-tps-50-targets"
            $ Criterion.nfIO
            $ BenchResult <$> Harness.runTpsTest perTargetPath 5.0
        ]
    ]
