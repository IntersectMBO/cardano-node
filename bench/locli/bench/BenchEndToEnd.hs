{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration benchmarks that decode real locli analysis outputs (JSON)
--   from a CI benchmark run.  Measures JSON deserialisation + NFData
--   evaluation of 'ClusterPerf' and 'SummaryOne', providing a floor for
--   end-to-end wall-clock time.
--
--   Expects to be run from the repository root with run data present under
--   @.\/run\/current\/analysis\/@.  Prints a skip message and exits cleanly
--   if the data is absent.
module Main (main) where

import           Cardano.Analysis.API.Types (ClusterPerf, SummaryOne)

import           Prelude

import           Control.Monad (unless)
import           Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as BS
import           System.Directory (doesFileExist)
import           System.Exit (exitSuccess)

import           Criterion.Main


-- | Default analysis directory: the @current@ symlink created by the
--   workbench.
analysisDir :: FilePath
analysisDir = "run/current/analysis"

main :: IO ()
main = do
  let cpFile  = analysisDir <> "/clusterperf.json"
      sumFile = analysisDir <> "/summary.json"

  cpExists  <- doesFileExist cpFile
  sumExists <- doesFileExist sumFile

  unless (cpExists && sumExists) $ do
    putStrLn $ "bench-locli-e2e: skipping — run data not found at " <> analysisDir
    putStrLn   "  (run 'wb analyse standard current' first, or point at a run directory)"
    exitSuccess

  cpBytes  <- BS.readFile cpFile
  sumBytes <- BS.readFile sumFile

  defaultMain
    [ bgroup "decode"
      [ bench "clusterperf.json" $
          nf (eitherDecodeStrict' @ClusterPerf) cpBytes
      , bench "summary.json" $
          nf (eitherDecodeStrict' @SummaryOne) sumBytes
      ]
    ]
