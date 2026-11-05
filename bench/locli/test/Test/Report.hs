{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | The bundled Typst templates render with the environments the report generators
--   build from real analysis results. Template errors otherwise only show at the end
--   of a workbench analysis run.
module Test.Report where

import           Cardano.Prelude

import           Data.Aeson (FromJSON, eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (createDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Cardano.Analysis.API
import           Cardano.Render
import           Cardano.Report
import           Cardano.Unlog.PlutusBudgetSummary (writeScalingCSV)
import           Cardano.Util (zeroUTCTime)

import           Test.Analysis.Compat (addedIn3_0, clusterPerfFixture, decodeClusterPerf, withoutKeys)

import           Hedgehog


readFixture :: FromJSON a => FilePath -> PropertyT IO a
readFixture f = evalEither =<< evalIO (eitherDecodeFileStrict ("test/data/analysis" </> f))

-- | A single run's analysis results, as a 3.0 analysis writes them.
fixtureRun :: PropertyT IO (SummaryOne, ClusterPerf, BlockPropOne)
fixtureRun =
  (,,) <$> readFixture "summary.json" <*> readFixture "clusterperf.json" <*> readFixture "blockprop.json"

-- | The fixture run's cluster performance, as an analysis by locli 2.x wrote it.
fixtureClusterPerf2x :: PropertyT IO ClusterPerf
fixtureClusterPerf2x = do
  o <- evalEither =<< evalIO (eitherDecodeFileStrict clusterPerfFixture)
  evalEither $ decodeClusterPerf (withoutKeys addedIn3_0 o)

-- | Fully rendered: no template syntax left over.
assertRendered :: Text -> PropertyT IO ()
assertRendered t = do
  assert . not $ T.null t
  for_ ["{{", "}}", "{%", "%}"] $ \tag -> do
    annotate (toS tag)
    assert . not $ tag `T.isInfixOf` t

assertContains :: Text -> Text -> PropertyT IO ()
assertContains needle t = do
  annotate (toS needle)
  assert $ needle `T.isInfixOf` t

typstCfg :: RenderConfig
typstCfg = mkRenderConfigDef AsTypst

anchor :: Anchor
anchor = tagsAnchor ["fixture-run"] zeroUTCTime ([], []) Nothing Nothing

-- | Template single_table.ede, for each table of the single-run analysis.
prop_single_tables_render = withTests 1 $ property $ do
  (summ, cp, bp) <- fixtureRun
  cp2x <- fixtureClusterPerf2x

  assertRendered =<< evalIO (generateTypstSummary typstCfg anchor (iFields sumFieldsReport) summ)
  for_ [mtFieldsReport, mtFieldsSnapshots, mtFieldsTxProcessing] $ \fs ->
    assertRendered =<< evalIO (generateTypstCDFs typstCfg anchor (dFields fs) OfOverallDataset cp)
  for_ [bpFieldsControl, bpFieldsForger, bpFieldsPeers, bpFieldsEndToEnd] $ \fs ->
    assertRendered =<< evalIO (generateTypstCDFs typstCfg anchor (dFields fs) OfOverallDataset bp)

  -- A metric missing from an older run's analysis renders as not available.
  snapshots2x <- evalIO $ generateTypstCDFs typstCfg anchor (dFields mtFieldsSnapshots) OfOverallDataset cp2x
  assertRendered snapshots2x
  assertContains "—" snapshots2x

-- | Template main_single.ede: the single-run report, with and without its table files.
prop_single_run_report_renders = withTests 1 $ property $ do
  (summ, cp, bp) <- fixtureRun
  let tableFiles = sTypstFile <$> summaryReportSection summ : analysesReportSections cp bp

  evalIO (withSystemTempDirectory "locli-test-report" $ \dir -> do
      let reportFile = dir </> "report-fixture-run.typ"
      missing <- generateTypstReport reportFile Nothing summ cp bp
      for_ tableFiles $ \f -> T.writeFile (dir </> f) ""
      present <- generateTypstReport reportFile Nothing summ cp bp
      pure (missing, present))
    >>= \(missing, present) -> do
      for_ [missing, present] $ \t -> do
        assertRendered t
        assertContains "<export_file_name>" t
        assert . not $ "Plutus Workload Calibration" `T.isInfixOf` t
      -- a table file is imported if present, otherwise noted as missing
      for_ tableFiles $ \f -> do
        let imported = "import \"" <> T.pack f <> "\""
            noted    = "Table file `" <> T.pack f <> "` was not found"
        assertContains noted    missing
        assertContains imported present
        annotate f
        assert . not $ imported `T.isInfixOf` missing
        assert . not $ noted    `T.isInfixOf` present

-- | The Plutus workload calibration: its CSV is written if every run has the generator's
--   budget summary (test/data/generator/, next to the fixture run's analysis directory),
--   and the single-run report renders it as an addendum.
prop_plutus_calibration_renders = withTests 1 $ property $ do
  (summ, cp, bp) <- fixtureRun

  (written, noSummary, partial, csv, report) <- evalIO $
    withSystemTempDirectory "locli-test-plutus" $ \dir -> do
      let noGenerator = dir </> "analysis"
      createDirectory noGenerator
      written   <- writeScalingCSV dir ["test/data/analysis"]
      csv       <- maybe (pure "") (T.readFile . (dir </>)) written
      noSummary <- writeScalingCSV dir [noGenerator]
      partial   <- writeScalingCSV dir ["test/data/analysis", noGenerator]
      report    <- generateTypstReport (dir </> "report-fixture-run.typ") written summ cp bp
      pure (written, noSummary, partial, csv, report)

  written   === Just "plutus-workload-scaling.csv"
  noSummary === Nothing
  partial   === Nothing
  -- one column, from the fixture's budget summary
  assertContains "loops per tx,3777" csv
  assertContains "txns per block,4"  csv

  assertRendered report
  assertContains "= Addendum: Plutus Workload Calibration" report
  assertContains "csv(\"plutus-workload-scaling.csv\")" report
  assertContains "columns: 2" report

-- | Template main_comparison.ede: comparing a run with itself as analysed by locli 2.x,
--   with and without a Plutus calibration addendum.
prop_comparison_report_renders = withTests 1 $ property $ do
  (summ, cp, bp) <- fixtureRun
  cp2x <- fixtureClusterPerf2x
  let run   = (SomeSummary summ, cp,   SomeBlockProp bp)
      run2x = (SomeSummary summ, cp2x, SomeBlockProp bp)

  plain <- evalIO $ generateTypst "report.typ" Nothing run [run2x]
  assertRendered plain
  assertContains "—" plain

  withScaling <- evalIO $ generateTypst "report.typ" (Just "plutus-workload-scaling.csv") run [run2x]
  assertRendered withScaling
  assertContains "plutus-workload-scaling.csv" withScaling

tests :: IO Bool
tests =
  checkSequential $$discover
