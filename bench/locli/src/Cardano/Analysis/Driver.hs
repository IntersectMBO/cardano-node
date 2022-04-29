{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
module Cardano.Analysis.Driver
  ( AnalysisCmdError
  , renderAnalysisCmdError
  , runAnalysisCommand
  ) where

import Prelude                          (String)
import Prelude           qualified as P (head)
import Cardano.Prelude

import Control.Arrow                    ((&&&))
import Control.Monad.Trans.Except.Extra (firstExceptT)

import Data.Text                        qualified as T
import Data.List                        ((!!))

import Graphics.Histogram               qualified as Hist
import Graphics.Gnuplot.Frame.OptionSet qualified as Opts

import Data.Distribution
import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Ground
import Cardano.Analysis.MachTimeline
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.LogObject          hiding (Text)
import Cardano.Unlog.Render
import Cardano.Util


_reportBanner :: [FilterName] -> FilePath -> Text
_reportBanner fnames inputfp = mconcat
  [ "input: ", logfileRunIdentifier (T.pack inputfp), " "
  , "locli: ", gitRev getVersion, " "
  , "filters: ", T.intercalate " " (unFilterName <$> fnames)
  ]

-- | This estimates the run identifier, given the expected path structure.
logfileRunIdentifier :: Text -> Text
logfileRunIdentifier fp =
  case drop (length xs - 3) xs of
    rundir:_ -> rundir
    _ -> fp
 where
   xs = T.split (== '/') fp

--
-- Analysis command dispatch
--
runAnalysisCommand :: Maybe Run -> [([ChainFilter], FilterName)] -> [JsonLogfile] -> AnalysisCommand -> ExceptT AnalysisCmdError IO ()
runAnalysisCommand (Just run) filters logfiles@(_:_) (MachineTimelineCmd oFiles) = do
  progress "run"     (Q . T.unpack . logfileRunIdentifier . T.pack . unJsonLogfile $ P.head logfiles)

  let (,) justFilters filterNames = unzip filters

  firstExceptT AnalysisCmdError $
    runMachineTimeline run logfiles (mconcat justFilters) filterNames oFiles
runAnalysisCommand (Just run) filters logfiles@(_:_) (BlockPropagationCmd oFiles) = do
  progress "run"     (Q . T.unpack . logfileRunIdentifier . T.pack . unJsonLogfile $ P.head logfiles)

  let (,) justFilters filterNames = unzip filters

  firstExceptT AnalysisCmdError $
    runBlockPropagation run (mconcat justFilters) filterNames logfiles oFiles
runAnalysisCommand _ _ _ SubstringKeysCmd = liftIO $
  mapM_ putStrLn logObjectStreamInterpreterKeys
runAnalysisCommand _ _ _ RunInfoCmd = pure ()
runAnalysisCommand Nothing _ _ _ =
  throwError MissingRunContext
runAnalysisCommand _ _ [] _ =
  throwError MissingLogfiles

--
-- Analysis command implementation
--
runLiftLogObjects :: [JsonLogfile]
                  -> ExceptT Text IO [(JsonLogfile, [LogObject])]
runLiftLogObjects fs = liftIO $
  flip mapConcurrently fs
    (joinT . (pure &&& readLogObjectStream . unJsonLogfile))
 where
   joinT :: (IO a, IO b) -> IO (a, b)
   joinT (a, b) = (,) <$> a <*> b

runBlockProp :: Run -> [ChainFilter] -> [(JsonLogfile, [LogObject])]
             -> ExceptT Text IO (BlockPropagation, [BlockEvents])
runBlockProp run filters objLists = liftIO $
  blockProp run filters objLists

runBlockPropagation :: Run -> [ChainFilter] -> [FilterName] -> [JsonLogfile] -> BlockPropagationOutputFiles
                    -> ExceptT Text IO ()
runBlockPropagation run filters _filterNames logfiles BlockPropagationOutputFiles{..} = do
  objLists <- runLiftLogObjects logfiles
  (blockPropagation, chain) <- runBlockProp run filters objLists

  forM_ bpofAnalysis $ dumpObject "blockprop"
    blockPropagation
  forM_ bpofChain $ dumpObjects "blockprop-chain"
    chain

  forM_ bpofForgerPretty $ dumpText "blockprop-forger" $
    renderDistributions run RenderPretty bpFieldsForger Nothing blockPropagation

  forM_ bpofPeersPretty $ dumpText "blockprop-peers" $
    renderDistributions run RenderPretty bpFieldsPeers Nothing blockPropagation

  forM_ bpofPropagationPretty $ dumpText "blockprop-propagation" $
    renderDistributions run RenderPretty bpFieldsPropagation (Just briefPercSpecs) blockPropagation

  forM_ bpofFullStatsPretty $ dumpText "blockprop-full-stats" $
    renderDistributions run RenderPretty (const True) Nothing blockPropagation

  forM_ bpofChainPretty $ dumpText "blockprop-chain" $
    renderTimeline run (const True) chain

runSlotFilters ::
     Run
  -> [ChainFilter] -> [FilterName]
  -> [(JsonLogfile, [SlotStats])]
  -> ExceptT Text IO (DataDomain SlotNo, [(JsonLogfile, [SlotStats])])
runSlotFilters Run{genesis} flts _fltNames slots = do
  filtered <- liftIO $ mapConcurrentlyPure (fmap $ filterSlotStats flts) slots
  let samplePre  =    slots !! 0 & snd
      samplePost = filtered !! 0 & snd
      domain = mkDataDomain
        ((head    samplePre  <&> slSlot) & fromMaybe 0)
        ((lastMay samplePre  <&> slSlot) & fromMaybe 0)
        ((head    samplePost <&> slSlot) & fromMaybe 0)
        ((lastMay samplePost <&> slSlot) & fromMaybe 0)
        (fromIntegral . unSlotNo)
  progress "subsetting" $ J domain
  pure $ (,) domain filtered

 where
   -- | Use the supplied chain filters.
   --
   --   The idea is that the initial part is useless until the node actually starts
   --   to interact with the blockchain, so we drop all slots until they start
   --   getting non-zero chain density reported.
   --
   --   On the trailing part, we drop everything since the last leadership check.
   filterSlotStats :: [ChainFilter] -> [SlotStats] -> [SlotStats]
   filterSlotStats filters =
     filter (\x -> all (testSlotStats genesis x) slotFilters)
    where
      slotFilters :: [SlotCond]
      slotFilters = catSlotFilters filters

runSlotStatsSummary :: Run -> [(JsonLogfile, [SlotStats])]
  -> ExceptT Text IO [(JsonLogfile, MachTimeline)]
runSlotStatsSummary run slots = liftIO $
  mapConcurrentlyPure (fmap $ slotStatsSummary run) slots

runMachineTimeline ::
     Run -> [JsonLogfile] -> [ChainFilter] -> [FilterName] -> MachineTimelineOutputFiles
  -> ExceptT Text IO ()
runMachineTimeline run@Run{} logfiles filters filterNames MachineTimelineOutputFiles{..} = do
  objLists <- runLiftLogObjects logfiles
  (_scalars, slotsRaw :: [(JsonLogfile, [SlotStats])]) <- liftIO $
    mapAndUnzip redistribute <$>
      collectSlotStats run objLists

  (_domain, slots :: [(JsonLogfile, [SlotStats])]) <-
    runSlotFilters run filters filterNames slotsRaw

  forM_ mtofSlotStats . const $
    dumpAssociatedObjectStreams "slots" slots

  analyses :: [(JsonLogfile, MachTimeline)] <-
    runSlotStatsSummary run slots

  forM_ mtofAnalysis . const $
    dumpAssociatedObjects "perf-stats"
      analyses

  forM_ mtofFullStatsPretty . const $
    dumpAssociatedTextStreams "perf-full" $
      fmap (fmap $ renderDistributions run RenderPretty (const True) Nothing) analyses

  forM_ mtofReportStatsPretty . const $
    dumpAssociatedTextStreams "perf-report" $
      fmap (fmap $ renderDistributions run RenderPretty mtFieldsReport Nothing) analyses

  forM_ mtofTimelinePretty . const $
    dumpAssociatedTextStreams "mach" $
      fmap (fmap $ renderTimeline run (const True)) slots

  -- forM_ mtofFullStatsCsv $ dumpText "perf-full" $
  --   renderDistributions run RenderCsv (const True) analyses

 where
   _renderHistogram :: Integral a
     => String -> String -> [a] -> OutputFile -> IO ()
   _renderHistogram desc ylab xs (OutputFile f) =
     Hist.plotAdv f opts hist >> pure ()
    where
      hist = Hist.histogram Hist.binFreedmanDiaconis $ fromIntegral <$> xs
      opts = Opts.title desc $ Opts.yLabel ylab $ Opts.xLabel "Population" $
             Hist.defOpts hist

   _renderExportStats :: RunScalars -> MachTimeline -> CsvOutputFile -> IO ()
   _renderExportStats rs s (CsvOutputFile f) =
     withFile f WriteMode $
       \h -> do
         progress "csv-stats" (Q f)
         mapM_ (hPutStrLn h)
           (renderDistributions run RenderCsv (const True) Nothing s)
         mapM_ (hPutStrLn h) $
           renderRunExport run
           <>
           renderRunScalars rs

renderRunScalars :: RunScalars -> [Text]
renderRunScalars RunScalars{..} =
  T.intercalate "," <$>
  [[ "Run time",       maybe "---" show rsElapsed ]
  ,[ "Txs submitted",  maybe "---" show rsSubmitted ]
  ,[ "Submission TPS", maybe "---" (show . sum) rsThreadwiseTps]
  ]
