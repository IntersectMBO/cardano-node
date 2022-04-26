{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
module Cardano.Analysis.Driver
  ( AnalysisCmdError
  , renderAnalysisCmdError
  , runAnalysisCommand
  ) where

import Prelude                          (String)
import Prelude           qualified as P (error, head)
import Cardano.Prelude

import Control.Arrow                    ((&&&))
import Control.Monad
import Control.Monad.Trans.Except.Extra (firstExceptT)
import Control.Concurrent.Async         (mapConcurrently)

import Data.Aeson                       qualified as AE
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        qualified as T
import Data.Text.IO                     qualified as T
import Data.List                        ((!!))

import System.FilePath                  qualified as F

import Graphics.Histogram               qualified as Hist
import Graphics.Gnuplot.Frame.OptionSet qualified as Opts

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Ground
import Cardano.Analysis.MachTimeline
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.LogObject          hiding (Text)
import Cardano.Unlog.Render


-- | This estimates the run identifier, given the expected path structure.
logfileRunIdentifier :: Text -> Text
logfileRunIdentifier fp =
  case drop (length xs - 3) xs of
    rundir:_ -> rundir
    _ -> fp
 where
   xs = T.split (== '/') fp

textId :: FilePath -> [FilterName] -> Text
textId inputfp fnames = mconcat
  [ "input: ", logfileRunIdentifier (T.pack inputfp), " "
  , "locli: ", gitRev getVersion
  , "filters: ", T.intercalate " " (unFilterName <$> fnames)
  ]

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
runLiftLogObjects :: [JsonLogfile] -> ExceptT Text IO [(JsonLogfile, [LogObject])]
runLiftLogObjects fs = liftIO $
  flip mapConcurrently fs
    (joinT . (pure &&& readLogObjectStream . unJsonLogfile))
 where
   joinT :: (IO a, IO b) -> IO (a, b)
   joinT (a, b) = (,) <$> a <*> b

runDumpLogObjects :: [(JsonLogfile, [LogObject])] -> ExceptT Text IO ()
runDumpLogObjects os = liftIO $ do
  flip mapConcurrently os $
    \(JsonLogfile f, objs) -> do
      dumpLOStream objs
        (JsonOutputFile $ F.dropExtension f <> ".logobjects.json")
  pure ()

runBlockPropagation ::
  Run -> [ChainFilter] -> [FilterName] -> [JsonLogfile] -> BlockPropagationOutputFiles -> ExceptT Text IO ()
runBlockPropagation run filters filterNames logfiles BlockPropagationOutputFiles{..} = do
  objLists <- runLiftLogObjects logfiles

  liftIO $ do
    blockPropagation <- blockProp run filters objLists

    forM_ bpofTimelinePretty $
      \(TextOutputFile f) ->
        withFile f WriteMode $ \hnd -> do
          progress "pretty-timeline" (Q f)
          hPutStrLn hnd $ textId f filterNames
          mapM_ (T.hPutStrLn hnd)
            (renderDistributions run RenderPretty blockPropagation)
          mapM_ (T.hPutStrLn hnd)
            (renderTimeline run $ bpChainBlockEvents blockPropagation)

    forM_ bpofAnalysis $
      \(JsonOutputFile f) ->
        withFile f WriteMode $ \hnd -> do
          progress "analysis" (Q f)
          LBS.hPutStrLn hnd (AE.encode blockPropagation)

runMachineTimeline ::
  Run -> [JsonLogfile] -> [ChainFilter] -> [FilterName] -> MachineTimelineOutputFiles -> ExceptT Text IO ()
runMachineTimeline run@Run{genesis} logfiles filters filterNames MachineTimelineOutputFiles{..} = do
  objLists <- runLiftLogObjects logfiles

  liftIO $ do
    -- 0. Recover LogObjects

    -- 1. Derive the basic scalars and vectors
    let (,) runStats allSlotStats = timelineFromLogObjects run (snd $ objLists !! 0)
    forM_ mtofSlotStats $
      \(JsonOutputFile f) -> do
        progress "raw-slots" (Q f)
        withFile f WriteMode $ \hnd ->
          forM_ allSlotStats $ LBS.hPutStrLn hnd . AE.encode

    -- 2. Filter the slot stats
    let slotStats = filterSlotStats filters allSlotStats
    progress "subsetting" $ J $
      DataDomain
        ((head    allSlotStats <&> slSlot) & fromMaybe 0)
        ((lastMay allSlotStats <&> slSlot) & fromMaybe 0)
        ((head       slotStats <&> slSlot) & fromMaybe 0)
        ((lastMay    slotStats <&> slSlot) & fromMaybe 0)

    -- 3. Derive the timeline
    let timeline :: MachTimeline
        timeline = slotStatsMachTimeline run slotStats
        timelineOutput :: LBS.ByteString
        timelineOutput = AE.encode timeline

    -- 4. Render various outputs
    forM_ mtofTimelinePretty
      (renderPrettyMachTimeline slotStats timeline)
    forM_ mtofStatsCsv
      (renderExportStats runStats timeline)
    forM_ mtofTimelineCsv
       (renderExportTimeline slotStats)
    forM_ mtofHistogram
      (renderHistogram "CPU usage spans over 85%" "Span length"
        (toList $ sort $ sSpanLensCPU85 timeline))

    flip (maybe $ LBS.putStrLn timelineOutput) mtofAnalysis $
      \case
        JsonOutputFile f ->
          withFile f WriteMode $ \hnd -> do
            progress "analysis" (Q f)
            LBS.hPutStrLn hnd timelineOutput
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

   renderHistogram :: Integral a
     => String -> String -> [a] -> OutputFile -> IO ()
   renderHistogram desc ylab xs (OutputFile f) =
     Hist.plotAdv f opts hist >> pure ()
    where
      hist = Hist.histogram Hist.binFreedmanDiaconis $ fromIntegral <$> xs
      opts = Opts.title desc $ Opts.yLabel ylab $ Opts.xLabel "Population" $
             Hist.defOpts hist

   renderPrettyMachTimeline ::
        [SlotStats] -> MachTimeline -> TextOutputFile -> IO ()
   renderPrettyMachTimeline xs s (TextOutputFile f) =
     withFile f WriteMode $ \hnd -> do
       progress "pretty-timeline" (Q f)
       hPutStrLn hnd $ textId f filterNames
       mapM_ (T.hPutStrLn hnd)
         (renderDistributions run RenderPretty s)
       mapM_ (T.hPutStrLn hnd)
         (renderTimeline run xs)
   renderExportStats :: RunScalars -> MachTimeline -> CsvOutputFile -> IO ()
   renderExportStats rs s (CsvOutputFile f) =
     withFile f WriteMode $
       \h -> do
         progress "csv-stats" (Q f)
         mapM_ (hPutStrLn h)
           (renderDistributions run RenderCsv s)
         mapM_ (hPutStrLn h) $
           renderRunExport run
           <>
           renderRunScalars rs
   renderExportTimeline :: [SlotStats] -> CsvOutputFile -> IO ()
   renderExportTimeline _xs (CsvOutputFile _o) =
     P.error "Timeline export is not supported."
     -- withFile o WriteMode $
     --   mapM_ (T.hPutStrLn hnd) (renderTimeline xs)

dumpLOStream :: [LogObject] -> JsonOutputFile -> IO ()
dumpLOStream objs (JsonOutputFile f) = do
  progress "logobjects" (Q f)
  withFile f WriteMode $ \hnd -> do
    forM_ objs $ LBS.hPutStrLn hnd . AE.encode

renderRunScalars :: RunScalars -> [Text]
renderRunScalars RunScalars{..} =
  T.intercalate "," <$>
  [[ "Run time",       maybe "---" show rsElapsed ]
  ,[ "Txs submitted",  maybe "---" show rsSubmitted ]
  ,[ "Submission TPS", maybe "---" (show . sum) rsThreadwiseTps]
  ]
