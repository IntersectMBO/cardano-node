--{-# LANGUAGE ImpredicativeTypes #-}
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
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Control.Concurrent.Async         (mapConcurrently)

import Data.Aeson                       qualified as AE
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        qualified as T
import Data.Text.IO                     qualified as T

import System.FilePath                  qualified as F

import Graphics.Histogram               qualified as Hist
import Graphics.Gnuplot.Frame.OptionSet qualified as Opts

import Text.Printf

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.MachTimeline
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.Commands
import Cardano.Unlog.LogObject          hiding (Text)
import Cardano.Unlog.Render


data AnalysisCmdError
  = AnalysisCmdError                         !Text
  | RunMetaParseError      !JsonRunMetafile  !Text
  | GenesisParseError      !JsonGenesisFile  !Text
  | ChainFiltersParseError !JsonFilterFile   !Text
  deriving Show

renderAnalysisCmdError :: AnalysisCommand -> AnalysisCmdError -> Text
renderAnalysisCmdError cmd err =
  case err of
    AnalysisCmdError  err' -> renderError cmd err'
      "Analysis command failed"
      pure
    RunMetaParseError (JsonRunMetafile fp) err' -> renderError cmd err'
      ("Benchmark run metafile parse failed: " <> T.pack fp)
      pure
    GenesisParseError (JsonGenesisFile fp) err' -> renderError cmd err'
      ("Genesis parse failed: " <> T.pack fp)
      pure
    ChainFiltersParseError (JsonFilterFile fp) err' -> renderError cmd err'
      ("Chain filter list parse failed: " <> T.pack fp)
      pure
 where
   renderError :: AnalysisCommand -> a -> Text -> (a -> [Text]) -> Text
   renderError cmd' cmdErr desc renderer =
      mconcat [ desc, ": "
              , renderAnalysisCommand cmd'
              , "  Error: "
              , mconcat (renderer cmdErr)
              ]

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
runAnalysisCommand :: AnalysisCommand -> ExceptT AnalysisCmdError IO ()
runAnalysisCommand (MachineTimelineCmd genesisFile metaFile mChFiltersFile logfiles oFiles) = do
  progress "run"     (Q . T.unpack . logfileRunIdentifier . T.pack . unJsonLogfile $ P.head logfiles)
  progress "genesis" (Q $ unJsonGenesisFile genesisFile)
  progress "meta"    (Q $ unJsonRunMetafile metaFile)
  runPartial <- firstExceptT (RunMetaParseError metaFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @RunPartial <$> LBS.readFile (unJsonRunMetafile metaFile))
  run        <- firstExceptT (GenesisParseError genesisFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile genesisFile))
                <&> completeRun runPartial
  progress "run"     (J run)

  filters <- forM mChFiltersFile $ \f ->
               firstExceptT (ChainFiltersParseError f . T.pack)
                 (readChainFilter f)
  let (,) justFilters filterNames = unzip filters

  -- progress "inputs"  (L $ unJsonLogfile <$> logfiles)
  progress "filters" (J $ filterNames <&> unFilterName)
  firstExceptT AnalysisCmdError $
    runMachineTimeline run logfiles (mconcat justFilters) filterNames oFiles

runAnalysisCommand (BlockPropagationCmd genesisFile metaFile mChFiltersFile logfiles oFiles) = do
  progress "run"     (Q . T.unpack . logfileRunIdentifier . T.pack . unJsonLogfile $ P.head logfiles)
  progress "genesis" (Q $ unJsonGenesisFile genesisFile)
  progress "meta"    (Q $ unJsonRunMetafile metaFile)
  runPartial <- firstExceptT (RunMetaParseError metaFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @RunPartial <$> LBS.readFile (unJsonRunMetafile metaFile))
  run        <- firstExceptT (GenesisParseError genesisFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile genesisFile))
                <&> completeRun runPartial
  progress "run"     (J run)

  filters <- forM mChFiltersFile $ \f ->
               firstExceptT (ChainFiltersParseError f . T.pack)
                 (readChainFilter f)
  let (,) justFilters filterNames = unzip filters

  -- progress "inputs"  (L $ unJsonLogfile <$> logfiles)
  progress "filters" (J $ filterNames <&> unFilterName)
  firstExceptT AnalysisCmdError $
    runBlockPropagation run (mconcat justFilters) filterNames logfiles oFiles

runAnalysisCommand SubstringKeysCmd =
  liftIO $ mapM_ putStrLn logObjectStreamInterpreterKeys

runAnalysisCommand (RunInfoCmd genesisFile metaFile) = do
  progress "genesis" (Q $ unJsonGenesisFile genesisFile)
  progress "meta"    (Q $ unJsonRunMetafile metaFile)
  runPartial <- firstExceptT (RunMetaParseError metaFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @RunPartial <$> LBS.readFile (unJsonRunMetafile metaFile))
  run        <- firstExceptT (GenesisParseError genesisFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile genesisFile))
                <&> completeRun runPartial
  progress "run"     (J run)

runBlockPropagation ::
  Run -> [ChainFilter] -> [FilterName] -> [JsonLogfile] -> BlockPropagationOutputFiles -> ExceptT Text IO ()
runBlockPropagation run filters filterNames logfiles BlockPropagationOutputFiles{..} = do
  liftIO $ do
    -- 0. Recover LogObjects
    objLists :: [(JsonLogfile, [LogObject])] <- flip mapConcurrently logfiles
      (joinT . (pure &&& readLogObjectStream))

    forM_ bpofLogObjects . const $ do
      flip mapConcurrently objLists $
        \(JsonLogfile f, objs) -> do
            dumpLOStream objs
              (JsonOutputFile $ F.dropExtension f <> ".logobjects.json")

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
 where
   joinT :: (IO a, IO b) -> IO (a, b)
   joinT (a, b) = (,) <$> a <*> b

data F
  = R String
  | Q String
  | L [String]
  | forall a. AE.ToJSON a => J a

progress :: MonadIO m => String -> F -> m ()
progress key = putStrLn . T.pack . \case
  R x  -> printf "{ \"%s\":  %s }"    key x
  Q x  -> printf "{ \"%s\": \"%s\" }" key x
  L xs -> printf "{ \"%s\": \"%s\" }" key (intercalate "\", \"" xs)
  J x  -> printf "{ \"%s\": %s }" key (LBS.unpack $ AE.encode x)

runMachineTimeline ::
  Run -> [JsonLogfile] -> [ChainFilter] -> [FilterName] -> MachineTimelineOutputFiles -> ExceptT Text IO ()
runMachineTimeline run@Run{genesis} logfiles filters filterNames MachineTimelineOutputFiles{..} = do
  liftIO $ do
    -- 0. Recover LogObjects
    objs :: [LogObject] <- concat <$> mapM readLogObjectStream logfiles
    forM_ mtofLogObjects $
      (dumpLOStream objs)

    -- 1. Derive the basic scalars and vectors
    let (,) runStats noisySlotStats = timelineFromLogObjects run objs
    forM_ mtofSlotStats $
      \(JsonOutputFile f) -> do
        progress "raw-slots" (Q f)
        withFile f WriteMode $ \hnd ->
          forM_ noisySlotStats $ LBS.hPutStrLn hnd . AE.encode

    -- 2. Reprocess the slot stats
    let slotStats = filterSlotStats filters noisySlotStats

    let rawSlotFirst = (head    noisySlotStats <&> slSlot) & fromMaybe 0
        rawSlotLast  = (lastMay noisySlotStats <&> slSlot) & fromMaybe 0
        anaSlotFirst = (head         slotStats <&> slSlot) & fromMaybe 0
        anaSlotLast  = (lastMay      slotStats <&> slSlot) & fromMaybe 0
    liftIO . LBS.putStrLn . AE.encode $
      DataDomain
        rawSlotFirst rawSlotLast
        anaSlotFirst anaSlotLast

    -- 3. Derive the timeline
    let drvVectors0, _drvVectors1 :: [DerivedSlot]
        (,) drvVectors0 _drvVectors1 = computeDerivedVectors slotStats
        timeline :: MachTimeline
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
    forM_ mtofDerivedVectors0Csv
       (renderDerivedSlots drvVectors0)
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

   renderDerivedSlots :: [DerivedSlot] -> CsvOutputFile -> IO ()
   renderDerivedSlots slots (CsvOutputFile f) = do
     withFile f WriteMode $ \hnd -> do
       progress "derived-slots" (Q f)
       hPutStrLn hnd derivedSlotsHeader
       forM_ slots $
         hPutStrLn hnd . renderDerivedSlot

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
