{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
module Cardano.Analysis.Driver
  ( AnalysisCmdError
  , renderAnalysisCmdError
  , runAnalysisCommand
  ) where

import Prelude                          (String, error)
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

import Ouroboros.Network.Block (SlotNo)

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.MachTimeline
import Cardano.Analysis.Profile
import Cardano.Unlog.Commands
import Cardano.Unlog.LogObject          hiding (Text)
import Cardano.Unlog.Render
import Cardano.Unlog.SlotStats


data AnalysisCmdError
  = AnalysisCmdError  !Text
  | RunMetaParseError !JsonRunMetafile !Text
  | GenesisParseError !JsonGenesisFile !Text
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
 where
   renderError :: AnalysisCommand -> a -> Text -> (a -> [Text]) -> Text
   renderError cmd' cmdErr desc renderer =
      mconcat [ desc, ": "
              , renderAnalysisCommand cmd'
              , "  Error: "
              , mconcat (renderer cmdErr)
              ]

--
-- Analysis command dispatch
--
runAnalysisCommand :: AnalysisCommand -> ExceptT AnalysisCmdError IO ()
runAnalysisCommand (MachineTimelineCmd genesisFile metaFile logfiles oFiles mEndSlot) = do
  chainInfo <-
    ChainInfo
      <$> firstExceptT (RunMetaParseError metaFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @Profile <$> LBS.readFile (unJsonRunMetafile metaFile))
      <*> firstExceptT (GenesisParseError genesisFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile genesisFile))
  firstExceptT AnalysisCmdError $
    runMachineTimeline chainInfo logfiles oFiles mEndSlot
runAnalysisCommand (BlockPropagationCmd genesisFile metaFile logfiles oFiles) = do
  chainInfo <-
    ChainInfo
      <$> firstExceptT (RunMetaParseError metaFile . T.pack)
                       (newExceptT $
                         AE.eitherDecode @Profile <$> LBS.readFile (unJsonRunMetafile metaFile))
      <*> firstExceptT (GenesisParseError genesisFile . T.pack)
                       (newExceptT $
                        AE.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile genesisFile))
  firstExceptT AnalysisCmdError $
    runBlockPropagation chainInfo logfiles oFiles
runAnalysisCommand SubstringKeysCmd =
  liftIO $ mapM_ putStrLn logObjectStreamInterpreterKeys

runBlockPropagation ::
  ChainInfo -> [JsonLogfile] -> BlockPropagationOutputFiles -> ExceptT Text IO ()
runBlockPropagation cInfo logfiles BlockPropagationOutputFiles{..} = do
  liftIO $ do
    putStrLn ("runBlockPropagation: lifting LO streams" :: Text)
    -- 0. Recover LogObjects
    objLists :: [(JsonLogfile, [LogObject])] <- flip mapConcurrently logfiles
      (joinT . (pure &&& readLogObjectStream))

    forM_ bpofLogObjects . const $ do
      flip mapConcurrently objLists $
        \(JsonLogfile f, objs) -> do
            putStrLn ("runBlockPropagation: dumping LO streams" :: Text)
            dumpLOStream objs
              (JsonOutputFile $ F.dropExtension f <> ".logobjects.json")

    blockPropagation <- blockProp cInfo objLists

    forM_ bpofTimelinePretty $
      \(TextOutputFile f) ->
        withFile f WriteMode $ \hnd -> do
          putStrLn ("runBlockPropagation: dumping pretty timeline" :: Text)
          hPutStrLn hnd . T.pack $ printf "--- input: %s" f
          mapM_ (T.hPutStrLn hnd)
            (renderDistributions (cProfile cInfo) RenderPretty blockPropagation)
          mapM_ (T.hPutStrLn hnd)
            (renderTimeline (cProfile cInfo) $ bpChainBlockEvents blockPropagation)

    forM_ bpofAnalysis $
      \(JsonOutputFile f) ->
        withFile f WriteMode $ \hnd -> do
          putStrLn ("runBlockPropagation: dumping analysis core" :: Text)
          LBS.hPutStrLn hnd (AE.encode blockPropagation)
 where
   joinT :: (IO a, IO b) -> IO (a, b)
   joinT (a, b) = (,) <$> a <*> b

runMachineTimeline ::
  ChainInfo -> [JsonLogfile] -> MachineTimelineOutputFiles -> Maybe SlotNo -> ExceptT Text IO ()
runMachineTimeline chainInfo logfiles MachineTimelineOutputFiles{..} mEndSlot = do
  liftIO $ do
    -- 0. Recover LogObjects
    objs :: [LogObject] <- concat <$> mapM readLogObjectStream logfiles
    forM_ mtofLogObjects
      (dumpLOStream objs)

    -- 1. Derive the basic scalars and vectors
    let (,) runStats noisySlotStats = timelineFromLogObjects chainInfo objs
    forM_ mtofSlotStats $
      \(JsonOutputFile f) ->
        withFile f WriteMode $ \hnd ->
          forM_ noisySlotStats $ LBS.hPutStrLn hnd . AE.encode

    -- 2. Reprocess the slot stats
    let slotStats = cleanupSlotStats mEndSlot noisySlotStats

    -- 3. Derive the timeline
    let drvVectors0, _drvVectors1 :: [DerivedSlot]
        (,) drvVectors0 _drvVectors1 = computeDerivedVectors slotStats
        timeline :: MachTimeline
        timeline = slotStatsMachTimeline chainInfo slotStats
        timelineOutput :: LBS.ByteString
        timelineOutput = AE.encode timeline

    -- 4. Render various outputs
    forM_ mtofTimelinePretty
      (renderPrettyMachTimeline slotStats timeline logfiles)
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
          withFile f WriteMode $ \hnd ->
            LBS.hPutStrLn hnd timelineOutput
 where
   p = cProfile chainInfo

   renderHistogram :: Integral a
     => String -> String -> [a] -> OutputFile -> IO ()
   renderHistogram desc ylab xs (OutputFile f) =
     Hist.plotAdv f opts hist >> pure ()
    where
      hist = Hist.histogram Hist.binFreedmanDiaconis $ fromIntegral <$> xs
      opts = Opts.title desc $ Opts.yLabel ylab $ Opts.xLabel "Population" $
             Hist.defOpts hist

   renderPrettyMachTimeline ::
        [SlotStats] -> MachTimeline -> [JsonLogfile] -> TextOutputFile -> IO ()
   renderPrettyMachTimeline xs s srcs o =
     withFile (unTextOutputFile o) WriteMode $ \hnd -> do
       hPutStrLn hnd . T.pack $
         printf "--- input: %s" (intercalate " " $ unJsonLogfile <$> srcs)
       mapM_ (T.hPutStrLn hnd)
         (renderDistributions p RenderPretty s)
       mapM_ (T.hPutStrLn hnd)
         (renderTimeline p xs)
   renderExportStats :: RunScalars -> MachTimeline -> CsvOutputFile -> IO ()
   renderExportStats rs s (CsvOutputFile o) =
     withFile o WriteMode $
       \h -> do
         mapM_ (hPutStrLn h)
           (renderDistributions p RenderCsv s)
         mapM_ (hPutStrLn h) $
           renderChainInfoExport chainInfo
           <>
           renderRunScalars rs
   renderExportTimeline :: [SlotStats] -> CsvOutputFile -> IO ()
   renderExportTimeline _xs (CsvOutputFile _o) =
     error "Timeline export is not supported."
     -- withFile o WriteMode $
     --   mapM_ (T.hPutStrLn hnd) (renderTimeline xs)

   renderDerivedSlots :: [DerivedSlot] -> CsvOutputFile -> IO ()
   renderDerivedSlots slots (CsvOutputFile o) = do
     withFile o WriteMode $ \hnd -> do
       hPutStrLn hnd derivedSlotsHeader
       forM_ slots $
         hPutStrLn hnd . renderDerivedSlot

dumpLOStream :: [LogObject] -> JsonOutputFile -> IO ()
dumpLOStream objs o =
  withFile (unJsonOutputFile o) WriteMode $ \hnd -> do
    forM_ objs $ LBS.hPutStrLn hnd . AE.encode

renderRunScalars :: RunScalars -> [Text]
renderRunScalars RunScalars{..} =
  T.intercalate "," <$>
  [[ "Run time",       maybe "---" show rsElapsed ]
  ,[ "Txs submitted",  maybe "---" show rsSubmitted ]
  ,[ "Submission TPS", maybe "---" (show . sum) rsThreadwiseTps]
  ]
