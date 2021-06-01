{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
module Cardano.Unlog.Summary
  ( AnalysisCmdError
  , renderAnalysisCmdError
  , runAnalysisCommand
  ) where

import           Prelude (String)
import           Cardano.Prelude

import           Control.Arrow ((&&&), (***))
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import           Control.Concurrent.Async (mapConcurrently)

import qualified Data.Aeson as Aeson
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified System.FilePath as F

import qualified Graphics.Histogram as Hist
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import           Data.Time.Clock (NominalDiffTime)
import           Text.Printf

import           Ouroboros.Network.Block (SlotNo(..))

import           Data.Distribution
import           Cardano.Analysis.Profile
import           Cardano.Unlog.BlockProp
import           Cardano.Unlog.Commands
import           Cardano.Unlog.LogObject hiding (Text)
import           Cardano.Unlog.Render
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats
import           Cardano.Unlog.Timeline


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
      ("Benchmark run metafile parse failed: " <> Text.pack fp)
      pure
    GenesisParseError (JsonGenesisFile fp) err' -> renderError cmd err'
      ("Genesis parse failed: " <> Text.pack fp)
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
-- CLI shelley command dispatch
--

runAnalysisCommand :: AnalysisCommand -> ExceptT AnalysisCmdError IO ()
runAnalysisCommand (MachineTimelineCmd genesisFile metaFile logfiles oFiles) = do
  chainInfo <-
    ChainInfo
      <$> firstExceptT (RunMetaParseError metaFile . Text.pack)
                       (newExceptT $
                         Aeson.eitherDecode @Profile <$> LBS.readFile (unJsonRunMetafile metaFile))
      <*> firstExceptT (GenesisParseError genesisFile . Text.pack)
                       (newExceptT $
                         Aeson.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile genesisFile))
  firstExceptT AnalysisCmdError $
    runMachineTimeline chainInfo logfiles oFiles
runAnalysisCommand (BlockPropagationCmd genesisFile metaFile logfiles oFiles) = do
  chainInfo <-
    ChainInfo
      <$> firstExceptT (RunMetaParseError metaFile . Text.pack)
                       (newExceptT $
                        Aeson.eitherDecode @Profile <$> LBS.readFile (unJsonRunMetafile metaFile))
      <*> firstExceptT (GenesisParseError genesisFile . Text.pack)
                       (newExceptT $
                         Aeson.eitherDecode @Genesis <$> LBS.readFile (unJsonGenesisFile genesisFile))
  firstExceptT AnalysisCmdError $
    runBlockPropagation chainInfo logfiles oFiles
runAnalysisCommand SubstringKeysCmd =
  liftIO $ mapM_ putStrLn logObjectStreamInterpreterKeys

runBlockPropagation ::
  ChainInfo -> [JsonLogfile] -> BlockPropagationOutputFiles -> ExceptT Text IO ()
runBlockPropagation chainInfo logfiles BlockPropagationOutputFiles{..} = do
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

    blockPropagation <- blockProp chainInfo objLists

    forM_ bpofTimelinePretty $
      \(TextOutputFile f) ->
        withFile f WriteMode $ \hnd -> do
          putStrLn ("runBlockPropagation: dumping pretty timeline" :: Text)
          hPutStrLn hnd . Text.pack $ printf "--- input: %s" f
          mapM_ (T.hPutStrLn hnd) (renderDistributions blockPropagation)

    forM_ bpofAnalysis $
      \(JsonOutputFile f) ->
        withFile f WriteMode $ \hnd -> do
          putStrLn ("runBlockPropagation: dumping analysis core" :: Text)
          LBS.hPutStrLn hnd (Aeson.encode blockPropagation)
 where
   joinT :: (IO a, IO b) -> IO (a, b)
   joinT (a, b) = (,) <$> a <*> b

runMachineTimeline ::
  ChainInfo -> [JsonLogfile] -> MachineTimelineOutputFiles -> ExceptT Text IO ()
runMachineTimeline chainInfo logfiles MachineTimelineOutputFiles{..} = do
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
          forM_ noisySlotStats $ LBS.hPutStrLn hnd . Aeson.encode

    -- 2. Reprocess the slot stats
    let slotStats = cleanupSlotStats noisySlotStats

    -- 3. Derive the summary
    let drvVectors0, _drvVectors1 :: [DerivedSlot]
        (,) drvVectors0 _drvVectors1 = computeDerivedVectors slotStats
        summary :: Summary
        summary = slotStatsSummary chainInfo slotStats
        timelineOutput :: LBS.ByteString
        timelineOutput = Aeson.encode summary

    -- 4. Render various outputs
    forM_ mtofTimelinePretty
      (renderPrettySummary slotStats summary logfiles)
    forM_ mtofStatsCsv
      (renderExportStats runStats summary)
    forM_ mtofTimelineCsv
       (renderExportTimeline slotStats)
    forM_ mtofDerivedVectors0Csv
       (renderDerivedSlots drvVectors0)
    forM_ mtofHistogram
      (renderHistogram "CPU usage spans over 85%" "Span length"
        (toList $ sort $ sSpanLensCPU85 summary))

    flip (maybe $ LBS.putStrLn timelineOutput) mtofAnalysis $
      \case
        JsonOutputFile f ->
          withFile f WriteMode $ \hnd ->
            LBS.hPutStrLn hnd timelineOutput
 where
   renderHistogram :: Integral a
     => String -> String -> [a] -> OutputFile -> IO ()
   renderHistogram desc ylab xs (OutputFile f) =
     Hist.plotAdv f opts hist >> pure ()
    where
      hist = Hist.histogram Hist.binFreedmanDiaconis $ fromIntegral <$> xs
      opts = Opts.title desc $ Opts.yLabel ylab $ Opts.xLabel "Population" $
             Hist.defOpts hist

   renderPrettySummary ::
        [SlotStats] -> Summary -> [JsonLogfile] -> TextOutputFile -> IO ()
   renderPrettySummary xs s srcs o =
     withFile (unTextOutputFile o) WriteMode $ \hnd -> do
       hPutStrLn hnd . Text.pack $
         printf "--- input: %s" (intercalate " " $ unJsonLogfile <$> srcs)
       renderSummmaryCDF  statsHeadP statsFormatP statsFormatPF s hnd
       renderSlotTimeline slotHeadP slotFormatP False xs hnd
   renderExportStats :: RunScalars -> Summary -> CsvOutputFile -> IO ()
   renderExportStats rs s (CsvOutputFile o) =
     withFile o WriteMode $
       \h -> do
         renderSummmaryCDF statsHeadE statsFormatE statsFormatEF s h
         mapM_ (hPutStrLn h) $
           renderChainInfoExport chainInfo
           <>
           renderRunScalars rs
   renderExportTimeline :: [SlotStats] -> CsvOutputFile -> IO ()
   renderExportTimeline xs (CsvOutputFile o) =
     withFile o WriteMode $
       renderSlotTimeline slotHeadE slotFormatE True xs

   renderSummmaryCDF :: Text -> Text -> Text -> Summary -> Handle -> IO ()
   renderSummmaryCDF statHead statFmt propFmt summary hnd = do
       hPutStrLn hnd statHead
       forM_ (toDistribLines statFmt propFmt summary) $
         hPutStrLn hnd

   renderDerivedSlots :: [DerivedSlot] -> CsvOutputFile -> IO ()
   renderDerivedSlots slots (CsvOutputFile o) = do
     withFile o WriteMode $ \hnd -> do
       hPutStrLn hnd derivedSlotsHeader
       forM_ slots $
         hPutStrLn hnd . renderDerivedSlot

dumpLOStream :: [LogObject] -> JsonOutputFile -> IO ()
dumpLOStream objs o =
  withFile (unJsonOutputFile o) WriteMode $ \hnd -> do
    forM_ objs $ LBS.hPutStrLn hnd . Aeson.encode

data Summary
  = Summary
    { sMaxChecks         :: !Word64
    , sSlotMisses        :: ![Word64]
    , sSpanLensCPU85     :: ![Int]
    , sSpanLensCPU85EBnd :: ![Int]
    , sSpanLensCPU85Rwd  :: ![Int]
    -- distributions
    , sMissDistrib       :: !(Distribution Float Float)
    , sLeadsDistrib      :: !(Distribution Float Word64)
    , sUtxoDistrib       :: !(Distribution Float Word64)
    , sDensityDistrib    :: !(Distribution Float Float)
    , sSpanCheckDistrib  :: !(Distribution Float NominalDiffTime)
    , sSpanLeadDistrib   :: !(Distribution Float NominalDiffTime)
    , sBlocklessDistrib  :: !(Distribution Float Word64)
    , sSpanLensCPU85Distrib
                         :: !(Distribution Float Int)
    , sSpanLensCPU85EBndDistrib :: !(Distribution Float Int)
    , sSpanLensCPU85RwdDistrib  :: !(Distribution Float Int)
    , sResourceDistribs  :: !(Resources (Distribution Float Word64))
    }
  deriving Show

renderRunScalars :: RunScalars -> [Text]
renderRunScalars RunScalars{..} =
  Text.intercalate "," <$>
  [[ "Run time",       maybe "---" show rsElapsed ]
  ,[ "Txs submitted",  maybe "---" show rsSubmitted ]
  ,[ "Submission TPS", maybe "---" (show . sum) rsThreadwiseTps]
  ]

instance ToJSON Summary where
  toJSON Summary{..} = Aeson.Array $ Vec.fromList
    [ Aeson.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85EBnd"
        , "xs" .= toJSON sSpanLensCPU85EBnd]
    , Aeson.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85Rwd"
        , "xs" .= toJSON sSpanLensCPU85Rwd]
    , Aeson.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85"
        , "xs" .= toJSON sSpanLensCPU85]
    , Aeson.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85Sorted"
        , "xs" .= toJSON (sort sSpanLensCPU85)]
    , extendObject "kind" "spancheck" $ toJSON sSpanCheckDistrib
    , extendObject "kind" "spanlead"  $ toJSON sSpanLeadDistrib
    , extendObject "kind" "cpu"       $ toJSON (rCentiCpu sResourceDistribs)
    , extendObject "kind" "gc"        $ toJSON (rCentiGC  sResourceDistribs)
    , extendObject "kind" "density"   $ toJSON sDensityDistrib
    , extendObject "kind" "utxo"      $ toJSON sUtxoDistrib
    , extendObject "kind" "leads"     $ toJSON sLeadsDistrib
    , extendObject "kind" "misses"    $ toJSON sMissDistrib
    , extendObject "kind" "blockless" $ toJSON sBlocklessDistrib
    , extendObject "kind" "rss"       $ toJSON (rRSS      sResourceDistribs)
    , extendObject "kind" "heap"      $ toJSON (rHeap     sResourceDistribs)
    , extendObject "kind" "live"      $ toJSON (rLive     sResourceDistribs)
    , extendObject "kind" "spanLensCPU85Distrib"  $
                                        toJSON sSpanLensCPU85Distrib
    , extendObject "kind" "spanLensCPU85EBndDistrib"  $
                                        toJSON sSpanLensCPU85EBndDistrib
    , extendObject "kind" "spanLensCPU85RwdDistrib"  $
                                        toJSON sSpanLensCPU85RwdDistrib
    ]

slotStatsSummary :: ChainInfo -> [SlotStats] -> Summary
slotStatsSummary CInfo{} slots =
  Summary
  { sMaxChecks        = maxChecks
  , sSlotMisses       = misses
  , sSpanLensCPU85    = spanLensCPU85
  , sSpanLensCPU85EBnd = sSpanLensCPU85EBnd
  , sSpanLensCPU85Rwd  = sSpanLensCPU85Rwd
  --
  , sMissDistrib      = computeDistribution stdPercentiles missRatios
  , sLeadsDistrib     =
      computeDistribution stdPercentiles (slCountLeads <$> slots)
  , sUtxoDistrib      =
      computeDistribution stdPercentiles (slUtxoSize <$> slots)
  , sDensityDistrib   =
      computeDistribution stdPercentiles (slDensity <$> slots)
  , sSpanCheckDistrib =
      computeDistribution stdPercentiles (slSpanCheck <$> slots)
  , sSpanLeadDistrib =
      computeDistribution stdPercentiles (slSpanLead <$> slots)
  , sBlocklessDistrib =
      computeDistribution stdPercentiles (slBlockless <$> slots)
  , sSpanLensCPU85Distrib
                      = computeDistribution stdPercentiles spanLensCPU85
  , sResourceDistribs =
      computeResDistrib stdPercentiles resDistProjs slots
  , sSpanLensCPU85EBndDistrib = computeDistribution stdPercentiles sSpanLensCPU85EBnd
  , sSpanLensCPU85RwdDistrib  = computeDistribution stdPercentiles sSpanLensCPU85Rwd
  }
 where
   sSpanLensCPU85EBnd = Vec.length <$>
                        filter (spanContainsEpochSlot 3) spansCPU85
   sSpanLensCPU85Rwd  = Vec.length <$>
                        filter (spanContainsEpochSlot 803) spansCPU85

   checkCounts      = slCountChecks <$> slots
   maxChecks        = if length checkCounts == 0
                      then 0 else maximum checkCounts
   misses           = (maxChecks -) <$> checkCounts
   missRatios       = missRatio <$> misses
   spansCPU85 :: [Vector SlotStats]
   spansCPU85       = spans
                        ((/= Just False) . fmap (>=85) . rCentiCpu . slResources)
                        (toList slots)
   spanLensCPU85    = spanLen <$> spansCPU85
   spanContainsEpochSlot :: Word64 -> Vector SlotStats -> Bool
   spanContainsEpochSlot s =
     uncurry (&&)
     . ((s >) . slEpochSlot . Vec.head &&&
        (s <) . slEpochSlot . Vec.last)
   spanLen :: Vector SlotStats -> Int
   spanLen = fromIntegral . unSlotNo . uncurry (-) . (slSlot *** slSlot) . (Vec.last &&& Vec.head)
   resDistProjs     =
     Resources
     { rCentiCpu    = rCentiCpu   . slResources
     , rCentiGC     = rCentiGC    . slResources
     , rCentiMut    = rCentiMut   . slResources
     , rGcsMajor    = rGcsMajor   . slResources
     , rGcsMinor    = rGcsMinor   . slResources
     , rRSS         = rRSS        . slResources
     , rHeap        = rHeap       . slResources
     , rLive        = rLive       . slResources
     , rAlloc       = rAlloc      . slResources
     , rCentiBlkIO  = rCentiBlkIO . slResources
     , rThreads     = rThreads    . slResources
     }

   missRatio :: Word64 -> Float
   missRatio = (/ fromIntegral maxChecks) . fromIntegral

mapSummary ::
     Text
  -> Summary
  -> Text
  -> (forall a. Num a => Distribution Float a -> Float)
  -> Text
mapSummary statsF Summary{..} desc f =
  distribPropertyLine desc
    (f sMissDistrib)
    (f sSpanCheckDistrib)
    (f sSpanLeadDistrib)
    (f sBlocklessDistrib)
    (f sDensityDistrib)
    (f (rCentiCpu sResourceDistribs))
    (f (rCentiGC sResourceDistribs))
    (f (rCentiMut sResourceDistribs))
    (f (rGcsMajor sResourceDistribs))
    (f (rGcsMinor sResourceDistribs))
    (f (rRSS sResourceDistribs))
    (f (rHeap sResourceDistribs))
    (f (rLive sResourceDistribs))
    (f (rAlloc sResourceDistribs))
    (f sSpanLensCPU85Distrib)
    (f sSpanLensCPU85EBndDistrib)
    (f sSpanLensCPU85RwdDistrib)
 where
   distribPropertyLine ::
        Text
     -> Float -> Float -> Float -> Float
     -> Float -> Float -> Float
     -> Float -> Float
     -> Float -> Float -> Float -> Float
     -> Float -> Float -> Float
     -> Float
     -> Text
   distribPropertyLine descr miss chkdt leaddt blkl dens cpu gc mut majg ming rss hea liv alc cpu85Sp cpu85SpEBnd cpu85SpRwd = Text.pack $
     printf (Text.unpack statsF)
    descr miss chkdt leaddt blkl dens cpu gc mut majg ming     rss hea liv alc cpu85Sp cpu85SpEBnd cpu85SpRwd

toDistribLines :: Text -> Text -> Summary -> [Text]
toDistribLines statsF distPropsF s@Summary{..} =
  distribLine
   <$> ZipList (pctSpec <$> dPercentiles sMissDistrib)
   <*> ZipList (max 1 . ceiling . (* fromIntegral (dSize sMissDistrib))
                    . (1.0 -) . pctFrac
                    <$> dPercentiles sMissDistrib)
   <*> ZipList (pctSample <$> dPercentiles sMissDistrib)
   <*> ZipList (pctSample <$> dPercentiles sSpanCheckDistrib)
   <*> ZipList (pctSample <$> dPercentiles sSpanLeadDistrib)
   <*> ZipList (pctSample <$> dPercentiles sBlocklessDistrib)
   <*> ZipList (pctSample <$> dPercentiles sDensityDistrib)
   <*> ZipList (pctSample <$> dPercentiles (rCentiCpu sResourceDistribs))
   <*> ZipList (min 999 . -- workaround for ghc-8.10.x
                pctSample <$> dPercentiles (rCentiGC sResourceDistribs))
   <*> ZipList (min 999 . -- workaround for ghc-8.10.x
                pctSample <$> dPercentiles (rCentiMut sResourceDistribs))
   <*> ZipList (pctSample <$> dPercentiles (rGcsMajor sResourceDistribs))
   <*> ZipList (pctSample <$> dPercentiles (rGcsMinor sResourceDistribs))
    -- <*> ZipList (pctSample <$> dPercentiles ( sResourceDistribs))
   <*> ZipList (pctSample <$> dPercentiles (rRSS sResourceDistribs))
   <*> ZipList (pctSample <$> dPercentiles (rHeap sResourceDistribs))
   <*> ZipList (pctSample <$> dPercentiles (rLive sResourceDistribs))
   <*> ZipList (pctSample <$> dPercentiles (rAlloc sResourceDistribs))
   <*> ZipList (pctSample <$> dPercentiles sSpanLensCPU85Distrib)
   <*> ZipList (pctSample <$> dPercentiles sSpanLensCPU85EBndDistrib)
   <*> ZipList (pctSample <$> dPercentiles sSpanLensCPU85RwdDistrib)
  & getZipList
  & (<> [ mapSummary distPropsF s "size" (fromIntegral . dSize)
        , mapSummary distPropsF s "avg"  dAverage
        ])
 where
   distribLine ::
        PercSpec Float -> Int
     -> Float -> NominalDiffTime -> NominalDiffTime -> Word64 -> Float
     -> Word64 -> Word64 -> Word64
     -> Word64 -> Word64
     -> Word64 -> Word64 -> Word64 -> Word64
     -> Int -> Int -> Int
     -> Text
   distribLine ps count miss chkdt' leaddt' blkl dens cpu gc mut
     majg ming rss hea liv alc cpu85Sp cpu85SpEBnd cpu85SpRwd = Text.pack $
     printf (Text.unpack statsF)
    (renderPercSpec 6 ps) count miss chkdt leaddt blkl dens cpu gc mut majg ming     rss hea liv alc cpu85Sp cpu85SpEBnd cpu85SpRwd
    where chkdt  = Text.init $ show chkdt' :: Text
          leaddt = Text.init $ show leaddt' :: Text

statsHeadE, statsFormatE, statsFormatEF :: Text
statsHeadP, statsFormatP, statsFormatPF :: Text
statsHeadP =
  "%tile Count MissR  CheckΔt  LeadΔt BlkLess Density  CPU  GC MUT Maj Min   RSS  Heap  Live Alloc CPU85%Lens/EBnd/Rwd"
statsHeadE =
  "%tile,Count,MissR,CheckΔ,LeadΔ,Blockless,ChainDensity,CPU,GC,MUT,GcMaj,GcMin,RSS,Heap,Live,Alloc,CPU85%Lens,/EpochBoundary,/Rewards"
statsFormatP =
  "%6s %5d %0.2f   %6s   %6s  %3d     %0.3f  %3d %3d %3d  %2d %3d %5d %5d %5d %5d    %4d   %4d %4d"
statsFormatE =
  "%s,%d,%0.2f,%s,%s,%d,%0.3f,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d"
statsFormatPF =
  "%6s %.2f %.2f   %.2f   %.2f  %.2f     %.2f  %.2f %.2f %.2f %.2f %.2f %.2f      %.2f %.2f %.2f %.2f %.2f"
statsFormatEF =
  "%s,0,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f"
