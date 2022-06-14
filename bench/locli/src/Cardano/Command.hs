{-# OPTIONS_GHC -fmax-pmcheck-models=5000 #-}
module Cardano.Command (module Cardano.Command) where

import Prelude                          (error)
import Cardano.Prelude                  hiding (State)

import Data.Aeson                       qualified as Aeson
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        (pack)
import Data.Text                        qualified as T
import Data.Text.Short                  (toText)
import Options.Applicative
import Options.Applicative              qualified as Opt

import System.FilePath

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Ground
import Cardano.Analysis.MachPerf
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.LogObject          hiding (Text)
import Cardano.Unlog.Render
import Data.CDF

data CommandError
  = CommandError    ChainCommand    Text
  deriving Show

renderCommandError :: CommandError -> Text
renderCommandError (CommandError cmd err) =
  "While executing chained command '" <> show cmd <> "':  " <> err

-- | Sub-commands
newtype Command
  -- | Analysis commands
  = ChainCommand [ChainCommand]
  deriving Show

parseChainCommand :: Parser ChainCommand
parseChainCommand =
  subparser (mconcat [ commandGroup "Common data:  logobject keys, run metafile & genesis"
   , op "list-logobject-keys" "List logobject keys that analyses care about"
     (ListLogobjectKeys
       <$> optTextOutputFile  "keys"           "Text file to write logobject keys to")
   , op "list-logobject-keys-legacy" "List legacy logobject keys that analyses care about"
     (ListLogobjectKeysLegacy
       <$> optTextOutputFile  "keys-legacy"     "Text file to write logobject keys to")
   , op "meta-genesis" "Machine performance timeline"
     (MetaGenesis
       <$> optJsonRunMetafile "run-metafile"    "The meta.json file from the benchmark run"
       <*> optJsonGenesisFile "shelley-genesis" "Genesis file of the run")
   ]) <|>

  subparser (mconcat [ commandGroup "Basic log objects"
   , op "unlog" "Read log files"
     (Unlog
       <$> some
           (optJsonLogfile    "log"             "JSON log stream")
       <*> optional
           (parseHostDeduction "host-from-log-filename"
                                                "Derive hostname from log filename: logs-HOSTNAME.*"))
   , op "dump-logobjects" "Dump lifted log object streams, alongside input files"
     (DumpLogObjects & pure)
   ]) <|>

  subparser (mconcat [ commandGroup "Block propagation:  machine views"
   , op "build-mach-views" "Build machine chain views"
     (BuildMachViews & pure)
   , op "dump-mach-views" "Dump machine chain views, alongside input files"
     (DumpMachViews & pure)
   , op "read-mach-views" "Read machine chain views, associated with their JSON log file inputs"
     (ReadMachViews
       <$> some
       (optJsonLogfile     "log"             "JSON log stream"))
   ]) <|>

   subparser (mconcat [ commandGroup "Block propagation:  chain input"
   , op "build-chain" "Rebuild chain"
     (RebuildChain & pure)
   , op "read-chain" "Read reconstructed chain"
     (ReadChain
       <$> optJsonInputFile  "chain"         "Block event stream (JSON)")
   , op "dump-chain-raw" "Dump raw chain"
     (DumpChainRaw
       <$> optJsonOutputFile "chain"         "JSON unfiltered chain output file")
   , op "timeline-chain-raw" "Render unfiltered chain timeline"
     (TimelineChainRaw
       <$> optTextOutputFile "timeline"      "Render a human-readable reconstructed unfiltered chain view")
   ]) <|>

   subparser (mconcat [ commandGroup "Block propagation:  chain filtering"
   , op "filter-chain" "Filter chain"
     (FilterChain
       <$> many
       (argChainFilterset  "filter"          "JSON list of block/slot selection criteria"))
   , op "dump-chain" "Dump filtered chain"
     (DumpChain
       <$> optJsonOutputFile "chain"         "JSON filtered chain output file")
   , op "timeline-chain" "Render chain timeline"
     (TimelineChain
       <$> optTextOutputFile "timeline"      "Render a human-readable reconstructed chain view")
   ]) <|>

   subparser (mconcat [ commandGroup "Machine performance analysis:  slot stats"
   , op "collect-slots" "Collect per-slot performance stats"
     (CollectSlots & pure)
   , op "dump-slots-raw" "Dump unfiltered slot stats JSON streams, alongside input files"
     (DumpSlotsRaw & pure)
   , op "filter-slots" "Filter per-slot performance stats"
     (FilterSlots
       <$> many
       (argChainFilterset  "filter"          "JSON list of slot selection criteria"))
   , op "dump-slots" "Dump filtered slot stats stream, alongside input files"
     (DumpSlots & pure)
   , op "timeline-slots" "Render machine slot timelines, alongside input files"
     (TimelineSlots & pure)
   ]) <|>

   subparser (mconcat [ commandGroup "Block propagation:  analysis"
   , op "propagation" "Block propagation analysis from chain"
     (Propagation & pure)
   , op "dump-propagation" "Dump block propagation analysis as JSON"
     (DumpPropagation
       <$> optJsonOutputFile "prop"          "JSON block propagation output file")
   , op "read-propagations" "Read block propagation analyses from JSON files"
     (ReadPropagations
       <$> some
       (optJsonInputFile     "prop"          "JSON block propagation input file"))
   , op "summarise-propagations" "Summarise a multi-run block propagation analysis"
     (SummarisePropagations & pure)
   , op "dump-multi-propagation" "Dump a multi-run block propagation analysis as JSON"
     (DumpMultiPropagation
       <$> optJsonOutputFile "multi-prop"         "JSON multi-run block propagation output file")
   ]) <|>

   subparser (mconcat [ commandGroup "Machine performance:  analysis"
   , op "machperf" "Statistical analysis on per-slot machine data"
     (Cardano.Command.MachPerf & pure)
   , op "dump-machperf" "Dump machine performance analysis as JSON, alongside input files"
     (DumpMachPerf & pure)
   ]) <|>

   subparser (mconcat [ commandGroup "Cluster performance:  analysis"
   , op "clusterperf" "Summarise machine performances into cluster performance"
     (ClusterPerf & pure)
   , op "dump-clusterperf" "Dump cluster performance stats as JSON"
     (DumpClusterPerf
       <$> optJsonOutputFile "clusterperf"   "JSON cluster performance output file")

   , op "read-clusterperfs" "Read multi-run cluster performance analysis as JSON"
     (ReadClusterPerfs
       <$> some
       (optJsonInputFile "clusterperf"   "JSON cluster performance input file"))
   , op "summarise-clusterperfs" "Consolidate cluster performance stats."
     (SummariseClusterPerfs & pure)
   , op "dump-multi-clusterperf" "Dump cluster performance stats as JSON"
     (DumpMultiClusterPerf
       <$> optJsonOutputFile "multi-clusterperf"  "JSON block propagation output file")
   ]) <|>

   subparser (mconcat [ commandGroup "Block propagation:  single-run reports"
   , op "report-prop-forger" "Render forger propagation report"
     (ReportPropForger
       <$> optTextOutputFile "report"        "Render a human-readable propagation report")
   , op "report-prop-peers" "Render peer propagation report"
     (ReportPropPeers
       <$> optTextOutputFile "report"        "Render a human-readable propagation report")
   , op "report-prop-endtoend" "Render end-to-end propagation report"
     (ReportPropEndToEnd
       <$> optTextOutputFile "report"        "Render a human-readable propagation report")
   , op "report-prop-full" "Render all propagation reports combined as a single table"
     (ReportPropFull
       <$> optTextOutputFile "report"        "Render a human-readable propagation report")
   ]) <|>

   subparser (mconcat [ commandGroup "Block propagation:  multi-run reports"
   , op "report-multi-prop-forger" "Render forger propagation report"
     (ReportMultiPropForger
       <$> optTextOutputFile "report"        "Render a human-readable propagation multi-run report summary")
   , op "report-multi-prop-peers" "Render peer propagation report"
     (ReportMultiPropPeers
       <$> optTextOutputFile "report"        "Render a human-readable propagation multi-run report summary")
   , op "report-multi-prop-endtoend" "Render end-to-end propagation report"
     (ReportMultiPropEndToEnd
       <$> optTextOutputFile "report"        "Render a human-readable propagation multi-run report summary")
   , op "report-multi-prop-full" "Render all propagation reports combined as a single table"
     (ReportMultiPropFull
       <$> optTextOutputFile "report"        "Render a human-readable propagation multi-run report summary")
   ]) <|>

   subparser (mconcat [ commandGroup "Machine performance:  reports"
   , op "report-machperf-full" "Render machine performance reports, alongside input files"
     (ReportMachPerfFull & pure)
   , op "report-machperf-brief" "Render machine performance reports, alongside input files"
     (ReportMachPerfBrief & pure)
   ]) <|>

   subparser (mconcat [ commandGroup "Cluster performance:  single-run reports"
   , op "report-clusterperf-full" "Render cluster performance report"
     (ReportClusterPerfFull
       <$> optTextOutputFile "report"        "Render a human-readable cluster performance report")
   , op "report-clusterperf-brief" "Render cluster performance report"
     (ReportClusterPerfBrief
       <$> optTextOutputFile "report"        "Render a human-readable cluster performance report")
   ]) <|>

   subparser (mconcat [ commandGroup "Cluster performance:  multi-run reports"
   , op "report-multi-clusterperf-full" "Render cluster performance report"
     (ReportMultiClusterPerfFull
       <$> optTextOutputFile "report"        "Render a human-readable cluster performance multi-run report summary")
   , op "report-multi-clusterperf-brief" "Render cluster performance report"
     (ReportMultiClusterPerfBrief
       <$> optTextOutputFile "report"        "Render a human-readable cluster performance multi-run report summary")
   ])
 where
   op :: String -> String -> Parser a -> Mod CommandFields a
   op c descr p =
     command c $ info (p <**> helper) $
       mconcat [ progDesc descr ]

   parseHostDeduction :: String -> String -> Parser HostDeduction
   parseHostDeduction name desc =
     Opt.flag' HostFromLogfilename
     (  Opt.long name
     <> Opt.help desc
     )

data ChainCommand
  = ListLogobjectKeys
      TextOutputFile
  | ListLogobjectKeysLegacy
      TextOutputFile

  | MetaGenesis         -- () -> Run
      JsonRunMetafile
      JsonGenesisFile

  | Unlog               -- () -> [(JsonLogfile, [LogObject])]
      [JsonLogfile]
      (Maybe HostDeduction)
  | DumpLogObjects

  | BuildMachViews      -- [(JsonLogfile, [LogObject])] -> [(JsonLogfile, MachView)]
  | DumpMachViews
  | ReadMachViews
      [JsonLogfile]

  | RebuildChain        -- [(JsonLogfile, MachView)] -> [BlockEvents]
  | ReadChain           -- () -> [BlockEvents]
      JsonInputFile
  | DumpChainRaw
      JsonOutputFile
  | TimelineChainRaw
      TextOutputFile

  | FilterChain         -- [ChainFilter] -> [BlockEvents] -> [BlockEvents]
      [JsonFilterFile]
  | DumpChain
      JsonOutputFile
  | TimelineChain
      TextOutputFile

  | CollectSlots
  | DumpSlotsRaw

  | FilterSlots
      [JsonFilterFile]
  | DumpSlots
  | TimelineSlots

  | Propagation
  | DumpPropagation
      JsonOutputFile
  | ReadPropagations
      [JsonInputFile]
  | SummarisePropagations
  | DumpMultiPropagation
      JsonOutputFile

  | MachPerf
  | DumpMachPerf

  | ClusterPerf
  | DumpClusterPerf
      JsonOutputFile

  | ReadClusterPerfs
      [JsonInputFile]
  | SummariseClusterPerfs
  | DumpMultiClusterPerf
      JsonOutputFile

  | ReportPropForger
      TextOutputFile
  | ReportPropPeers
      TextOutputFile
  | ReportPropEndToEnd
      TextOutputFile
  | ReportPropFull
      TextOutputFile

  | ReportMultiPropForger
      TextOutputFile
  | ReportMultiPropPeers
      TextOutputFile
  | ReportMultiPropEndToEnd
      TextOutputFile
  | ReportMultiPropFull
      TextOutputFile

  | ReportMachPerfFull
  | ReportMachPerfBrief

  | ReportClusterPerfFull
      TextOutputFile
  | ReportClusterPerfBrief
      TextOutputFile

  | ReportMultiClusterPerfFull
      TextOutputFile
  | ReportMultiClusterPerfBrief
      TextOutputFile

  deriving Show

data State
  = State
  { -- common
    sFilters       :: [FilterName]
  , sTags          :: [Text]
  , sRun           :: Maybe Run
  , sObjLists      :: Maybe [(JsonLogfile, [LogObject])]
  , sDomSlots      :: Maybe (DataDomain SlotNo)
  , sDomBlocks     :: Maybe (DataDomain BlockNo)
    -- propagation
  , sMachViews     :: Maybe [(JsonLogfile, MachView)]
  , sChainRaw      :: Maybe [BlockEvents]
  , sChain         :: Maybe [BlockEvents]
  , sBlockProp     :: Maybe [BlockPropOne]
  , sBlockProps    :: Maybe BlockProps
    -- performance
  , sSlotsRaw      :: Maybe [(JsonLogfile, [SlotStats])]
  , sScalars       :: Maybe [(JsonLogfile, RunScalars)]
  , sSlots         :: Maybe [(JsonLogfile, [SlotStats])]
  , sMachPerf      :: Maybe [(JsonLogfile, MachPerfOne)]
  , sClusterPerf   :: Maybe [ClusterPerf]
  , sClusterPerfs  :: Maybe ClusterPerfs
  }

sRunAnchor :: State -> Anchor
sRunAnchor State{sRun = Just run, sFilters} = runAnchor run sFilters
sRunAnchor _ = error "sRunAnchor with no run."

sTagsAnchor :: State -> Anchor
sTagsAnchor State{sFilters, sTags} = tagsAnchor sTags sFilters

runChainCommand :: State -> ChainCommand -> ExceptT CommandError IO State

runChainCommand s
  c@(ListLogobjectKeys f) = do
  dumpText "logobject-keys" (toText <$> logObjectStreamInterpreterKeys) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand s
  c@(ListLogobjectKeysLegacy f) = do
  dumpText "logobject-keys-legacy" (toText <$> logObjectStreamInterpreterKeysLegacy) f
    & firstExceptT (CommandError c)
  pure s

runChainCommand s
  c@(MetaGenesis runMeta shelleyGenesis) = do
  run <- readRun shelleyGenesis runMeta
         & firstExceptT (fromAnalysisError c)
  pure s { sRun = Just run }

runChainCommand s
  c@(Unlog logs mHostDed) = do
  los <- runLiftLogObjects logs mHostDed
         & firstExceptT (CommandError c)
  pure s { sObjLists = Just los }

runChainCommand s@State{sObjLists=Just objs}
  c@DumpLogObjects = do
  dumpAssociatedObjectStreams "logobjs" objs & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpLogObjects = missingCommandData c
  ["lifted log objects"]

-- runChainCommand s c@(ReadMachViews _ _)    -- () -> [(JsonLogfile, MachView)]

runChainCommand s@State{sRun=Just run, sObjLists=Just objs}
  BuildMachViews = do
  mvs <- buildMachViews run objs & liftIO
  pure s { sMachViews = Just mvs }
runChainCommand _ c@BuildMachViews = missingCommandData c
  ["run metadata & genesis", "lifted logobjects"]

runChainCommand s@State{sMachViews=Just machViews}
  c@DumpMachViews = do
  dumpAssociatedObjects "mach-views" machViews & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpMachViews = missingCommandData c
  ["machine views"]

runChainCommand s c@(ReadMachViews fs) = do
  machViews <- readAssociatedObjects "mach-views" fs & firstExceptT (CommandError c)
  pure s { sMachViews = Just machViews }

runChainCommand s@State{sRun=Just run, sMachViews=Just mvs}
  RebuildChain = do
  chainRaw <- rebuildChain run mvs & liftIO
  pure s { sChainRaw = Just chainRaw }
runChainCommand _ c@RebuildChain = missingCommandData c
  ["run metadata & genesis", "reconstructed chain"]

runChainCommand s
  c@(ReadChain f) = do
  chainRaw <- mapM (Aeson.eitherDecode @BlockEvents)
              . filter ((> 5) . LBS.length)
              . LBS.split '\n'
              <$> LBS.readFile (unJsonInputFile f)
              & newExceptT
              & firstExceptT (CommandError c . pack)
  pure s { sChainRaw = Just chainRaw, sChain = Just chainRaw }

runChainCommand s@State{sChainRaw=Just chainRaw}
  c@(DumpChainRaw f) = do
  dumpObjects "raw-chain" chainRaw f & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpChainRaw{} = missingCommandData c
  ["unfiltered chain"]

runChainCommand s@State{sRun=Just run, sChainRaw=Just chainRaw}
  c@(TimelineChainRaw f) = do
  dumpText "chain" (renderTimeline run (const True) False chainRaw) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@TimelineChainRaw{} = missingCommandData c
  ["run metadata & genesis", "unfiltered chain"]

runChainCommand s@State{sRun=Just run, sChainRaw=Just chainRaw}
  c@(FilterChain fltfs) = do
  flts <- readFilters fltfs
          & firstExceptT (CommandError c)
  (domSlot, domBlock, fltNames, chain) <- filterChain run flts chainRaw
                                & liftIO & firstExceptT (CommandError c)
  when (ddFilteredCount domBlock == 0) $
    throwE $ CommandError c $ mconcat
      [ "All ", show (ddRawCount domBlock), " blocks filtered out." ]
  pure s { sChain = Just chain, sDomSlots = Just domSlot, sDomBlocks = Just domBlock, sFilters = fltNames }
runChainCommand _ c@FilterChain{} = missingCommandData c
  ["run metadata & genesis", "unfiltered slot stats"]

runChainCommand s@State{sChain=Just chain}
  c@(DumpChain f) = do
  dumpObjects "chain" chain f & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpChain{} = missingCommandData c
  ["filtered chain"]

runChainCommand s@State{sRun=Just run, sChain=Just chain}
  c@(TimelineChain f) = do
  dumpText "chain" (renderTimeline run (const True) False chain) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@TimelineChain{} = missingCommandData c
  ["run metadata & genesis", "filtered chain"]

runChainCommand s@State{sRun=Just run, sObjLists=Just objs}
  c@CollectSlots = do
  (scalars, slotsRaw) <-
    fmap (mapAndUnzip redistribute) <$> collectSlotStats run objs
    & newExceptT
    & firstExceptT (CommandError c)
  pure s { sScalars = Just scalars, sSlotsRaw = Just slotsRaw }
runChainCommand _ c@CollectSlots = missingCommandData c
  ["run metadata & genesis", "lifted logobjects"]

runChainCommand s@State{sSlotsRaw=Just slotsRaw}
  c@DumpSlotsRaw = do
  dumpAssociatedObjectStreams "raw-slots" slotsRaw & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpSlotsRaw = missingCommandData c
  ["unfiltered slots"]

runChainCommand s@State{sRun=Just run, sSlotsRaw=Just slotsRaw}
  c@(FilterSlots fltfs) = do
  flts <- readFilters fltfs
          & firstExceptT (CommandError c)
  (domSlots, fltrd) <- runSlotFilters run flts slotsRaw
                       & liftIO
                       & firstExceptT (CommandError c)
  when (maximum (length . snd <$> fltrd) == 0) $
    throwE $ CommandError c $ mconcat
      [ "All ", show $ maximum (length . snd <$> slotsRaw), " slots filtered out." ]
  pure s { sSlots = Just fltrd, sDomSlots = Just domSlots }
runChainCommand _ c@FilterSlots{} = missingCommandData c
  ["run metadata & genesis", "unfiltered slot stats"]

runChainCommand s@State{sSlots=Just slots}
  c@DumpSlots = do
  dumpAssociatedObjectStreams "slots" slots & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpSlots = missingCommandData c
  ["filtered slots"]

runChainCommand s@State{sRun=Just run, sSlots=Just slots}
  c@TimelineSlots = do
  dumpAssociatedTextStreams "mach"
    (fmap (fmap $ renderTimeline run (const True) False) slots)
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@TimelineSlots{} = missingCommandData c
  ["run metadata & genesis", "filtered slots"]

runChainCommand s@State{sRun=Just run, sChain=Just chain, sDomSlots=Just domS, sDomBlocks=Just domB}
  Propagation = do
  prop <- blockProp run chain domS domB & liftIO
  pure s { sBlockProp = Just [prop] }
runChainCommand _ c@Propagation = missingCommandData c
  ["run metadata & genesis", "filtered chain", "data domains for slots & blocks"]

runChainCommand s@State{sBlockProp=Just [prop]}
  c@(DumpPropagation f) = do
  dumpObject "blockprop" prop f & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpPropagation{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{}
  c@(ReadPropagations fs) = do
  xs <- mapConcurrently (fmap (Aeson.eitherDecode @BlockPropOne) . LBS.readFile . unJsonInputFile) fs
    & fmap sequence
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sBlockProp = Just xs
         , sTags = pack . takeBaseName . takeDirectory . takeDirectory . unJsonInputFile <$> fs }

runChainCommand s@State{sBlockProp=Just props}
  c@SummarisePropagations = do
  xs <- pure (summariseBlockProps (nEquicentiles $ max 7 (length props)) props)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sBlockProps = Just xs }
runChainCommand _ c@SummarisePropagations{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{sBlockProps=Just props}
  c@(DumpMultiPropagation f) = do
  dumpObject "blockprops" props f & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpMultiPropagation{} = missingCommandData c
  ["multi-run block propagations"]

runChainCommand s@State{sRun=Just run, sSlots=Just slots}
  c@Cardano.Command.MachPerf = do
  perf <- mapConcurrentlyPure (slotStatsMachPerf run) slots
          & fmap sequence
          & newExceptT
          & firstExceptT (CommandError c)
  pure s { sMachPerf = Just perf }
runChainCommand _ c@Cardano.Command.MachPerf{} = missingCommandData c
  ["run metadata & genesis", "filtered slots"]

runChainCommand s@State{sMachPerf=Just perf}
  c@DumpMachPerf = do
  dumpAssociatedObjects "perf-stats" perf
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpMachPerf = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sMachPerf=Just machPerfs}
  c@Cardano.Command.ClusterPerf = do
  clusterPerf <- pure (summariseClusterPerf (nEquicentiles $ max 7 (length machPerfs)) (machPerfs <&> snd))
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sClusterPerf = Just [clusterPerf] }
runChainCommand _ c@Cardano.Command.ClusterPerf{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sClusterPerf=Just [perf]}
  c@(DumpClusterPerf f) = do
  dumpObject "cluster-perf" perf f & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpClusterPerf{} = missingCommandData c
  ["cluster performance stats"]

runChainCommand s@State{}
  c@(ReadClusterPerfs fs) = do
  xs <- mapConcurrently (fmap (Aeson.eitherDecode @ClusterPerf) . LBS.readFile . unJsonInputFile) fs
    & fmap sequence
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sClusterPerf = Just xs
         , sTags = pack . takeBaseName . takeDirectory . takeDirectory . unJsonInputFile <$> fs }

runChainCommand s@State{sClusterPerf=Just perfs}
  c@SummariseClusterPerfs = do
  xs <- pure (summariseClusterPerfs (nEquicentiles $ max 7 (length perfs)) perfs)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sClusterPerfs = Just xs }
runChainCommand _ c@SummariseClusterPerfs{} = missingCommandData c
  ["cluster performance stats"]

runChainCommand s@State{sClusterPerfs=Just mcp}
  c@(DumpMultiClusterPerf f) = do
  dumpObject "multi-cluster-perf" mcp f & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpMultiClusterPerf{} = missingCommandData c
  ["multi-run cluster performance stats"]

runChainCommand s@State{sBlockProp=Just (prop:_)}
  c@(ReportPropForger f) = do
  dumpText "blockprop-forger"
    (renderCDFs (sRunAnchor s) RenderPretty bpFieldsForger Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropForger{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{sBlockProp=Just (prop:_)}
  c@(ReportPropPeers f) = do
  dumpText "blockprop-peers"
    (renderCDFs (sRunAnchor s) RenderPretty bpFieldsPeers Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropPeers{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{sBlockProp=Just (prop:_)}
  c@(ReportPropEndToEnd f) = do
  dumpText "blockprop-endtoend"
    (renderCDFs (sRunAnchor s) RenderPretty bpFieldsPropagation (Just briefCentiles) prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropEndToEnd{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{sBlockProp=Just (prop:_)}
  c@(ReportPropFull f) = do
  dumpText "blockprop-full" (renderCDFs (sRunAnchor s) RenderPretty (const True) Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropFull{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{sBlockProps=Just prop}
  c@(ReportMultiPropForger f) = do
  dumpText "blockprop-multi-forger"
    (renderCDFs (sTagsAnchor s) RenderPretty bpFieldsForger Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMultiPropForger{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{sBlockProps=Just prop}
  c@(ReportMultiPropPeers f) = do
  dumpText "blockprop-multi-peers"
    (renderCDFs (sTagsAnchor s) RenderPretty bpFieldsPeers Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMultiPropPeers{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{sBlockProps=Just prop}
  c@(ReportMultiPropEndToEnd f) = do
  dumpText "blockprop-multi-endtoend"
    (renderCDFs (sTagsAnchor s) RenderPretty bpFieldsPropagation (Just briefCentiles) prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMultiPropEndToEnd{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{sBlockProps=Just prop}
  c@(ReportMultiPropFull f) = do
  dumpText "blockprop-multi-full"
    (renderCDFs (sTagsAnchor s) RenderPretty (const True) Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMultiPropFull{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{sMachPerf=Just perf}
  c@ReportMachPerfFull = do
  dumpAssociatedTextStreams "machperf-full"
    (fmap (fmap $ renderCDFs (sRunAnchor s) RenderPretty (const True) Nothing) perf)
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMachPerfFull{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sMachPerf=Just perf}
  c@ReportMachPerfBrief = do
  dumpAssociatedTextStreams "machperf-brief"
    (fmap (fmap $ renderCDFs (sRunAnchor s) RenderPretty mtFieldsReport Nothing) perf)
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMachPerfBrief{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sClusterPerf=Just [perf]}
  c@(ReportClusterPerfFull f) = do
  dumpText "clusterperf-full"
    (renderCDFs (sRunAnchor s) RenderPretty (const True) Nothing perf) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportClusterPerfFull{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sClusterPerf=Just [perf]}
  c@(ReportClusterPerfBrief f) = do
  dumpText "clusterperf-brief"
    (renderCDFs (sRunAnchor s) RenderPretty mtFieldsReport Nothing perf) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportClusterPerfBrief{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sClusterPerfs=Just (ClusterPerfs perf)}
  c@(ReportMultiClusterPerfFull f) = do
  dumpText "clusterperf-full"
    (renderCDFs (sTagsAnchor s) RenderPretty (const True) Nothing perf) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMultiClusterPerfFull{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sClusterPerfs=Just (ClusterPerfs perf)}
  c@(ReportMultiClusterPerfBrief f) = do
  dumpText "clusterperf-brief"
    (renderCDFs (sTagsAnchor s) RenderPretty mtFieldsReport Nothing perf) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMultiClusterPerfBrief{} = missingCommandData c
  ["machine performance stats"]


missingCommandData :: ChainCommand -> [String] -> ExceptT CommandError IO a
missingCommandData c xs =
  throwError $ CommandError c (pack $ printf "Some of the following data was missing:  %s" $ intercalate ", " xs)

_reportBanner :: [FilterName] -> FilePath -> Text
_reportBanner fnames inputfp = mconcat
  [ "input: ", logfileRunIdentifier (pack inputfp), " "
  , "locli: ", gitRev getVersion, " "
  , "filters: ", T.intercalate " " (unFilterName <$> fnames)
  ]
 where
   -- | This estimates the run identifier, given the expected path structure.
   logfileRunIdentifier :: Text -> Text
   logfileRunIdentifier fp =
     case drop (length xs - 3) xs of
       rundir:_ -> rundir
       _ -> fp
    where
      xs = T.split (== '/') fp

fromAnalysisError :: ChainCommand -> AnalysisCmdError -> CommandError
fromAnalysisError c (AnalysisCmdError t) = CommandError c t
fromAnalysisError c o = CommandError c (show o)

runCommand :: Command -> ExceptT CommandError IO ()

runCommand (ChainCommand cs) =
  foldM_ runChainCommand initialState cs
 where
   initialState :: State
   initialState =
     State [] []
           Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing

opts :: ParserInfo Command
opts =
  Opt.info (ChainCommand
              <$> some parseChainCommand
            <**> Opt.helper)
    ( Opt.fullDesc
      <> Opt.header
      "locli - high-level analyses on JSON log files emitted by cardano-node."
    )

pref :: ParserPrefs
pref = Opt.prefs showHelpOnEmpty
