{-# OPTIONS_GHC -fmax-pmcheck-models=5000 #-}
module Cardano.Command (module Cardano.Command) where

import Cardano.Prelude                  hiding (State)

import Data.Aeson                       qualified as Aeson
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        (pack)
import Data.Text                        qualified as T
import Data.Text.Short                  (toText)
import Options.Applicative
import Options.Applicative              qualified as Opt

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Ground
import Cardano.Analysis.MachPerf
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.LogObject          hiding (Text)
import Cardano.Unlog.Render
import Data.Distribution

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
       <$> optJsonOutputFile "analysis"      "JSON block propagation output file")
   ]) <|>

   subparser (mconcat [ commandGroup "Machine performance:  analysis"
   , op "machperf" "Statistical analysis on per-slot machine data"
     (Cardano.Command.MachPerf & pure)
   , op "dump-machperf" "Dump machine performance analysis as JSON, alongside input files"
     (DumpMachPerf & pure)
   ]) <|>

   subparser (mconcat [ commandGroup "Cluster performance:  analysis"
   , op "clusterperf" "Consolidate machine performance stats to cluster stats."
     (Cardano.Command.ClusterPerf & pure)
   , op "dump-clusterperf" "Dump cluster performance stats as JSON"
     (DumpClusterPerf
       <$> optJsonOutputFile "analysis"      "JSON block propagation output file")
   ]) <|>

   subparser (mconcat [ commandGroup "Block propagation:  reports"
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

   subparser (mconcat [ commandGroup "Machine performance:  reports"
   , op "report-perf-full" "Render machine performance reports, alongside input files"
     (ReportMachPerfFull & pure)
   , op "report-perf-brief" "Render machine performance reports, alongside input files"
     (ReportMachPerfBrief & pure)
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

  | MachPerf
  | DumpMachPerf
  | ClusterPerf
  | DumpClusterPerf
      JsonOutputFile

  | ReportPropForger
      TextOutputFile
  | ReportPropPeers
      TextOutputFile
  | ReportPropEndToEnd
      TextOutputFile
  | ReportPropFull
      TextOutputFile

  | ReportMachPerfFull
  | ReportMachPerfBrief

  deriving Show

data State
  = State
  { -- common
    sRun           :: Maybe Run
  , sObjLists      :: Maybe [(JsonLogfile, [LogObject])]
  , sDomSlots      :: Maybe (DataDomain SlotNo)
  , sDomBlocks     :: Maybe (DataDomain BlockNo)
    -- propagation
  , sMachViews     :: Maybe [(JsonLogfile, MachView)]
  , sChainRaw      :: Maybe [BlockEvents]
  , sChain         :: Maybe [BlockEvents]
  , sBlockProp     :: Maybe BlockPropagation
    -- performance
  , sSlotsRaw      :: Maybe [(JsonLogfile, [SlotStats])]
  , sScalars       :: Maybe [(JsonLogfile, RunScalars)]
  , sSlots         :: Maybe [(JsonLogfile, [SlotStats])]
  , sMachPerf      :: Maybe [(JsonLogfile, MachPerf)]
  , sClusterPerf   :: Maybe ClusterPerf
  }

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
  (domSlot, domBlock, chain) <- filterChain run flts chainRaw
                                & liftIO & firstExceptT (CommandError c)
  when (ddFilteredCount domBlock == 0) $
    throwE $ CommandError c $ mconcat
      [ "All ", show (ddRawCount domBlock), " blocks filtered out." ]
  pure s { sChain = Just chain, sDomSlots = Just domSlot, sDomBlocks = Just domBlock }
runChainCommand _ c@FilterChain{} = missingCommandData c
  ["run metadata & genesis", "slot filters", "unfiltered slot stats"]

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
  ["run metadata & genesis", "slot filters", "unfiltered slot stats"]

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
  pure s { sBlockProp = Just prop }
runChainCommand _ c@Propagation = missingCommandData c
  ["run metadata & genesis", "filtered chain", "data domains for slots & blocks"]

runChainCommand s@State{sBlockProp=Just prop}
  c@(DumpPropagation f) = do
  dumpObject "blockprop" prop f & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpPropagation{} = missingCommandData c
  ["block propagation"]

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

-- runChainCommand s@State{sRun=Just run, sSlots=Just slots, sMachPerf=Just perf}
--   c@Cardano.Command.ClusterPerf = do
--   perf <- mapConcurrentlyPure (slotStatsMachPerf run) slots
--           & fmap sequence
--           & newExceptT
--           & firstExceptT (CommandError c)
--   pure s { sClusterPerf = Just perf }
-- runChainCommand _ c@Cardano.Command.ClusterPerf{} = missingCommandData c
--   ["run metadata & genesis", "filtered slots", "machine performance stats"]

runChainCommand s@State{sRun=Just run, sBlockProp=Just prop}
  c@(ReportPropForger f) = do
  dumpText "blockprop-forger"
    (renderDistributions run RenderPretty bpFieldsForger Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropForger{} = missingCommandData c
  ["run metadata & genesis", "block propagation"]

runChainCommand s@State{sRun=Just run, sBlockProp=Just prop}
  c@(ReportPropPeers f) = do
  dumpText "blockprop-peers"
    (renderDistributions run RenderPretty bpFieldsPeers Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropPeers{} = missingCommandData c
  ["run metadata & genesis", "block propagation"]

runChainCommand s@State{sRun=Just run, sBlockProp=Just prop}
  c@(ReportPropEndToEnd f) = do
  dumpText "blockprop-endtoend"
    (renderDistributions run RenderPretty bpFieldsPropagation (Just briefPercSpecs) prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropEndToEnd{} = missingCommandData c
  ["run metadata & genesis", "block propagation"]

runChainCommand s@State{sRun=Just run, sBlockProp=Just prop}
  c@(ReportPropFull f) = do
  dumpText "blockprop-full" (renderDistributions run RenderPretty (const True) Nothing prop) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportPropFull{} = missingCommandData c
  ["run metadata & genesis", "block propagation"]

runChainCommand s@State{sRun=Just run, sMachPerf=Just perf}
  c@ReportMachPerfFull = do
  dumpAssociatedTextStreams "perf-full"
    (fmap (fmap $ renderDistributions run RenderPretty (const True) Nothing) perf)
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMachPerfFull{} = missingCommandData c
  ["run metadata & genesis", "machine performance stats"]

runChainCommand s@State{sRun=Just run, sMachPerf=Just perf}
  c@ReportMachPerfBrief = do
  dumpAssociatedTextStreams "perf-brief"
    (fmap (fmap $ renderDistributions run RenderPretty mtFieldsReport Nothing) perf)
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@ReportMachPerfBrief{} = missingCommandData c
  ["run metadata & genesis", "machien performance stats"]


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
     State Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing
           Nothing

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
