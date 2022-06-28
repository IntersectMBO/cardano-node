{-# OPTIONS_GHC -fmax-pmcheck-models=15000 #-}
module Cardano.Command (module Cardano.Command) where

import Cardano.Prelude                  hiding (State, head)

import Data.Aeson                       qualified as Aeson
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        (pack)
import Data.Text                        qualified as T
import Data.Text.Short                  (toText)
import Data.Time.Clock
import Options.Applicative
import Options.Applicative              qualified as Opt

import System.FilePath

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Ground
import Cardano.Analysis.MachPerf
import Cardano.Analysis.Run
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
     (CollectSlots
       <$> many
           (optJsonLogfile    "ignore-log"   "Omit data from listed log files from perf statistics"))
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
   , op "compute-propagation" "Block propagation analysis from chain"
     (ComputePropagation & pure)
   , op "render-propagation" "Write block propagation analysis results"
     (writerOpts RenderPropagation "Render"
      <*> parsePropSubset)
   , op "read-propagations" "Read block propagation analyses from JSON files"
     (ReadPropagations
       <$> some
       (optJsonInputFile     "prop"          "JSON block propagation input file"))
   , op "compute-multi-propagation" "Summarise a multi-run block propagation analysis"
     (ComputeMultiPropagation & pure)
   , op "render-multi-propagation" "Write multi-run block propagation stats"
     (writerOpts RenderMultiPropagation "Render"
      <*> parsePropSubset)
   ]) <|>

   subparser (mconcat [ commandGroup "Machine performance:  analysis"
   , op "compute-machperf" "Statistical analysis on per-slot machine data"
     (ComputeMachPerf & pure)
   , op "render-machperf" "Write machine performance analysis results, along input files"
     (pure $ RenderMachPerf AsPretty PerfFull)
   ]) <|>

   subparser (mconcat [ commandGroup "Cluster performance:  analysis"
   , op "compute-clusterperf" "Summarise machine performances into cluster performance"
     (ComputeClusterPerf & pure)
   , op "render-clusterperf" "Write cluster performance stats"
     (writerOpts RenderClusterPerf "Render"
      <*> parsePerfSubset)

   , op "read-clusterperfs" "Read multi-run cluster performance analysis as JSON"
     (ReadMultiClusterPerf
       <$> some
       (optJsonInputFile "clusterperf"   "JSON cluster performance input file"))
   , op "compute-multi-clusterperf" "Consolidate cluster performance stats."
     (ComputeMultiClusterPerf & pure)
   , op "render-multi-clusterperf" "Write multi-run cluster performance results"
     (writerOpts RenderMultiClusterPerf "Render"
      <*> parsePerfSubset)
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

parseRenderMode :: Parser RenderMode
parseRenderMode =
  [ Opt.flag' AsJSON    (Opt.long "json"    <> Opt.help "Full JSON dump output file")
  , Opt.flag' AsGnuplot (Opt.long "gnuplot" <> Opt.help "%s-pattern for separate Gnuplot output files, per CDF")
  , Opt.flag' AsOrg     (Opt.long "org"     <> Opt.help "Org mode table output file")
  , Opt.flag' AsReport  (Opt.long "report"  <> Opt.help "Org mode table output file, brief stats")
  , Opt.flag' AsPretty  (Opt.long "pretty"  <> Opt.help "Text report output file")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."

writerOpt :: (RenderMode -> TextOutputFile -> a) -> String -> RenderMode -> Parser a
writerOpt ctor desc mode = ctor mode <$> optTextOutputFile opt (desc <> descSuf)
 where
   (,) opt descSuf = optDescSuf mode
   optDescSuf :: RenderMode -> (String, String)
   optDescSuf = \case
     AsJSON    -> (,) "json"    " results as complete JSON dump"
     AsGnuplot -> (,) "gnuplot" " as individual Gnuplot files"
     AsOrg     -> (,) "org"     " as Org-mode table"
     AsReport  -> (,) "report"  " as Org-mode summary table"
     AsPretty  -> (,) "pretty"  " as text report"

writerOpts :: (RenderMode -> TextOutputFile -> a) -> String -> Parser a
writerOpts ctor desc = enumFromTo minBound maxBound
                       <&> writerOpt ctor desc
                       & \case
                           (x:xs) -> foldl (<|>) x xs
                           [] -> error "Crazy world."

data ChainCommand
  = ListLogobjectKeys       TextOutputFile
  | ListLogobjectKeysLegacy TextOutputFile

  |        MetaGenesis      JsonRunMetafile JsonGenesisFile

  |        Unlog            [JsonLogfile] (Maybe HostDeduction)
  |        DumpLogObjects

  |        BuildMachViews
  |         DumpMachViews
  |         ReadMachViews   [JsonLogfile]

  |         RebuildChain
  |            ReadChain    JsonInputFile
  |            DumpChainRaw JsonOutputFile
  |        TimelineChainRaw TextOutputFile
  |          FilterChain    [JsonFilterFile]
  |            DumpChain    JsonOutputFile
  |        TimelineChain    TextOutputFile

  |         CollectSlots    [JsonLogfile]
  |            DumpSlotsRaw
  |          FilterSlots    [JsonFilterFile]
  |            DumpSlots
  |        TimelineSlots

  |      ComputePropagation
  |       RenderPropagation RenderMode TextOutputFile PropSubset
  |        ReadPropagations [JsonInputFile]

  | ComputeMultiPropagation
  |  RenderMultiPropagation RenderMode TextOutputFile PropSubset

  |         ComputeMachPerf
  |          RenderMachPerf RenderMode PerfSubset

  |      ComputeClusterPerf
  |       RenderClusterPerf RenderMode TextOutputFile PerfSubset

  |    ReadMultiClusterPerf [JsonInputFile]
  | ComputeMultiClusterPerf
  |  RenderMultiClusterPerf RenderMode TextOutputFile PerfSubset

  deriving Show

data State
  = State
  { -- common
    sWhen             :: UTCTime
  , sFilters          :: [FilterName]
  , sTags             :: [Text]
  , sRun              :: Maybe Run
  , sObjLists         :: Maybe [(JsonLogfile, [LogObject])]
  , sDomSlots         :: Maybe (DataDomain SlotNo)
  , sDomBlocks        :: Maybe (DataDomain BlockNo)
    -- propagation
  , sMachViews        :: Maybe [(JsonLogfile, MachView)]
  , sChainRaw         :: Maybe [BlockEvents]
  , sChain            :: Maybe [BlockEvents]
  , sBlockProp        :: Maybe [BlockPropOne]
  , sMultiBlockProp   :: Maybe MultiBlockProp
    -- performance
  , sSlotsRaw         :: Maybe [(JsonLogfile, [SlotStats])]
  , sScalars          :: Maybe [(JsonLogfile, RunScalars)]
  , sSlots            :: Maybe [(JsonLogfile, [SlotStats])]
  , sMachPerf         :: Maybe [(JsonLogfile, MachPerfOne)]
  , sClusterPerf      :: Maybe [ClusterPerf]
  , sMultiClusterPerf :: Maybe MultiClusterPerf
  }

sRunAnchor :: State -> Anchor
sRunAnchor State{sRun = Just run, sFilters, sWhen, sDomSlots, sDomBlocks}
  = runAnchor run sWhen sFilters sDomSlots sDomBlocks
sRunAnchor _ = error "sRunAnchor with no run."

sTagsAnchor :: State -> Anchor
sTagsAnchor State{sFilters, sTags, sWhen, sDomSlots, sDomBlocks}
  = tagsAnchor sTags sWhen sFilters sDomSlots sDomBlocks

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
  c@(CollectSlots ignores) = do
  let nonIgnored = flip filter objs $ (`notElem` ignores) . fst
  forM_ ignores $
    progress "perf-ignored-log" . R . unJsonLogfile
  (scalars, slotsRaw) <-
    fmap (mapAndUnzip redistribute) <$> collectSlotStats run nonIgnored
    & newExceptT
    & firstExceptT (CommandError c)
  pure s { sScalars = Just scalars, sSlotsRaw = Just slotsRaw }
runChainCommand _ c@CollectSlots{} = missingCommandData c
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
  ComputePropagation = do
  prop <- blockProp run chain domS domB & liftIO
  pure s { sBlockProp = Just [prop] }
runChainCommand _ c@ComputePropagation = missingCommandData c
  ["run metadata & genesis", "filtered chain", "data domains for slots & blocks"]

runChainCommand s@State{sBlockProp=Just [prop]}
  c@(RenderPropagation mode f subset) = do
  forM_ (renderCDF (sRunAnchor s) (propSubsetFn subset) Nothing mode prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderPropagation{} = missingCommandData c
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
  c@ComputeMultiPropagation = do
  xs <- pure (summariseMultiBlockProp (nEquicentiles $ max 7 (length props)) props)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sMultiBlockProp = Just xs }
runChainCommand _ c@ComputeMultiPropagation{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{sMultiBlockProp=Just prop}
  c@(RenderMultiPropagation mode f subset) = do
  forM_ (renderCDF (sTagsAnchor s) (propSubsetFn subset) Nothing mode prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderMultiPropagation{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{sRun=Just run, sSlots=Just slots}
  c@ComputeMachPerf = do
  perf <- mapConcurrentlyPure (slotStatsMachPerf run) slots
          & fmap sequence
          & newExceptT
          & firstExceptT (CommandError c)
  pure s { sMachPerf = Just perf }
runChainCommand _ c@ComputeMachPerf{} = missingCommandData c
  ["run metadata & genesis", "filtered slots"]

runChainCommand s@State{sMachPerf=Just perf}
  c@(RenderMachPerf _mode _subset) = do
  dumpAssociatedObjects "perf-stats" perf
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderMachPerf{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sMachPerf=Just machPerfs}
  c@ComputeClusterPerf = do
  clusterPerf <- pure (summariseClusterPerf (nEquicentiles $ max 7 (length machPerfs)) (machPerfs <&> snd))
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sClusterPerf = Just [clusterPerf] }
runChainCommand _ c@ComputeClusterPerf{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sClusterPerf=Just [prop]}
  c@(RenderClusterPerf mode f subset) = do
  forM_ (renderCDF (sRunAnchor s) (perfSubsetFn subset) Nothing mode prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderClusterPerf{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{}
  c@(ReadMultiClusterPerf fs) = do
  xs <- mapConcurrently (fmap (Aeson.eitherDecode @ClusterPerf) . LBS.readFile . unJsonInputFile) fs
    & fmap sequence
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sClusterPerf = Just xs
         , sTags = pack . takeBaseName . takeDirectory . takeDirectory . unJsonInputFile <$> fs }

runChainCommand s@State{sClusterPerf=Just perfs}
  c@ComputeMultiClusterPerf = do
  xs <- pure (summariseMultiClusterPerf (nEquicentiles $ max 7 (length perfs)) perfs)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sMultiClusterPerf = Just xs }
runChainCommand _ c@ComputeMultiClusterPerf{} = missingCommandData c
  ["cluster performance stats"]

runChainCommand s@State{sMultiClusterPerf=Just (MultiClusterPerf perf)}
  c@(RenderMultiClusterPerf mode f subset) = do
  forM_ (renderCDF (sTagsAnchor s) (perfSubsetFn subset) Nothing mode perf) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderMultiClusterPerf{} = missingCommandData c
  ["multi-run cluster preformance stats"]


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

runCommand (ChainCommand cs) = do
  now <- liftIO getCurrentTime
  foldM_ runChainCommand (initialState now) cs
 where
   initialState :: UTCTime -> State
   initialState now =
     State now [] []
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
