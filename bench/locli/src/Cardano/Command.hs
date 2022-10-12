{-# OPTIONS_GHC -fmax-pmcheck-models=25000 #-}
module Cardano.Command (module Cardano.Command) where

import Cardano.Prelude                  hiding (State, head)

import Data.Aeson                       qualified as Aeson
import Data.ByteString                  qualified as BS
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Map.Strict                  qualified as Map
import Data.Text                        (pack)
import Data.Text                        qualified as T
import Data.Text.Short                  (toText)
import Data.Time.Clock
import Options.Applicative
import Options.Applicative              qualified as Opt

import System.FilePath
import System.Posix.Files               qualified as IO

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.MachPerf
import Cardano.Analysis.Run
import Cardano.Render
import Cardano.Unlog.LogObject          hiding (Text)
import Cardano.Report
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

data ChainCommand
  = ListLogobjectKeys       TextOutputFile
  | ListLogobjectKeysLegacy TextOutputFile

  |        MetaGenesis      (JsonInputFile RunPartial) (JsonInputFile Genesis)

  |        Unlog            [JsonLogfile] (Maybe HostDeduction) Bool [LOAnyType]
  |        DumpLogObjects

  |        BuildMachViews
  |         DumpMachViews
  |         ReadMachViews   [JsonLogfile]

  |         RebuildChain    [JsonFilterFile] [ChainFilter]
  |            DumpChain    (JsonOutputFile [BlockEvents]) (JsonOutputFile [BlockEvents])
  |            ReadChain    (JsonInputFile [BlockEvents])
  |        TimelineChain    TextOutputFile [RTComments BlockEvents]

  |         CollectSlots    [JsonLogfile]
  |            DumpSlotsRaw
  |          FilterSlots    [JsonFilterFile] [ChainFilter]
  |            DumpSlots
  |        TimelineSlots

  |      ComputePropagation
  |       RenderPropagation RenderFormat TextOutputFile PropSubset
  |        ReadPropagations [JsonInputFile BlockPropOne]
  | ComputeMultiPropagation
  |  RenderMultiPropagation RenderFormat TextOutputFile PropSubset CDF2Aspect

  |         ComputeMachPerf
  |          RenderMachPerf RenderFormat PerfSubset

  |      ComputeClusterPerf
  |       RenderClusterPerf RenderFormat TextOutputFile PerfSubset
  |        ReadClusterPerfs [JsonInputFile MultiClusterPerf]
  | ComputeMultiClusterPerf
  |  RenderMultiClusterPerf RenderFormat TextOutputFile PerfSubset CDF2Aspect

  |      ComputeSummary
  |       RenderSummary     RenderFormat TextOutputFile
  |         ReadSummaries   [JsonInputFile Summary]

  |             Compare     InputDir (Maybe TextInputFile) TextOutputFile
                            [( JsonInputFile RunPartial
                             , JsonInputFile Genesis
                             , JsonInputFile ClusterPerf
                             , JsonInputFile BlockPropOne)]

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
       <$> optJsonInputFile "run-metafile"    "The meta.json file from the benchmark run"
       <*> optJsonInputFile "shelley-genesis" "Genesis file of the run")
   ]) <|>

  subparser (mconcat [ commandGroup "Basic log objects"
   , op "unlog" "Read log files"
     (Unlog
       <$> some
           (optJsonLogfile    "log"             "JSON log stream")
       <*> optional
           (parseHostDeduction "host-from-log-filename"
                                                "Derive hostname from log filename: logs-HOSTNAME.*")
       <*> Opt.flag False True (Opt.long "lodecodeerror-ok"
                                    <> Opt.help "Allow non-EOF LODecodeError logobjects")
       <*> many
           (optLOAnyType      "ok-loany"        "[MULTI] Allow a particular LOAnyType"))
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

   subparser (mconcat [ commandGroup "Block propagation"
   , op "rebuild-chain" "Rebuild chain"
     (RebuildChain
       <$> many
       (argChainFilterset  "filter"          "JSON list of block/slot selection criteria")
       <*> many argChainFilterExpr)
   , op "read-chain" "Read reconstructed chain"
     (ReadChain
       <$> optJsonInputFile  "chain"         "Block event stream (JSON)")
   , op "dump-chain" "Dump chain"
     (DumpChain
       <$> optJsonOutputFile "chain"         "JSON chain output file"
       <*> optJsonOutputFile "chain-rejecta" "JSON rejected chain output file")
   , op "timeline-chain" "Render chain timeline"
     (TimelineChain
       <$> optTextOutputFile "timeline"      "Render a human-readable reconstructed chain view"
       <*> many parseRTCommentsBP)
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
       (argChainFilterset  "filter"          "JSON list of slot selection criteria")
       <*> many argChainFilterExpr)
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
      <*> parsePropSubset
      <*> parseCDF2Aspect)
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
   , op "read-clusterperfs" "Read some cluster performance analyses as JSON"
     (ReadClusterPerfs
       <$> some
       (optJsonInputFile "clusterperf"   "JSON cluster performance input file"))

   , op "compute-multi-clusterperf" "Consolidate cluster performance stats."
     (ComputeMultiClusterPerf & pure)
   , op "render-multi-clusterperf" "Write multi-run cluster performance results"
     (writerOpts RenderMultiClusterPerf "Render"
      <*> parsePerfSubset
      <*> parseCDF2Aspect)
   ]) <|>

   subparser (mconcat [ commandGroup "Analysis summary"
   , op "compute-summary" "Compute run analysis summary"
     (ComputeSummary & pure)
   , op "render-summary" "Render run analysis summary"
     (writerOpts RenderSummary "Render")
   , op "read-summaries" "Read analysis summaries"
     (ReadSummaries
       <$> some
       (optJsonInputFile     "summary"          "JSON block propagation input file"))
   ]) <|>

   subparser (mconcat [ commandGroup "Run comparison"
   , op "compare" "Generate a report comparing multiple runs"
     (Compare
       <$> optInputDir       "ede"      "Directory with EDE templates."
       <*> optional
           (optTextInputFile "template" "Template to use as base.")
       <*> optTextOutputFile "report"   "Report .org file to create."
       <*> some
       ((,,,)
        <$> optJsonInputFile "run-metafile"    "The meta.json file of a benchmark run"
        <*> optJsonInputFile "shelley-genesis" "Genesis file of the run"
        <*> optJsonInputFile "perf"            "JSON cluster performance input file"
        <*> optJsonInputFile "prop"            "JSON block propagation input file"
       ))
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

   optLOAnyType :: String -> String -> Parser LOAnyType
   optLOAnyType opt desc =
     Opt.option Opt.auto
     ( Opt.long opt
       <> Opt.help desc
       <> Opt.metavar "LOAnyType" )

parseRTCommentsBP :: Parser (RTComments BlockEvents)
parseRTCommentsBP =
  [ Opt.flag' BEErrors     (Opt.long "chain-errors"   <> Opt.help "Show per-block anomalies")
  , Opt.flag' BEFilterOuts (Opt.long "filter-reasons" <> Opt.help "Explain per-block filter-out reasons")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."

parseRenderFormat :: Parser RenderFormat
parseRenderFormat =
  [ Opt.flag' AsJSON    (Opt.long "json"    <> Opt.help "Full JSON dump output file")
  , Opt.flag' AsGnuplot (Opt.long "gnuplot" <> Opt.help "%s-pattern for separate Gnuplot output files, per CDF")
  , Opt.flag' AsOrg     (Opt.long "org"     <> Opt.help "Org mode table output file")
  , Opt.flag' AsReport  (Opt.long "report"  <> Opt.help "Org mode table output file, brief stats")
  , Opt.flag' AsPretty  (Opt.long "pretty"  <> Opt.help "Text report output file")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."

writerOpt :: (RenderFormat -> TextOutputFile -> a) -> String -> RenderFormat -> Parser a
writerOpt ctor desc mode = ctor mode <$> optTextOutputFile opt (desc <> descSuf)
 where
   (,) opt descSuf = optDescSuf mode
   optDescSuf :: RenderFormat -> (String, String)
   optDescSuf = \case
     AsJSON    -> (,) "json"    " results as complete JSON dump"
     AsGnuplot -> (,) "gnuplot" " as individual Gnuplot files"
     AsOrg     -> (,) "org"     " as Org-mode table"
     AsReport  -> (,) "report"  " as Org-mode summary table"
     AsPretty  -> (,) "pretty"  " as text report"

writerOpts :: (RenderFormat -> TextOutputFile -> a) -> String -> Parser a
writerOpts ctor desc = enumFromTo minBound maxBound
                       <&> writerOpt ctor desc
                       & \case
                           (x:xs) -> foldl (<|>) x xs
                           [] -> error "Crazy world."

data State
  = State
  { -- common
    sWhen             :: UTCTime
  , sFilters          :: ([FilterName], [ChainFilter])
  , sTags             :: [Text]
  , sRun              :: Maybe Run
  , sObjLists         :: Maybe [(JsonLogfile, [LogObject])]
  , sDomSlots         :: Maybe (DataDomain SlotNo)
  , sDomBlocks        :: Maybe (DataDomain BlockNo)
    -- propagation
  , sMachViews        :: Maybe [(JsonLogfile, MachView)]
  , sChain            :: Maybe [BlockEvents]
  , sChainRejecta     :: Maybe [BlockEvents]
  , sBlockProp        :: Maybe [BlockPropOne]
  , sMultiBlockProp   :: Maybe MultiBlockProp
    -- performance
  , sSlotsRaw         :: Maybe [(JsonLogfile, [SlotStats NominalDiffTime])]
  , sScalars          :: Maybe [(JsonLogfile, RunScalars)]
  , sSlots            :: Maybe [(JsonLogfile, [SlotStats NominalDiffTime])]
  , sMachPerf         :: Maybe [(JsonLogfile, MachPerfOne)]
  , sClusterPerf      :: Maybe [ClusterPerf]
  , sMultiClusterPerf :: Maybe MultiClusterPerf
    --
  , sSummaries        :: Maybe [Summary]
  }

computeSummary :: State -> Summary
computeSummary =
  \case
    State{sRun           = Nothing} -> err "a run"
    State{sObjLists      = Nothing} -> err "logobjects"
    State{sObjLists      = Just []} -> err "logobjects"
    State{sClusterPerf   = Nothing} -> err "cluster performance results"
    State{sBlockProp     = Nothing} -> err "block propagation results"
    State{sChainRejecta  = Nothing} -> err "chain rejects"
    State{sDomSlots      = Nothing} -> err "a slot domain"
    State{sDomBlocks     = Nothing} -> err "a block domain"
    State{ sObjLists     = Just (fmap snd -> objLists)
         -- , sClusterPerf  = Just clusterPerf
         -- , sBlockProp    = Just blockProp
         , sChainRejecta = Just chainRejecta
         , sDomSlots     = Just sumDomainSlots
         , sDomBlocks    = Just sumDomainBlocks
         , ..} ->
      Summary
      { sumWhen                = sWhen
      , sumFilters             = sFilters
      , sumLogStreams          = countOfList  objLists
      , sumLogObjects          = countOfLists objLists
      , sumBlocksRejected      = countOfList chainRejecta
      , ..
      }
     where
       sumChainRejectionStats =
         chainRejecta
         <&> fmap fst . filter (not . snd) . beAcceptance
          &  concat
          &  foldr' (\k m -> Map.insertWith (+) k 1 m) Map.empty
          &  Map.toList
 where
   err = error . ("Summary of a run requires " <>)

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
  progress "run" (Q $ printf "reading run metadata & Shelley genesis")
  run <- readRun shelleyGenesis runMeta
         & firstExceptT (fromAnalysisError c)
  pure s { sRun = Just run }

runChainCommand s
  c@(Unlog logs mHostDed okDErr okAny) = do
  progress "logs" (Q $ printf "parsing %d log files" $ length logs)
  los <- runLiftLogObjects logs mHostDed okDErr okAny
         & firstExceptT (CommandError c)
  pure s { sObjLists = Just los }

runChainCommand s@State{sObjLists=Just objs}
  c@DumpLogObjects = do
  progress "logobjs" (Q $ printf "dumping %d logobject streams" $ length objs)
  dumpAssociatedObjectStreams "logobjs" objs & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpLogObjects = missingCommandData c
  ["lifted log objects"]

-- runChainCommand s c@(ReadMachViews _ _)    -- () -> [(JsonLogfile, MachView)]

runChainCommand s@State{sRun=Just run, sObjLists=Just objs}
  BuildMachViews = do
  progress "machviews" (Q $ printf "building %d machviews" $ length objs)
  mvs <- buildMachViews run objs & liftIO
  pure s { sMachViews = Just mvs }
runChainCommand _ c@BuildMachViews = missingCommandData c
  ["run metadata & genesis", "lifted logobjects"]

runChainCommand s@State{sMachViews=Just machViews}
  c@DumpMachViews = do
  progress "machviews" (Q $ printf "dumping %d machviews" $ length machViews)
  dumpAssociatedObjects "mach-views" machViews & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpMachViews = missingCommandData c
  ["machine views"]

runChainCommand s c@(ReadMachViews fs) = do
  progress "machviews" (Q $ printf "reading %d machviews" $ length fs)
  machViews <- readAssociatedObjects "mach-views" fs & firstExceptT (CommandError c)
  pure s { sMachViews = Just machViews }

runChainCommand s@State{sRun=Just run, sMachViews=Just mvs}
  c@(RebuildChain fltfs fltExprs) = do
  progress "machviews" (Q $ printf "filtering %d machviews" $ length mvs)
  (fltFiles,
   (<> [ FilterName "inline-expr" | not (null fltExprs)])
    -> fltNames) <- readFilters fltfs
          & firstExceptT (CommandError c)
  let flts = fltFiles <> fltExprs
  (domSlot, domBlock, chainRejecta, chain) <- rebuildChain run flts fltNames mvs
                                               & liftIO
  pure s { sChain = Just chain
         , sChainRejecta = Just chainRejecta
         , sDomSlots = Just domSlot
         , sDomBlocks = Just domBlock
         , sFilters = (fltNames, flts)
         }
  -- pure s { sChain = Just chain }
runChainCommand _ c@RebuildChain{} = missingCommandData c
  ["run metadata & genesis", "reconstructed chain"]

runChainCommand s
  c@(ReadChain f) = do
  progress "chain" (Q $ printf "reading chain")
  chain <- mapM (Aeson.eitherDecode @BlockEvents)
           . filter ((> 5) . LBS.length)
           . LBS.split '\n'
           <$> LBS.readFile (unJsonInputFile f)
           & newExceptT
           & firstExceptT (CommandError c . pack)
  pure s { sChain = Just chain }

runChainCommand s@State{sChain=Just chain, sChainRejecta=Just chainRejecta}
  c@(DumpChain f fRej) = do
  progress "chain" (Q $ printf "dumping chain")
  dumpObjects "chain" chain f & firstExceptT (CommandError c)
  progress "chain-rejecta" (Q $ printf "dumping chain rejecta")
  dumpObjects "chain-rejecta" chainRejecta fRej & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpChain{} = missingCommandData c
  ["chain"]

runChainCommand s@State{sRun=Just run, sChain=Just chain}
  c@(TimelineChain f comments) = do
  progress "chain" (Q $ printf "dumping prettyprinted chain")
  dumpText "chain" (renderTimeline run (const True) comments chain) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@TimelineChain{} = missingCommandData c
  ["run metadata & genesis", "chain"]

runChainCommand s@State{sRun=Just run, sObjLists=Just objs}
  c@(CollectSlots ignores) = do
  let nonIgnored = flip filter objs $ (`notElem` ignores) . fst
  forM_ ignores $
    progress "perf-ignored-log" . R . unJsonLogfile
  progress "slots" (Q $ printf "building slot %d timelines" $ length objs)
  (scalars, slotsRaw) <-
    fmap (mapAndUnzip redistribute) <$> collectSlotStats run nonIgnored
    & newExceptT
    & firstExceptT (CommandError c)
  pure s { sScalars = Just scalars, sSlotsRaw = Just (fmap (fmap (deltifySlotStats (genesis run))) <$> slotsRaw) }
runChainCommand _ c@CollectSlots{} = missingCommandData c
  ["run metadata & genesis", "lifted logobjects"]

runChainCommand s@State{sSlotsRaw=Just slotsRaw}
  c@DumpSlotsRaw = do
  progress "slots" (Q $ printf "dumping %d unfiltered slot timelines" $ length slotsRaw)
  dumpAssociatedObjectStreams "raw-slots" slotsRaw & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpSlotsRaw = missingCommandData c
  ["unfiltered slots"]

runChainCommand s@State{sRun=Just run, sSlotsRaw=Just slotsRaw}
  c@(FilterSlots fltfs fltExprs) = do
  progress "slots" (Q $ printf "filtering %d slot timelines" $ length slotsRaw)
  (fltFiles,
    (<> [ FilterName "inline-expr" | not (null fltExprs)])
    -> fltNames) <- readFilters fltfs & firstExceptT (CommandError c)
  let flts = fltFiles <> fltExprs
  forM_ flts $
    progress "filter" . Q . show
  (domSlots, fltrd) <- runSlotFilters run flts slotsRaw
                       & liftIO
                       & firstExceptT (CommandError c)
  progress "filtered-slotstats-slot-domain" $ J domSlots
  when (maximum (length . snd <$> fltrd) == 0) $
    throwE $ CommandError c $ mconcat
      [ "All ", show $ maximum (length . snd <$> slotsRaw), " slots filtered out." ]
  pure s { sSlots = Just fltrd
         , sDomSlots = Just domSlots
         , sFilters = (fltNames, flts)
         }
runChainCommand _ c@FilterSlots{} = missingCommandData c
  ["run metadata & genesis", "unfiltered slot stats"]

runChainCommand s@State{sSlots=Just slots}
  c@DumpSlots = do
  progress "slots" (Q $ printf "dumping %d slot timelines" $ length slots)
  dumpAssociatedObjectStreams "slots" slots & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpSlots = missingCommandData c
  ["filtered slots"]

runChainCommand s@State{sRun=Just run, sSlots=Just slots}
  c@TimelineSlots = do
  progress "mach" (Q $ printf "dumping %d slot timelines" $ length slots)
  dumpAssociatedTextStreams "mach"
    (fmap (fmap $ renderTimeline run (const True) []) slots)
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@TimelineSlots{} = missingCommandData c
  ["run metadata & genesis", "filtered slots"]

runChainCommand s@State{sRun=Just run, sChain=Just chain, sDomSlots=Just domS, sDomBlocks=Just domB}
  ComputePropagation = do
  progress "block-propagation" $ J (domS, domB)
  prop <- blockProp run chain domS domB & liftIO
  pure s { sBlockProp = Just [prop] }
runChainCommand _ c@ComputePropagation = missingCommandData c
  ["run metadata & genesis", "chain", "data domains for slots & blocks"]

runChainCommand s@State{sBlockProp=Just [prop]}
  c@(RenderPropagation mode f subset) = do
  progress "block-propagation" $ Q "rendering block propagation CDFs"
  forM_ (renderAnalysisCDFs (sRunAnchor s) (propSubsetFn subset) OfOverallDataset Nothing mode prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderPropagation{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{}
  c@(ReadPropagations fs) = do
  progress "block-propagations" (Q $ printf "reading %d block propagations" $ length fs)
  xs <- mapConcurrently readJsonDataIO fs
    & fmap sequence
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sBlockProp = Just xs
         , sTags = pack . takeBaseName . takeDirectory . takeDirectory . unJsonInputFile <$> fs }

runChainCommand s@State{sBlockProp=Just props}
  c@ComputeMultiPropagation = do
  progress "block-propagations" (Q $ printf "computing %d block propagations" $ length props)
  xs <- pure (summariseMultiBlockProp (nEquicentiles $ max 7 (length props)) props)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sMultiBlockProp = Just xs }
runChainCommand _ c@ComputeMultiPropagation{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{sMultiBlockProp=Just prop}
  c@(RenderMultiPropagation mode f subset aspect) = do
  progress "block-propagations" (Q "rendering multi-run block propagation")
  forM_ (renderAnalysisCDFs (sTagsAnchor s) (propSubsetFn subset) aspect Nothing mode prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderMultiPropagation{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{sRun=Just run, sSlots=Just slots}
  c@ComputeMachPerf = do
  progress "machperf" (Q $ printf "computing %d machine performances" $ length slots)
  perf <- mapConcurrentlyPure (slotStatsMachPerf run) slots
          & fmap sequence
          & newExceptT
          & firstExceptT (CommandError c)
  pure s { sMachPerf = Just perf }
runChainCommand _ c@ComputeMachPerf{} = missingCommandData c
  ["run metadata & genesis", "filtered slots"]

runChainCommand s@State{sMachPerf=Just perf}
  c@(RenderMachPerf _mode _subset) = do
  progress "machperf" (Q $ printf "dumping %d machine performance stats" $ length perf)
  dumpAssociatedObjects "perf-stats" perf
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderMachPerf{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sMachPerf=Just machPerfs}
  c@ComputeClusterPerf = do
  progress "clusterperf" (Q $ printf "summarising %d machine performances" $ length machPerfs)
  clusterPerf <- pure (summariseClusterPerf (nEquicentiles $ max 7 (length machPerfs)) (machPerfs <&> snd))
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sClusterPerf = Just [clusterPerf] }
runChainCommand _ c@ComputeClusterPerf{} = missingCommandData c
  ["machine performance stats"]

runChainCommand s@State{sClusterPerf=Just [prop]}
  c@(RenderClusterPerf mode f subset) = do
  progress "clusterperf" (Q $ printf "rendering cluster performance")
  forM_ (renderAnalysisCDFs (sRunAnchor s) (perfSubsetFn subset) OfOverallDataset Nothing mode prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderClusterPerf{} = missingCommandData c
  ["multi-run block propagation"]

runChainCommand s@State{}
  c@(ReadClusterPerfs fs) = do
  progress "clusterperfs" (Q $ printf "reading %d cluster performances" $ length fs)
  xs <- mapConcurrently (fmap (Aeson.eitherDecode @ClusterPerf) . LBS.readFile . unJsonInputFile) fs
    & fmap sequence
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sClusterPerf = Just xs
         , sTags = pack . takeBaseName . takeDirectory . takeDirectory . unJsonInputFile <$> fs }

runChainCommand s@State{sClusterPerf=Just perfs}
  c@ComputeMultiClusterPerf = do
  progress "clusterperfs" (Q $ printf "summarising %d cluster performances" $ length perfs)
  xs <- pure (summariseMultiClusterPerf (nEquicentiles $ max 7 (length perfs)) perfs)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sMultiClusterPerf = Just xs }
runChainCommand _ c@ComputeMultiClusterPerf{} = missingCommandData c
  ["cluster performance stats"]

runChainCommand s@State{sMultiClusterPerf=Just (MultiClusterPerf perf)}
  c@(RenderMultiClusterPerf mode f subset aspect) = do
  progress "clusterperfs" (Q $ printf "rendering multi-run cluster performance")
  forM_ (renderAnalysisCDFs (sTagsAnchor s) (perfSubsetFn subset) aspect Nothing mode perf) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name mode)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderMultiClusterPerf{} = missingCommandData c
  ["multi-run cluster preformance stats"]

runChainCommand s ComputeSummary = do
  progress "summary" (Q "summarising a run")
  pure s { sSummaries = Just [computeSummary s] }

runChainCommand s@State{sSummaries = Just (_summary:_)} c@(RenderSummary fmt f) = do
  progress "summary" (Q $ printf "rendering summary")
  dumpText "summary" body (modeFilename f "" fmt)
    & firstExceptT (CommandError c)
  pure s
 where body = [""] -- renderSummary summary
runChainCommand _ c@RenderSummary{} = missingCommandData c
  ["run summary"]

runChainCommand s@State{}
  c@(ReadSummaries fs) = do
  progress "summaries" (Q $ printf "reading %d run summaries" $ length fs)
  xs <- mapConcurrently (fmap (Aeson.eitherDecode @Summary) . LBS.readFile . unJsonInputFile) fs
    & fmap sequence
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sSummaries = Just xs }

runChainCommand s c@(Compare ede mTmpl outf@(TextOutputFile outfp) runs) = do
  progress "report" (Q $ printf "rendering report for %d runs" $ length runs)
  xs :: [(ClusterPerf, BlockPropOne, Run)] <- forM runs $
    \(mf,gf,cpf,bpf)->
      (,,)
      <$> readJsonData cpf (CommandError c)
      <*> readJsonData bpf (CommandError c)
      <*> (readRun gf mf & firstExceptT (fromAnalysisError c))
  (tmpl, orgReport) <- case xs of
    baseline:deltas@(_:_) -> liftIO $
      Cardano.Report.generate ede mTmpl baseline deltas
    _ -> throwE $ CommandError c $ mconcat
         [ "At least two runs required for comparison." ]
  dumpText "report" [orgReport] outf
    & firstExceptT (CommandError c)

  let tmplPath = Cardano.Analysis.API.replaceExtension outfp "ede"
  liftIO . unlessM (IO.fileExist tmplPath) $
    BS.writeFile tmplPath tmpl

  pure s

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
     State now ([], []) []
           Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing

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
