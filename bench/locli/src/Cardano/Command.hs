{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fmax-pmcheck-models=25000 #-}
module Cardano.Command (module Cardano.Command) where

import Cardano.Prelude          hiding (State)

import Data.Aeson                       qualified as Aeson
import Data.Aeson.Text                  qualified as Aeson
import Data.ByteString                  qualified as BS
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.ByteString.Char8            qualified as BS8
import Data.Map                         qualified as Map
import Data.Text                        (pack)
import Data.Text                        qualified as T
import Data.Text.Lazy                   qualified as LT
import Data.Text.Short                  (toText)
import Data.Time.Clock
import Data.Tuple.Extra                 (both)
import Options.Applicative
import Options.Applicative              qualified as Opt

import System.Directory                 (doesFileExist)
import System.FilePath
import System.Posix.Files               qualified as IO

import Cardano.Analysis.API
import Cardano.Analysis.BlockProp
import Cardano.Analysis.MachPerf
import Cardano.Analysis.Summary
import Cardano.Render
import Cardano.Report
import Cardano.Unlog.LogObject
import Cardano.Util             hiding (head)

data CommandError
  = CommandError    ChainCommand    Text
  deriving Show

renderCommandError :: CommandError -> Text
renderCommandError (CommandError cmd err) =
  "ERROR: While executing locli operation:\n\n    '" <> show cmd <> "'\n\n  " <> err

-- | Sub-commands
newtype Command
  -- | Analysis commands
  = ChainCommand [ChainCommand]
  deriving Show

data ChainCommand
  = ListLogobjectKeys       TextOutputFile
  | ListLogobjectKeysLegacy TextOutputFile

  |        ReadMetaGenesis  (JsonInputFile  RunPartial) (JsonInputFile  Genesis)
  |        WriteMetaGenesis TextOutputFile              TextOutputFile

  |        Unlog            (JsonInputFile (RunLogs ())) Bool (Maybe [LOAnyType])
  |        DumpLogObjects

  |    ValidateHashTimeline (JsonInputFile [LogObject])

  |        BuildMachViews
  |         DumpMachViews
  |         ReadMachViews   [JsonLogfile]

  |         RebuildChain    [JsonFilterFile] [ChainFilter]
  |            DumpChain    (JsonOutputFile [BlockEvents]) (JsonOutputFile [BlockEvents])
  |            ReadChain    (JsonInputFile [BlockEvents])
  |        TimelineChain    RenderConfig TextOutputFile [TimelineComments BlockEvents]

  |         CollectSlots    [JsonLogfile]
  |            DumpSlotsRaw
  |          FilterSlots    [JsonFilterFile] [ChainFilter]
  |            DumpSlots
  |        TimelineSlots    RenderConfig [TimelineComments (SlotStats NominalDiffTime)]

  |      ComputePropagation
  |       RenderPropagation RenderConfig TextOutputFile PropSubset
  |        ReadPropagations [JsonInputFile BlockPropOne]
  | ComputeMultiPropagation
  |  RenderMultiPropagation RenderConfig TextOutputFile PropSubset CDF2Aspect

  |         ComputeMachPerf
  |          RenderMachPerf RenderConfig PerfSubset

  |      ComputeClusterPerf
  |       RenderClusterPerf RenderConfig TextOutputFile PerfSubset
  |        ReadClusterPerfs [JsonInputFile MultiClusterPerf]
  | ComputeMultiClusterPerf
  |  RenderMultiClusterPerf RenderConfig TextOutputFile PerfSubset CDF2Aspect

  |      ComputeSummary
  |       RenderSummary     RenderConfig TextOutputFile
  |         ReadSummaries   [JsonInputFile SummaryOne]
  | ComputeMultiSummary
  |  RenderMultiSummary     RenderConfig TextOutputFile

  |             Compare     InputDir (Maybe TextInputFile) TextOutputFile
                            [( JsonInputFile SomeSummary
                             , JsonInputFile ClusterPerf
                             , JsonInputFile SomeBlockProp)]

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
   , op "read-meta-genesis" "Read the run metadata:  meta.json and Shelley genesis"
     (ReadMetaGenesis
       <$> optJsonInputFile "run-metafile"    "The meta.json file from the benchmark run"
       <*> optJsonInputFile "shelley-genesis" "Genesis file of the run")

   , op "write-meta-genesis" "Write the run metadata:  meta.json and Shelley genesis"
     (WriteMetaGenesis
       <$> optTextOutputFile "run-metafile"    "The meta.json file from the benchmark run"
       <*> optTextOutputFile "shelley-genesis" "Genesis file of the run")
   ]) <|>

  subparser (mconcat [ commandGroup "Basic log objects"
   , op "unlog" "Read log files"
     (Unlog
       <$> optJsonInputFile    "run-logs"       "Run log manifest (API/Types.hs:RunLogs)"
       <*> Opt.flag False True (Opt.long "lodecodeerror-ok"
                                 <> Opt.help "Allow non-EOF LODecodeError logobjects")
       <*> optional
            (some
             (optLOAnyType      "ok-loany"        "[MULTI] Allow a particular LOAnyType"))
     )
   , op "dump-logobjects" "Dump lifted log object streams, alongside input files"
     (DumpLogObjects & pure)
   , op "hash-timeline" "Quickly validate timeline by hashes"
     (ValidateHashTimeline
        <$> optJsonInputFile  "timeline"     "Hash timeline (JSON from prepare step)")
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
       <$> parseFixedRenderConfig AsPretty
       <*> optTextOutputFile "timeline"      "Render a human-readable reconstructed chain view"
       <*> many parseTimelineCommentsBP)
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
     (TimelineSlots
       <$> parseFixedRenderConfig AsPretty
       <*> many parseTimelineCommentsSS)
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
     (pure $ RenderMachPerf (mkRenderConfigDef AsPretty) PerfFull)
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

   , op "compute-multi-summary" "Compute a multi-run summary"
     (ComputeMultiSummary & pure)
   , op "render-multi-summary" "Write out multi-run summary results"
     (writerOpts RenderMultiSummary "Render")
   ]) <|>

   subparser (mconcat [ commandGroup "Run comparison"
   , op "compare" "Generate a report comparing multiple runs"
     (Compare
       <$> optInputDir       "ede"      "Directory with EDE templates."
       <*> optional
           (optTextInputFile "template" "Template to use as base.")
       <*> optTextOutputFile "report"   "Report .org file to create."
       <*> some
       ((,,)
        <$> optJsonInputFile "summary" "JSON analysis summary input file"
        <*> optJsonInputFile "perf"    "JSON cluster performance input file"
        <*> optJsonInputFile "prop"    "JSON block propagation input file"
       ))
   ])
 where
   op :: String -> String -> Parser a -> Mod CommandFields a
   op c descr p =
     command c $ info (p <**> helper) $
       mconcat [ progDesc descr ]

   optLOAnyType :: String -> String -> Parser LOAnyType
   optLOAnyType opt desc =
     Opt.option Opt.auto
     ( Opt.long opt
       <> Opt.help desc
       <> Opt.metavar "LOAnyType" )

parseTimelineCommentsBP :: Parser (TimelineComments BlockEvents)
parseTimelineCommentsBP =
  [ Opt.flag' BEErrors     (Opt.long "with-chain-errors"   <> Opt.help "Show per-block anomalies")
  , Opt.flag' BEFilterOuts (Opt.long "with-filter-reasons" <> Opt.help "Explain per-block filter-out reasons")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."

parseTimelineCommentsSS :: Parser (TimelineComments (SlotStats NominalDiffTime))
parseTimelineCommentsSS =
  [ Opt.flag' SSLogObjects (Opt.long "with-logobjects"     <> Opt.help "Show per-slot logobjects")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."

parseWithoutDateVerMeta, parseWithoutRunMeta :: Parser Bool
parseWithoutDateVerMeta =
  Opt.flag True False (Opt.long "without-datever-meta" <>
                       Opt.help "Omit date/version from report rendering.")
parseWithoutRunMeta =
  Opt.flag True False (Opt.long "without-run-meta" <>
                       Opt.help "Omit run metadata from report rendering.")

parseFixedRenderConfig :: RenderFormat -> Parser RenderConfig
parseFixedRenderConfig cf =
  RenderConfig cf
    <$> parseWithoutDateVerMeta
    <*> parseWithoutRunMeta

writerOpt :: (RenderConfig -> TextOutputFile -> a) -> String -> RenderFormat -> Parser a
writerOpt ctor desc rcFormat = do
  (\rcDateVerMetadata rcRunMetadata -> ctor RenderConfig{..})
    <$> parseWithoutDateVerMeta
    <*> parseWithoutRunMeta
    <*> optTextOutputFile opt (desc <> descSuf)
 where
   (,) opt descSuf = optDescSuf rcFormat
   optDescSuf :: RenderFormat -> (String, String)
   optDescSuf = \case
     AsJSON    -> (,) "json"    " results as complete JSON dump"
     AsGnuplot -> (,) "gnuplot" " as individual Gnuplot files"
     AsOrg     -> (,) "org"     " as Org-mode table"
     AsReport  -> (,) "org-report"  " as Org-mode summary table"
     AsPretty  -> (,) "pretty"  " as text report"

writerOpts :: (RenderConfig -> TextOutputFile -> a) -> String -> Parser a
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
  , sRunLogs          :: Maybe (RunLogs [LogObject])
  , sDomSlots         :: Maybe (DataDomain I SlotNo)
    -- propagation
  , sMachViews        :: Maybe [(JsonLogfile, MachView)]
  , sChain            :: Maybe Chain
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
  , sSummaries        :: Maybe [SummaryOne]
  , sMultiSummary     :: Maybe MultiSummary
  }

callComputeSummary :: State -> Either Text SummaryOne
callComputeSummary =
  \case
    State{sRun           = Nothing} -> err "a run"
    State{sRunLogs       = Nothing} -> err "logobjects"
    State{sClusterPerf   = Nothing} -> err "cluster performance results"
    State{sBlockProp     = Nothing} -> err "block propagation results"
    State{sChain         = Nothing} -> err "chain"
    State{ sRunLogs      = Just runLogs
         , sClusterPerf  = Just [clusterPerf]
         , sBlockProp    = Just [blockProp']
         , sChain        = Just chain
         , sRun          = Just Run{..}
         , ..} -> Right $
      computeSummary sWhen metadata genesis genesisSpec generatorProfile
                     runLogs sFilters
                     clusterPerf blockProp' chain
    _ -> err "Impossible to get here."
 where
   err = Left . ("Summary of a run requires " <>)

stateAnchor :: [Text] -> State -> Anchor
stateAnchor tags State{sFilters, sWhen, sClusterPerf, sChain} =
  tagsAnchor tags sWhen sFilters
             ((sClusterPerf <&> fmap (head . mpDomainSlots) . head & join.join) <|>
              (sChain       <&> cDomSlots))
             (sChain        <&> cDomBlocks)

sAnchor :: HasCallStack => State -> Anchor
sAnchor s@State{sMultiSummary=Just summary}
  = stateAnchor [tag . sumMeta $ summary] s
sAnchor s@State{sRun = Just run}
  = stateAnchor [tag . metadata $ run] s
sAnchor State{sTags=[]} = error "sAnchor with no run or multi-summary."
sAnchor s@State{sTags}
  = stateAnchor sTags s

quote :: T.Text -> T.Text
quote = (<> "\"") . ("\"" <>)

runChainCommand :: State -> ChainCommand -> ExceptT CommandError IO State

runChainCommand s
  c@(ListLogobjectKeys f) = do
  dumpText "logobject-keys" (quote . toText <$> logObjectStreamInterpreterKeys) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand s
  c@(ListLogobjectKeysLegacy f) = do
  dumpText "logobject-keys-legacy" (quote . toText <$> logObjectStreamInterpreterKeysLegacy) f
    & firstExceptT (CommandError c)
  pure s

runChainCommand s
  c@(ReadMetaGenesis runMeta shelleyGenesis) = do
  progress "run" (Q $ printf "reading run metadata & Shelley genesis")
  run <- readRun shelleyGenesis runMeta
         & firstExceptT (fromAnalysisError c)
  progress "run" (Q $ printf "identifier: %s" (ident $ metadata run))
  pure s { sRun = Just run }

runChainCommand s@State{sMultiSummary=Just summ@Summary{..}}
  c@(WriteMetaGenesis runMeta shelleyGenesis) = do
  dumpText "meta"    [LT.toStrict $ Aeson.encodeToLazyText (summaryMetaJson summ)] runMeta
    & firstExceptT (CommandError c)
  dumpText "genesis" [LT.toStrict $ Aeson.encodeToLazyText sumGenesis] shelleyGenesis
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@WriteMetaGenesis{} = missingCommandData c
  ["multi objects"]

runChainCommand s
  c@(Unlog rlf okDErr loAnyLimit) = do
  progress "logs" (Q $ printf "reading run log manifest %s" $ unJsonInputFile rlf)
  runLogsBare <- Aeson.eitherDecode @(RunLogs ())
                 <$> LBS.readFile (unJsonInputFile rlf)
                 & newExceptT
                 & firstExceptT (CommandError c . pack)
  progress "logs" (Q $ printf "parsing logs for %d hosts" $
                   Map.size $ rlHostLogs runLogsBare)
  progress "logs" (Q $ printf "LOAny constraint:  %s" (show loAnyLimit :: String))
  runLogs <- runLiftLogObjects runLogsBare okDErr loAnyLimit
             & firstExceptT (CommandError c)
  pure s { sRunLogs = Just runLogs }

runChainCommand s@State{sRunLogs=Just (rlLogs -> objs)}
  c@DumpLogObjects = do
  progress "logobjs" (Q $ printf "dumping %d logobject streams" $ length objs)
  dumpAssociatedObjectStreams "logobjs" objs & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpLogObjects = missingCommandData c
  ["lifted log objects"]

-- runChainCommand s c@(ReadMachViews _ _)    -- () -> [(JsonLogfile, MachView)]

runChainCommand s
  c@(ValidateHashTimeline timelineJson) = do
  progress "logs" (Q $ printf "validating hash timeline")
  let f = unJsonInputFile timelineJson
  hashTimeline <- liftIO $
    doesFileExist f >>= bool
      (return [])
      (readLogObjectStream f False Nothing)
  case (hashTimeline, checkAllForgersKnown hashTimeline) of
    ([], _)           -> progress "logs" (Q $ printf "%s not found - skipping" f)
    (_, Nothing)      -> progress "logs" (Q $ printf "all forgers known")
    (_, Just (x, h))  -> throwE $ CommandError c $
                          "unknown forger for block hash " <> (toText . unHash) h
                          <> " in:\n" <> LT.toStrict (Aeson.encodeToLazyText x)
  pure s

runChainCommand s@State{sRun=Just run, sRunLogs=Just (rlLogs -> objs)}
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
  forM_ flts $
    progress "filter" . Q . show

  let chain = rebuildChain run flts fltNames mvs
  progress "tip" $ Q . show . beBlock . last $ cMainChain chain

  pure s { sChain = Just chain
         , sFilters = (fltNames, flts)
         }
  -- pure s { sChain = Just chain }
runChainCommand _ c@RebuildChain{} = missingCommandData c
  ["run metadata & genesis", "reconstructed chain"]

runChainCommand _ ReadChain{} = do
    pure $ error "ReadChain not implemented"
  -- progress "chain" (Q $ printf "reading chain")
  -- chain <- mapM (Aeson.eitherDecode @BlockEvents)
  --          . filter ((> 5) . LBS.length)
  --          . LBS.split '\n'
  --          <$> LBS.readFile (unJsonInputFile f)
  --          & newExceptT
  --          & firstExceptT (CommandError c . pack)
  -- pure s { sChain = Just chain }

runChainCommand s@State{sChain=Just Chain{..}}
  c@(DumpChain f fRej) = do
  progress "chain" (Q $ printf "dumping chain")
  dumpObjects "chain" cMainChain f & firstExceptT (CommandError c)
  progress "chain-rejecta" (Q $ printf "dumping chain rejecta")
  dumpObjects "chain-rejecta" cRejecta fRej & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@DumpChain{} = missingCommandData c
  ["chain"]

runChainCommand s@State{sRun=Just _run, sChain=Just Chain{..}}
  c@(TimelineChain rc f comments) = do
  progress "chain" (Q $ printf "dumping prettyprinted chain: %s" (show rc :: String))
  dumpText "chain" (renderTimelineWithClass (const True) rc (sAnchor s) comments cMainChain) f
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@TimelineChain{} = missingCommandData c
  ["run metadata & genesis", "chain"]

runChainCommand s@State{sRun=Just run, sRunLogs=Just (rlLogs -> objs)}
  c@(CollectSlots ignores) = do
  let nonIgnored = flip filter objs $ (`notElem` ignores) . fst
  forM_ ignores $
    progress "perf-ignored-log" . R . unJsonLogfile
  progress "slots" (Q $ printf "building slot %d timelines" $ length objs)
  (scalars, slotsRaw) <-
    fmap (mapAndUnzip redistribute) <$> collectSlotStats run nonIgnored
    & newExceptT
    & firstExceptT (CommandError c)
  pure s { sScalars  = Just scalars
         , sSlotsRaw = Just (fmap (fmap (deltifySlotStats (genesis run))) <$> slotsRaw) }
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

runChainCommand s@State{sRun=Just _run, sSlots=Just slots}
  c@(TimelineSlots rc comments) = do
  progress "mach" (Q $ printf "dumping %d slot timelines: %s" (length slots) (show rc :: String))
  dumpAssociatedTextStreams "mach"
    (fmap (fmap $ renderTimelineWithClass (const True) rc (sAnchor s) comments) slots)
    & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@TimelineSlots{} = missingCommandData c
  ["run metadata & genesis", "filtered slots"]

runChainCommand s@State{sRun=Just run, sChain=Just chain@Chain{..}, sRunLogs}
  c@ComputePropagation = do
  progress "block-propagation" $ J (cDomBlocks, cDomSlots)
  prop <- pure (blockProp run chain)
          & newExceptT
          & firstExceptT liftBlockPropError
  pure s { sBlockProp = Just [prop] }
 where
   liftBlockPropError :: BlockPropError -> CommandError
   liftBlockPropError = \case
     e@BPEEntireChainFilteredOut{} -> CommandError c $
       renderBlockPropError e
       <> maybe "" ((".\n\n  Missing traces in run logs (not all are fatal):\n\n    " <>)
                    . T.intercalate "\n    "
                    . fmap toText
                    . rlMissingTraces)
                sRunLogs
runChainCommand _ c@ComputePropagation = missingCommandData c
  ["run metadata & genesis", "chain", "data domains for slots & blocks"]

runChainCommand s@State{sBlockProp=Just [prop]}
  c@(RenderPropagation rc@RenderConfig{..} f subset) = do
  progress "block-propagation" $ Q "rendering block propagation CDFs"
  forM_ (renderAnalysisCDFs
          (sAnchor s) (propSubsetFn subset) OfOverallDataset Nothing rc prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name rcFormat)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderPropagation{} = missingCommandData c
  ["block propagation"]

runChainCommand s@State{}
  c@(ReadPropagations fs) = do
  progress "block-propagations" (Q $ printf "reading %d block propagations" $ length fs)
  xs :: [BlockProp I] <- mapConcurrently readJsonDataIO fs
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
  c@(RenderMultiPropagation rc@RenderConfig{..} f subset aspect) = do
  progress "block-propagations" (Q "rendering multi-run block propagation")
  forM_ (renderAnalysisCDFs
          (sAnchor s) (propSubsetFn subset) aspect Nothing rc prop) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name rcFormat)
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

runChainCommand s@State{sClusterPerf=Just [perf]}
  c@(RenderClusterPerf rc@RenderConfig{..} f subset) = do
  progress "clusterperf" (Q $ printf "rendering cluster performance")
  forM_ (renderAnalysisCDFs (sAnchor s) (perfSubsetFn subset) OfOverallDataset Nothing rc perf) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name rcFormat)
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
  c@(RenderMultiClusterPerf rc@RenderConfig{..} f subset aspect) = do
  progress "clusterperfs" (Q $ printf "rendering multi-run cluster performance")
  forM_ (renderAnalysisCDFs (sAnchor s) (perfSubsetFn subset) aspect Nothing rc perf) $
    \(name, body) ->
      dumpText (T.unpack name) body (modeFilename f name rcFormat)
      & firstExceptT (CommandError c)
  pure s
runChainCommand _ c@RenderMultiClusterPerf{} = missingCommandData c
  ["multi-run cluster preformance stats"]

runChainCommand s c@ComputeSummary = do
  progress "summary" (Q "summarising a run")
  summary <- pure (callComputeSummary s)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sSummaries = Just [summary] }

runChainCommand s@State{sSummaries = Just (summary:_)} c@(RenderSummary rc@RenderConfig{..} f) = do
  progress "summary" (Q "rendering summary")
  dumpText "summary" bodySummary (modeFilename f "" rcFormat)
    & firstExceptT (CommandError c)
  forM_ (sumProfilingData summary) $
    \profData -> do
      let bodyProfiling =
            renderProfilingData rc anchor
              (uncurry (||)
               . both ((>= 1.0) . unI . cdfAverage)
               . (peTime &&& peAlloc))
              profData
      dumpText "profiling" bodyProfiling (TextOutputFile $ replaceFileName (unTextOutputFile f) "profiling" System.FilePath.<.> "org")
        & firstExceptT (CommandError c)
  pure s
 where bodySummary = renderSummary rc anchor (iFields sumFieldsReport) summary
       anchor = sAnchor s
runChainCommand _ c@RenderSummary{} = missingCommandData c
  ["run summary"]

runChainCommand s@State{}
  c@(ReadSummaries fs) = do
  progress "summaries" (Q $ printf "reading %d run summaries" $ length fs)
  xs <- mapConcurrently (fmap Aeson.eitherDecode . LBS.readFile . unJsonInputFile) fs
    & fmap sequence
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sSummaries = Just xs }

runChainCommand s@State{sWhen, sSummaries=Just xs}
  c@ComputeMultiSummary = do
  progress "multi-summary" (Q $ printf "computing multi-summary of %d summaries" $ length xs)
  r <- pure (summariseMultiSummary sWhen (nEquicentiles $ max 7 (length xs)) xs)
    & newExceptT
    & firstExceptT (CommandError c . show)
  pure s { sMultiSummary = Just r }
runChainCommand _ c@ComputeMultiSummary{} = missingCommandData c
  ["multi-run summary"]

runChainCommand s@State{sMultiSummary=Just summary}
  c@(RenderMultiSummary rc@RenderConfig{..} f) = do
  progress "multi-summary" (Q "rendering multi-run summary")
  dumpText "multi-summary" body (modeFilename f "" rcFormat)
    & firstExceptT (CommandError c)
  forM_ (sumProfilingData summary) $
    \profData -> do
      let bodyProfiling =
            renderProfilingData rc anchor
              (uncurry (||)
               . both ((>= 1.0) . unI . cdfAverage)
               . (peTime &&& peAlloc))
              profData
      dumpText "multi-profiling" bodyProfiling (TextOutputFile $ replaceFileName (unTextOutputFile f) "profiling" System.FilePath.<.> "org")
        & firstExceptT (CommandError c)
  pure s
 where body = renderSummary rc anchor (iFields sumFieldsReport) summary
       anchor = sAnchor s
runChainCommand _ c@RenderMultiSummary{} = missingCommandData c
  ["multi-run summary"]

runChainCommand s c@(Compare ede mTmpl outf@(TextOutputFile outfp) runs) = do
  progress "report" (Q $ printf "rendering report for %d runs" $ length runs)
  xs :: [(SomeSummary, ClusterPerf, SomeBlockProp)] <- forM runs $
    \(sumf,cpf,bpf)->
      (,,)
      <$> readJsonData sumf (CommandError c)
      <*> readJsonData cpf  (CommandError c)
      <*> readJsonData bpf  (CommandError c)
  (tmpl, tmplEnv, orgReport) <- case xs of
    baseline:deltas@(_:_) -> liftIO $ do
      Cardano.Report.generate ede mTmpl baseline deltas
    _ -> throwE $ CommandError c $ mconcat
         [ "At least two runs required for comparison." ]
  liftIO $
    withFile (outfp `System.FilePath.replaceExtension` "env.json") WriteMode $
      \hnd -> BS8.hPutStrLn hnd tmplEnv
  dumpText "report" [orgReport] outf
    & firstExceptT (CommandError c)

  let tmplPath = Cardano.Util.replaceExtension outfp "ede"
  liftIO . unlessM (IO.fileExist tmplPath) $
    BS.writeFile tmplPath tmpl

  pure s

missingCommandData :: ChainCommand -> [String] -> ExceptT CommandError IO a
missingCommandData c xs =
  throwError $ CommandError c (pack $ printf "Some of the following data was missing:  %s" $ intercalate ", " xs)

_reportBanner :: [FilterName] -> FilePath -> Text
_reportBanner fnames inputfp = mconcat
  [ "input: ", logfileRunIdentifier (pack inputfp), " "
  , "locli: ", gitRev getLocliVersion, " "
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
