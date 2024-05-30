{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Cardano.Report
  ( module Cardano.Report
  )
where

import           Cardano.Analysis.API
import           Cardano.Analysis.Summary
import           Cardano.Prelude hiding (head)
import           Cardano.Render
import           Cardano.Table
import           Cardano.Util

import qualified Data.Aeson.Encode.Pretty as AEP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Tuple.Extra (fst3, snd3, thd3)
import           System.Environment (lookupEnv)
import           System.Posix.User
import           Text.EDE hiding (Id)


newtype Author   = Author   { unAuthor   :: Text } deriving newtype (FromJSON, ToJSON)
newtype ShortId  = ShortId  { unShortId  :: Text } deriving newtype (FromJSON, ToJSON)
newtype Tag      = Tag      { unTag      :: Text } deriving newtype (FromJSON, ToJSON)

data ReportMeta
  = ReportMeta
    { rmAuthor       :: ![Author]
    , rmDate         :: !Text
    , rmLocliVersion :: !LocliVersion
    , rmTarget       :: !Version
    , rmTag          :: !Tag
    }
instance ToJSON ReportMeta where
  toJSON ReportMeta{..} = object
    [ "author"     .= rmAuthor
    , "date"       .= rmDate
    , "locli"      .= rmLocliVersion
    , "target"     .= rmTarget
    , "tag"        .= rmTag
    ]

-- Multiple authors would be good to detect. For the moment,
-- it'll just always generate a singleton list.
getReport :: [Metadata] -> Version -> IO ReportMeta
getReport metas _ver = do
  rmAuthor <- (:[]) <$>
    getGecosFullUsername
              `catch`
              \(_ :: SomeException) ->
                 getFallbackUserId
  rmDate <- getCurrentTime <&> T.take 16 . show
  let rmLocliVersion = getLocliVersion
      rmTarget = Version $ ident $ last metas
      rmTag = Tag $ multiRunTag Nothing metas
  pure ReportMeta{..}
 where
   getGecosFullUsername, getFallbackUserId :: IO Author
   getGecosFullUsername =
     (getUserEntryForID =<< getRealUserID)
     <&> Author . T.pack . takeWhile (/= ',') . userGecos

   getFallbackUserId =
     (\user host->
        Author . T.pack $
        fromMaybe "user" user <> "@" <> fromMaybe "localhost" host)
     <$> lookupEnv "USER"
     <*> lookupEnv "HOSTNAME"

data Workload
  = WValue
  | WPlutusLoopCountdown
  | WPlutusLoopSECP
  | WPlutusLoopBLST
  | WPlutusUnknown

instance ToJSON Workload where
  toJSON = \case
    WValue               -> "value-only"
    WPlutusLoopCountdown -> "Plutus countdown loop"
    WPlutusLoopSECP      -> "Plutus SECP loop"
    WPlutusLoopBLST      -> "Plutus BLST loop"
    WPlutusUnknown       -> "Plutus (other)"

filenameInfix :: Workload -> Text
filenameInfix = \case
  WPlutusLoopCountdown  -> "plutus"
  WPlutusLoopSECP       -> "plutus-secp"
  WPlutusLoopBLST       -> "plutus-blst"
  WValue                -> "value-only"
  _                     -> "unknown"

data Section where
  STable ::
    { sData      :: !(a p)
    , sFields    :: !FSelect
    , sNameCol   :: !Text
    , sValueCol  :: !Text
    , sDataRef   :: !Text
    , sOrgFile   :: !Text
    , sTitle     :: !Text
    } -> Section

formatSuffix :: RenderFormat -> Text
formatSuffix AsOrg = "org"
formatSuffix AsLaTeX = "latex"
formatSuffix _ = "txt"

summaryReportSection :: RenderFormat -> Summary f -> Section
summaryReportSection rf summ =
  STable summ (ISel @SummaryOne $ iFields sumFieldsReport) "Parameter" "Value"   "summary"
    ("summary." <> formatSuffix rf)
    "Overall run parameters"

analysesReportSections :: RenderFormat -> MachPerf (CDF I) -> BlockProp f -> [Section]
analysesReportSections rf mp bp =
  [ STable mp (DSel @MachPerf  $ dFields mtFieldsReport)   "metric"  "average"    "perf" ("clusterperf.report." <> ext)
    "Resource Usage"

  , STable bp (DSel @BlockProp $ dFields bpFieldsControl)  "metric"  "average" "control" ("blockprop.control." <> ext)
    "Anomaly control"

  , STable bp (DSel @BlockProp $ dFields bpFieldsForger)   "metric"  "average"   "forge" ("blockprop.forger." <> ext)
    "Forging"

  , STable bp (DSel @BlockProp $ dFields bpFieldsPeers)    "metric"  "average"   "peers" ("blockprop.peers." <> ext)
    "Individual peer propagation"

  , STable bp (DSel @BlockProp $ dFields bpFieldsEndToEnd) "metric"  "average" "end2end" ("blockprop.endtoend." <> ext)
    "End-to-end propagation"
  ]
  where
    ext = formatSuffix rf

--
-- Representation of a run, structured for template generator's needs.
--

liftTmplRun :: Summary a -> TmplRun
liftTmplRun Summary{sumWorkload=generatorProfile
                   ,sumMeta=meta@Metadata{..}} =
  TmplRun
  { trMeta      = meta
  , trManifest  = manifest & unsafeShortenManifest 5
  , trWorkload  =
    case plutusLoopScript generatorProfile of
      Nothing                               -> WValue
      Just script
        | "Loop" `T.isPrefixOf` script      -> WPlutusLoopCountdown
        | script == "EcdsaSecp256k1Loop"    -> WPlutusLoopSECP
        | script == "SchnorrSecp256k1Loop"  -> WPlutusLoopSECP
        | script == "HashOntoG2AndAdd"      -> WPlutusLoopBLST
        | otherwise                         -> WPlutusUnknown
  }

data TmplRun
  = TmplRun
    { trMeta         :: !Metadata
    , trWorkload     :: !Workload
    , trManifest     :: !Manifest
    }

instance ToJSON TmplRun where
  toJSON TmplRun{..} =
    object
      [ "meta"       .= trMeta
      , "workload"   .= trWorkload
      , "branch"     .= componentBranch (getComponent "cardano-node" trManifest)
      , "ver"        .= ident trMeta
      , "rev"        .= unManifest trManifest
      , "fileInfix"  .= filenameInfix trWorkload
      ]

liftTmplSection :: Section -> TmplSection
liftTmplSection =
  \case
    STable{..} ->
      TmplTable
      { tsTitle       = sTitle
      , tsNameCol     = sNameCol
      , tsValueCol    = sValueCol
      , tsDataRef     = sDataRef
      , tsOrgFile     = sOrgFile
      , tsRowPrecs    = fs <&> fromEnum
      , tsVars        = [ ("nSamples", "Sample count")
                        ]
      }
     where fs = case sFields of
                  ISel sel -> filter sel timelineFields <&> fPrecision
                  DSel sel -> filter sel      cdfFields <&> fPrecision

data TmplSection
  = TmplTable
    { tsTitle        :: !Text
    , tsNameCol      :: !Text
    , tsValueCol     :: !Text
    , tsDataRef      :: !Text
    , tsOrgFile      :: !Text
    , tsRowPrecs     :: ![Int]
    , tsVars         :: ![(Text, Text)] -- map from Org constant name to description
    }

instance ToJSON TmplSection where
  toJSON TmplTable{..} = object
    [ "title"     .= tsTitle
    , "nameCol"   .= tsNameCol
    , "valueCol"  .= tsValueCol
    , "dataRef"   .= tsDataRef
    , "orgFile"   .= tsOrgFile
    -- Yes, strange as it is, this is the encoding to ease iteration in ED-E.
    , "rowPrecs"  .= tsRowPrecs
    , "vars"      .= Map.fromList (zip tsVars ([0..] <&> flip T.replicate ">" . (length tsVars -))
                                   <&> \((k, name), angles) ->
                                         (k, Map.fromList @Text
                                             [("name", name),
                                              ("angles", angles)]))
    ]

generateLaTeX :: (SomeSummary, ClusterPerf, SomeBlockProp)
          -> [(SomeSummary, ClusterPerf, SomeBlockProp)]
          -- The result tuple is: summary, resource, anomaly, forging, peers
          -> IO (Text, Text, Text, Text, Text, Text)
generateLaTeX (SomeSummary (summ :: Summary f), cp :: MachPerf cpt, SomeBlockProp (bp :: BlockProp bpt)) rest = do
  ctx <- getReport metas . ciVersion . getComponent "cardano-node"
                                     . trManifest $ last restTmpls
  time <- getCurrentTime
  let anchor :: Anchor
      anchor =
          Anchor {
              aRuns = renderSummaryName summ :
                        [renderSummaryName s | (SomeSummary s, _, _) <- rest]
            , aFilters = ([], [])
            , aSlots = Nothing
            , aBlocks = Nothing
            , aVersion = getLocliVersion
            , aWhen = time
          }
      -- FIXME: Plutus workloads need to be able to be distinguished from
      --        Value workloads.
      isPlutus :: Bool
      isPlutus = False
      workloadName :: Text
      workloadName   | isPlutus  = "Plutus Workload"
                     | otherwise = "Value Workload"

      reportTypeName :: Text
      reportTypeName | null rest = "Benchmark"
                     | otherwise = "Comparison"

      -- titleName generates the title for the document as a whole.
      -- It tries to respond like reportTypeName to the nullity or
      -- otherwise of the list of secondary runs. Some of that involves
      -- phrasing, so there is some strangeness with the conjunction only
      -- being used before the final list element, while the earlier
      -- list elements are separated by commas.
      titleName :: Text
      titleName = case aRuns anchor of
        [] -> error "titleName in generateLaTeX: no runs to compare"
        [single]        ->
          reportTypeName <> " of " <> single <> " in " <> workloadName
        base : comp@(_:_) -> workloadName <> " " <> reportTypeName <> " of "
          <> base <> " against "
          <> case L.reverse comp of
               [] -> error "titleName in generateLaTeX: no secondary runs"
               [onlyComp] -> onlyComp
               lastComp : restComp@(_:_) -> T.concat . L.reverse
                 $ ("and " <> lastComp) : map (<> ", ") restComp

      titlingText :: Text
      titlingText = unlines
        . map latexFixup
        $ [ "\\def\\locliauthor{" <> T.intercalate " \\\\and " authorList <> "}"
          , "\\def\\loclititle{" <> titleName <> "}"
          , "\\def\\loclidate{" <> rmDate ctx <> "}"
          ] where
        authorList = map unAuthor $ rmAuthor ctx
      mkTable :: [Int] -> [Field select con functor] -> [[Double]] -> Table
      mkTable sizes fields rows
        = Table {
             tColHeaders
                 = let h : t = aRuns anchor
                    -- hex escape sequence for majuscule Greek delta
                    in h : concatMap (\run -> [run, "\x394", "\x394%"]) t
          ,  tColumns        = map (map $ formatDouble W6) rows
          ,  tExtended       = False
          ,  tApexHeader     = Nothing
          ,  tRowHeaders     = map fShortDesc fields
          ,  tSummaryHeaders = ["Sample count"]
          ,  tSummaryValues  = [[formatInt W4 sz] | sz <- sizes]
          ,  tFormula        = []
          ,  tConstants      = []
          }

  pure    ( titlingText
          , unlines . renderSummaryList renderConfig anchor
                           (iFields sumFieldsReport :: Field ISelect I a' -> Bool) summ
                         $ map fst3 rest
          , unlines . renderAsLaTeX $ mkTable (map head resourceSamples) resourceFields resourceRows
          , unlines . renderAsLaTeX $ mkTable (map head anomalySamples)  anomalyFields  anomalyRows
          , unlines . renderAsLaTeX $ mkTable (map head forgingSamples)  forgingFields  forgingRows
          , unlines . renderAsLaTeX $ mkTable (map head peersSamples)    peersFields    peersRows
          )
  where
   metas :: [Metadata]
   metas = sumMeta summ : fmap (\(SomeSummary ss, _, _) -> sumMeta ss) rest
   restTmpls = fmap ((\(SomeSummary ss) -> liftTmplRun ss) . fst3) rest
   renderConfig =
     RenderConfig {
       rcFormat = AsLaTeX
     , rcDateVerMetadata = False
     , rcRunMetadata = False
     }
   anomalyFields, forgingFields, peersFields :: [Field DSelect _blkt' BlockProp]
   anomalyFields  = filterFields $ dFields bpFieldsControl
   forgingFields  = filterFields $ dFields bpFieldsForger
   peersFields    = filterFields $ dFields bpFieldsPeers
   resourceFields :: [Field DSelect cpt MachPerf]
   resourceFields = filterFields $ dFields mtFieldsReport

   mapBlkFields :: [FieldName] -> SomeBlockProp -> [Double]
   mapBlkFields fields (SomeBlockProp (bp' :: BlockProp _bpt'))
     = [ mapField bp' cdfAverageVal f | f <- filterFields $ dFields fields ]
   anomalyMapFields, forgingMapFields, peersMapFields :: SomeBlockProp -> [Double]
   anomalyMapFields = mapBlkFields bpFieldsControl
   forgingMapFields = mapBlkFields bpFieldsForger
   peersMapFields   = mapBlkFields bpFieldsPeers
   resourceMapFields :: MachPerf cpt -> [Double]
   resourceMapFields cp'
     = [ mapField cp' cdfAverageVal f | f <- resourceFields ]

   mapBlkSizes :: [FieldName] -> SomeBlockProp -> [Int]
   mapBlkSizes fields (SomeBlockProp (bp' :: BlockProp _bpt'))
     = [ mapField bp' cdfSize f | f <- filterFields $ dFields fields ]
   anomalySamples, forgingSamples, peersSamples, resourceSamples :: [[Int]]
   anomalySamples  = map (mapBlkSizes bpFieldsControl) $ SomeBlockProp bp : map thd3 rest
   forgingSamples  = map (mapBlkSizes bpFieldsForger)  $ SomeBlockProp bp : map thd3 rest
   peersSamples    = map (mapBlkSizes bpFieldsPeers)   $ SomeBlockProp bp : map thd3 rest
   resourceSamples = map (\c -> [mapField c cdfSize f | f <- resourceFields]) $ cp : map snd3 rest

   anomalyBaseline, forgingBaseline, peersBaseline, resourceBaseline :: [Double]
   anomalyBaseline  = anomalyMapFields  $ SomeBlockProp bp
   forgingBaseline  = forgingMapFields  $ SomeBlockProp bp
   peersBaseline    = peersMapFields    $ SomeBlockProp bp
   resourceBaseline = resourceMapFields                 cp

   -- The innermost tuple is a chunk of a row corresponding to a single run.
   -- Before transpose, the inner list corresponds to fields / rows varying.
   -- Before transpose, the outer list corresponds to runs varying.
   mkDelta :: Double -> Double -> (Double, Double, Double)
   mkDelta x y = (y, y - x, 100 * (y - x) / x)
   anomalyMkDeltas, forgingMkDeltas, peersMkDeltas :: SomeBlockProp -> [(Double, Double, Double)]
   anomalyMkDeltas  = zipWith mkDelta anomalyBaseline  . anomalyMapFields
   forgingMkDeltas = zipWith mkDelta forgingBaseline . forgingMapFields
   peersMkDeltas   = zipWith mkDelta peersBaseline   . peersMapFields
   resourceMkDeltas :: MachPerf cpt -> [(Double, Double, Double)]
   resourceMkDeltas = zipWith mkDelta resourceBaseline . resourceMapFields

   mkRows :: [Double] -> ((SomeSummary, ClusterPerf, SomeBlockProp) -> [(Double, Double, Double)]) -> [[Double]]
   mkRows baseline project
                    = (baseline :)
                    . transpose
                    . map (concatMap $ \(u, v, w) -> [u, v, w])
                    . transpose
                    $ map project rest
   anomalyRows, forgingRows, peersRows, resourceRows :: [[Double]]
   anomalyRows      = mkRows anomalyBaseline  $ anomalyMkDeltas  . thd3
   forgingRows      = mkRows forgingBaseline  $ forgingMkDeltas  . thd3
   peersRows        = mkRows peersBaseline    $ peersMkDeltas    . thd3
   resourceRows     = mkRows resourceBaseline $ resourceMkDeltas . snd3

generate :: InputDir -> Maybe TextInputFile
         -> (SomeSummary, ClusterPerf, SomeBlockProp) -> [(SomeSummary, ClusterPerf, SomeBlockProp)]
         -> IO (ByteString, ByteString, Text)
generate (InputDir ede) mReport (SomeSummary summ, cp, SomeBlockProp bp) rest = do
  ctx  <- getReport metas (last restTmpls & trManifest & getComponent "cardano-node" & ciVersion)
  tmplRaw <- BS.readFile (maybe defaultReportPath unTextInputFile mReport)
  tmpl <- parseWith defaultSyntax (includeFile ede) "report" tmplRaw
  let tmplEnv           = mkTmplEnv ctx baseTmpl restTmpls
      tmplEnvSerialised = AEP.encodePretty tmplEnv
  Text.EDE.result
    (error . show)
    (pure . (tmplRaw, LBS.toStrict tmplEnvSerialised,) . LT.toStrict) $ tmpl >>=
    \x ->
      renderWith mempty x tmplEnv
 where
   metas = sumMeta summ : fmap (\(SomeSummary ss, _, _) -> sumMeta ss) rest

   defaultReportPath = ede <> "/report.ede"

   baseTmpl  = liftTmplRun summ
   restTmpls = fmap ((\(SomeSummary ss) -> liftTmplRun ss). fst3) rest

   mkTmplEnv rc b rs = fromPairs
     [ "report"     .= rc
     , "base"       .= b
     , "runs"       .= rs
     , "summary"    .= liftTmplSection (summaryReportSection AsOrg summ)
     , "analyses"   .= (liftTmplSection <$> analysesReportSections AsOrg cp bp)
     , "dictionary" .= metricDictionary
     , "charts"     .=
       ((dClusterPerf metricDictionary & onlyKeys clusterPerfKeys)
        <>
        (dBlockProp   metricDictionary & onlyKeys blockPropKeys))
     ]

onlyKeys :: [Text] -> Map.Map Text DictEntry -> [DictEntry]
onlyKeys ks m =
  ks <&>
     \case
       (Nothing, k) -> error $ "Report.generate:  missing metric: " <> show k
       (Just x, _) -> x
     . (flip Map.lookup m &&& identity)

blockPropKeys, clusterPerfKeys :: [Text]
clusterPerfKeys =
          [ "CentiCpu"
          , "CentiGC"
          , "CentiMut"
          , "Alloc"
          , "GcsMajor"
          , "GcsMinor"
          , "Heap"
          , "Live"
          , "RSS"

          , "cdfStarted"
          , "cdfBlkCtx"
          , "cdfLgrState"
          , "cdfLgrView"
          , "cdfLeading"

          , "cdfDensity"
          , "cdfBlockGap"
          , "cdfSpanLensCpu"
          , "cdfSpanLensCpuEpoch"
          ]

blockPropKeys =
          [ "cdfForgerLead"
          , "cdfForgerTicked"
          , "cdfForgerMemSnap"
          , "cdfForgerForge"
          , "cdfForgerAnnounce"
          , "cdfForgerSend"
          , "cdfPeerNoticeFirst"
          , "cdfPeerAdoption"
          , "cdf0.50"
          , "cdf0.80"
          , "cdf0.90"
          , "cdf0.96"
          ]
