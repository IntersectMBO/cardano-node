{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Report
  ( generateOrg
  , generateTypst
  )
where

import           Cardano.Analysis.API
import           Cardano.Analysis.Summary
import           Cardano.Prelude
import           Cardano.Render (formatDouble, renderScalarLim)
import           Cardano.Util as Util

import           Prelude (id, tail, unzip3)

import qualified Data.Aeson.Encode.Pretty as AEP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Tuple.Extra (fst3)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>))
import           System.Posix.User
import           Text.EDE hiding (Id)

import           Paths_locli


newtype Author   = Author   { _unAuthor   :: Text } deriving newtype (FromJSON, ToJSON)
newtype ShortId  = ShortId  { _unShortId  :: Text } deriving newtype (FromJSON, ToJSON)
newtype Tag      = Tag      { _unTag      :: Text } deriving newtype (FromJSON, ToJSON)

data ReportMeta
  = ReportMeta
    { rmAuthor       :: !Author
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


getReport :: [Metadata] -> Version -> IO ReportMeta
getReport metas _ver = do
  rmAuthor <- getGecosFullUsername
              `catch`
              \(_ :: SomeException) ->
                 getFallbackUserId
  rmDate <- getCurrentTime <&> T.take 10 . show
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
  | WPlutusLoopRipemd
  | WPlutusUnknown

instance ToJSON Workload where
  toJSON = \case
    WValue               -> "value-only"
    WPlutusLoopCountdown -> "Plutus countdown loop"
    WPlutusLoopSECP      -> "Plutus SECP loop"
    WPlutusLoopBLST      -> "Plutus BLST loop"
    WPlutusLoopRipemd    -> "Plutus RIPEMD-160 loop"
    WPlutusUnknown       -> "Plutus (other)"

filenameInfix :: Workload -> Text
filenameInfix = \case
  WPlutusLoopCountdown  -> "plutus"
  WPlutusLoopSECP       -> "plutus-secp"
  WPlutusLoopBLST       -> "plutus-blst"
  WPlutusLoopRipemd     -> "plutus-ripemd"
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

summaryReportSection :: Summary f -> Section
summaryReportSection summ =
  STable summ (ISel @SummaryOne $ iFields sumFieldsReport) "Parameter" "Value"   "summary" "summary.org"
    "Overall run parameters"

machPerfSection :: MachPerf (CDF I) -> Section
machPerfSection mp =
  STable mp (DSel @MachPerf     $ dFields mtFieldsReport)  "metric"  "average"    "perf" "clusterperf.report.org"
    "Resource Usage"

blockPropSections :: BlockProp f -> [Section]
blockPropSections bp =
  [ STable bp (DSel @BlockProp $ dFields bpFieldsControl)  "metric"  "average" "control" "blockprop.control.org"
    "Anomaly control"

  , STable bp (DSel @BlockProp $ dFields bpFieldsForger)   "metric"  "average"   "forge" "blockprop.forger.org"
    "Forging"

  , STable bp (DSel @BlockProp $ dFields bpFieldsPeers)    "metric"  "average"   "peers" "blockprop.peers.org"
    "Individual peer propagation"

  , STable bp (DSel @BlockProp $ dFields bpFieldsEndToEnd) "metric"  "average" "end2end" "blockprop.endtoend.org"
    "End-to-end propagation"
  ]

analysesReportSections :: MachPerf (CDF I) -> BlockProp f -> [Section]
analysesReportSections mp bp =
  machPerfSection mp : blockPropSections bp

rowDecriptions :: FSelect -> [Text]
rowDecriptions = \case
  ISel sel -> filter sel timelineFields <&> fShortDesc
  DSel sel -> filter sel cdfFields      <&> describe
  where
    describe f = fShortDesc f <> ", " <> renderUnit (fUnit f)


--
-- Representation of a run, structured for template generator's needs.
--

liftTmplRun :: Summary a -> TmplRun
liftTmplRun Summary{sumWorkload=generatorProfile
                   ,sumMeta=meta@Metadata{..}} =
  TmplRun
  { trMeta      = meta {profile_content = mempty}   -- currently not required in any .ede template
  , trManifest  = manifest & unsafeShortenManifest 5
  , trWorkload  =
    case plutusLoopScript generatorProfile of
      Nothing                               -> WValue
      Just script
        | "Loop" `T.isPrefixOf` script      -> WPlutusLoopCountdown
        | script == "EcdsaSecp256k1Loop"    -> WPlutusLoopSECP
        | script == "SchnorrSecp256k1Loop"  -> WPlutusLoopSECP
        | script == "HashOntoG2AndAdd"      -> WPlutusLoopBLST
        | script == "Ripemd160"             -> WPlutusLoopRipemd
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



---
--- Org
---

liftTmplSectionOrg :: Section -> TmplSectionOrg
liftTmplSectionOrg =
  \case
    STable{..} ->
      TmplTableOrg
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

data TmplSectionOrg
  = TmplTableOrg
    { tsTitle        :: !Text
    , tsNameCol      :: !Text
    , tsValueCol     :: !Text
    , tsDataRef      :: !Text
    , tsOrgFile      :: !Text
    , tsRowPrecs     :: ![Int]
    , tsVars         :: ![(Text, Text)] -- map from Org constant name to description
    }

instance ToJSON TmplSectionOrg where
  toJSON TmplTableOrg{..} = object
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

generateOrg :: InputDir -> Maybe TextInputFile
         -> (SomeSummary, ClusterPerf, SomeBlockProp) -> [(SomeSummary, ClusterPerf, SomeBlockProp)]
         -> IO (ByteString, ByteString, Text)
generateOrg (InputDir ede) mReport (SomeSummary summ, cp, SomeBlockProp bp) rest = do
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
     , "summary"    .= liftTmplSectionOrg (summaryReportSection summ)
     , "analyses"   .= (liftTmplSectionOrg <$> analysesReportSections cp bp)
     , "dictionary" .= metricDictionary
     , "charts"     .= charts
     ]


---
--- Typst
---

data Coloring = None | Green | Red deriving (Eq, Show)

colorCode :: Coloring -> Text
colorCode None  = T.empty
colorCode Green = "gr"
colorCode Red   = "rd"

newtype TypstCell = C (Text, Coloring, Int) deriving Show

instance IsString TypstCell where
  fromString x = C (fromString x, None, 0)

instance ToJSON TypstCell where
  toJSON (C (t, col, width)) = object
    [ "cont"  .= (colorCode col <> wrapIn "[]" t)
    , "width" .= width
    ]

cell :: Text -> TypstCell
cell t = C (t, None, 0)

justifyCells :: [TypstCell] -> [TypstCell]
justifyCells [] = []
justifyCells cs =
  let maxWidth = maximum $ map width cs
  in map (\(C (t, c, _)) -> C (t, c, maxWidth)) cs
  where
    width (C (t, None, _)) = 2 + T.length t
    width (C (t ,_ , _))   = 4 + T.length t

wrapIn :: String -> Text -> Text
wrapIn [open, close] = T.cons open . (`T.snoc` close)
wrapIn _             = id

renderTableTypst :: TmplSectionTypst -> TmplSectionTypst
renderTableTypst tmpl@TmplTableTypst{..} =
  tmpl {tstHeader = Util.head table, tstRows = tail table}
  where
    valCells = case tstVals of
      Left txts     -> map (map cell) txts
      Right doubles -> map (map (cell . formatDouble W5)) (insertDeltaRows tstRowDesc doubles)

    runIdsMono  = wrapIn "``" <$> tstRunIds
    runIds      = map cell $
      if isRight tstVals then insertDeltaHeaders runIdsMono else runIdsMono

    allContent =
        map cell (T.empty : tstRowDesc)
      : zipWith (:) runIds valCells

    table = transpose $ map justifyCells allContent

insertDeltaHeaders :: [Text] -> [Text]
insertDeltaHeaders (base:rs@(_:_)) =
  base : concatMap extraCols rs
  where
    extraCols = (: ["Δ", "Δ%"])
insertDeltaHeaders x = x

insertDeltaRows :: [Text] -> [[Double]] -> [[Double]]
insertDeltaRows rowDescs =
  transpose . map go1 . transpose
  where
    go1 []      = error "insertDeltaRows: implementation error - empty row data"
    go1 [r]     = [r]
    go1 (r:rs)  = r : go r rowDescs rs

    go _ _ [] = []
    go base (_rowDesc:rds) (x:xs) =
      let
        absDelta = x - base
        relDelta = if base == 0 then nan else 100 * absDelta / base
      in x : absDelta : relDelta : go base rds xs
    go _ [] _ = error "insertDeltaRows: implementation error - missing row definitions"

nan :: Double
nan = 0 / 0


data TmplSectionTypst
  = TmplTableTypst
    { tstTitle        :: !Text
    , tstHeader       :: ![TypstCell]
    , tstRows         :: ![[TypstCell]]
    , tstRunIds       :: ![Text]
    , tstRowDesc      :: ![Text]
    , tstVals         :: !(Either [[Text]] [[Double]])
    , tstVars         :: ![(Text, Text)]
    }
    deriving Show

instance ToJSON TmplSectionTypst where
  toJSON TmplTableTypst{..} = object
    [ "title"     .= tstTitle
    , "header"    .= tstHeader
    , "rows"      .= tstRows
    , "vars"      .= Map.fromList (zip tstVars ([0..] <&> flip T.replicate ">" . (length tstVars -))
                                   <&> \((k, name), angles) ->
                                         (k, Map.fromList @Text
                                             [("name", name),
                                              ("angles", angles)]))
    ]

liftTmplSectionTypst :: [Text] -> Either [[Text]] [[Double]] -> Section -> TmplSectionTypst
liftTmplSectionTypst tstRunIds tstVals STable{..} =
  TmplTableTypst
    { tstTitle        = sTitle
    , tstHeader       = []
    , tstRows         = [[]]
    , tstVars         = []
    , ..
    }
  where
    tstRowDesc = rowDecriptions sFields

extractAsText :: forall f a. (a ~ Summary f, TimelineFields a)
  => (Field ISelect I a -> Bool) -> a -> [Text]
extractAsText fieldSelr summ =
  fields' <&> renderScalarLim (Just 32) summ
 where
   fields' :: [Field ISelect I a]
   fields' = filter fieldSelr timelineFields

extractAverage :: forall p a. (CDFFields a p, KnownCDF p)
    => (Field DSelect p a -> Bool) -> a p -> [Double]
extractAverage fieldSelr x =
  fields' <&> f
 where
   fields' :: [Field DSelect p a]
   fields' = filter fieldSelr cdfFields

   f :: Field DSelect p a -> Double
   f = mapField x cdfAverageVal


generateTypst ::
            FilePath
         -> (SomeSummary, ClusterPerf, SomeBlockProp) -> [(SomeSummary, ClusterPerf, SomeBlockProp)]
         -> IO (ByteString, Text)
generateTypst typstFileName theBase@(SomeSummary summ, cp, SomeBlockProp bp) rest = do
  ctx       <- getReport metas (last templates & trManifest & getComponent "cardano-node" & ciVersion)
  tmplMain  <- getDataFileName $ "report-templates" </> "typst" </> "main_comparison.ede"
  tmpl      <- parseFile tmplMain

  let tmplEnv           = mkTmplEnv ctx templates
      tmplEnvSerialised = AEP.encodePretty tmplEnv
  Text.EDE.result
    (error . show)
    (pure . (LBS.toStrict tmplEnvSerialised,) . LT.toStrict) $ tmpl >>=
    \x ->
      renderWith mempty x tmplEnv
 where
   allRuns = theBase : rest

   selSummary :: Field ISelect I a -> Bool
   selSummary = iFields sumFieldsReport

   selAnalyses :: (SomeSummary, ClusterPerf, SomeBlockProp) -> [[Double]]
   selAnalyses (_, cperf, SomeBlockProp sbp) =
     [ extractAverage (dFields mtFieldsReport)   cperf
     , extractAverage (dFields bpFieldsControl)  sbp
     , extractAverage (dFields bpFieldsForger)   sbp
     , extractAverage (dFields bpFieldsPeers)    sbp
     , extractAverage (dFields bpFieldsEndToEnd) sbp
     ]

   metas          :: [Metadata]
   templates      :: [TmplRun]
   summaryContent :: [[Text]]
   runIds         :: [Text]
   (metas, templates, summaryContent) = unzip3
     [(sumMeta ss, liftTmplRun ss, extractAsText selSummary ss) | (SomeSummary ss, _, _) <- allRuns]
   runIds = map ident metas

   summarySection = liftTmplSectionTypst runIds (Left summaryContent) $ summaryReportSection summ

   analysisSections =
     [ liftTmplSectionTypst runIds (Right content) section
        | (content, section) <- zip analysesContents (analysesReportSections cp bp)
     ]
     where
      analysesContents = transpose $ map selAnalyses allRuns

   mkTmplEnv rc runTempls = fromPairs
     [ "report"     .= rc
     , "base"       .= Util.head runTempls
     , "runs"       .= runTempls
     , "summary"    .= renderTableTypst summarySection
     , "analyses"   .= map renderTableTypst analysisSections
     , "dictionary" .= metricDictionary
     , "charts"     .= charts
     , "typstFile"  .= typstFileName
     ]


--
-- charts to be plotted (used in both report types)
--

charts :: [DictEntry]
charts =
  (dClusterPerf metricDictionary & onlyKeys
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
    , "cdfLeading"

    , "cdfDensity"
    , "cdfBlockGap"
    , "cdfSpanLensCpu"
    , "cdfSpanLensCpuEpoch"
    ])
  <>
  (dBlockProp   metricDictionary & onlyKeys
    [ "cdfForgerLead"
    , "cdfForgerTicked"
    , "cdfForgerMemSnap"
    , "cdfForgerForge"
    , "cdfForgerAnnounce"
    , "cdfForgerAnnounceCum"
    , "cdfForgerSend"
    , "cdfPeerNoticeFirst"
    , "cdfPeerAdoption"
    , "cdf0.50"
    , "cdf0.80"
    , "cdf0.90"
    , "cdf0.96"
    ])
  where
   onlyKeys :: [Text] -> Map.Map Text DictEntry -> [DictEntry]
   onlyKeys ks m =
     ks <&>
     \case
       (Nothing, k) -> error $ "Report.charts:  missing metric: " <> show k
       (Just x, _) -> x
     . (flip Map.lookup m &&& identity)
