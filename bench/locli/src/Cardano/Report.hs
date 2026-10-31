{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Report
  ( generateOrg
  , generateTypst
  , generateTypstCDFs
  , generateTypstSummary
  , generateTypstReport
  )
where

import           Cardano.Analysis.API
import           Cardano.Analysis.Summary
import           Cardano.Prelude
import           Cardano.Render (Anchor, CDF2Aspect, RenderConfig (..), aspectColHeadersAndProjections,
                                 renderAnchorDateVer, renderAnchorFiltersAndDomains, renderAnchorRuns,
                                 renderScalarLim)
import           Cardano.Typst
import           Cardano.Util as Util

import           Prelude (id, tail, unzip3)

import qualified Data.Aeson.Encode.Pretty as AEP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Tuple.Extra (fst3)
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>))
import qualified System.FilePath as FP
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
  | WPlutusLoopExpMod
  | WPlutusUnknown

instance ToJSON Workload where
  toJSON = \case
    WValue               -> "value-only"
    WPlutusLoopCountdown -> "Plutus countdown loop"
    WPlutusLoopSECP      -> "Plutus SECP loop"
    WPlutusLoopBLST      -> "Plutus BLST loop"
    WPlutusLoopRipemd    -> "Plutus RIPEMD-160 loop"
    WPlutusLoopExpMod    -> "Plutus ExpMod loop"
    WPlutusUnknown       -> "Plutus (other)"

filenameInfix :: Workload -> Text
filenameInfix = \case
  WPlutusLoopCountdown  -> "plutus"
  WPlutusLoopSECP       -> "plutus-secp"
  WPlutusLoopBLST       -> "plutus-blst"
  WPlutusLoopRipemd     -> "plutus-ripemd"
  WPlutusLoopExpMod     -> "plutus-expmod"
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
      Nothing                                  -> WValue
      Just script
        | "Loop"        `T.isPrefixOf` script  -> WPlutusLoopCountdown
        | "EcdsaSecp"   `T.isPrefixOf` script  -> WPlutusLoopSECP
        | "SchnorrSecp" `T.isPrefixOf` script  -> WPlutusLoopSECP
        | script == "HashOntoG2AndAdd"         -> WPlutusLoopBLST
        | script == "MultiScalarMulG1"         -> WPlutusLoopBLST
        | script == "Ripemd160"                -> WPlutusLoopRipemd
        | script == "ExpModInteger"            -> WPlutusLoopExpMod
        | otherwise                            -> WPlutusUnknown
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

newtype TypstCell = C (Text, Coloring, Int) deriving Show

instance IsString TypstCell where
  fromString = cell . fromString

instance ToJSON TypstCell where
  toJSON (C (t, col, width)) = object
    [ "cont"  .= (colorCode col <> wrapIn "[]" t)
    , "width" .= width
    ]

-- | A cell displaying the text verbatim.
cell :: Text -> TypstCell
cell t = C (typstEscape t, NoCol, 0)

-- | A cell displaying the text in monospace (raw, so no escaping applies).
rawCell :: Text -> TypstCell
rawCell t = C (wrapIn "``" t, NoCol, 0)

cellNumField :: NumField -> TypstCell
cellNumField nf@(_, col) = C (typstEscape $ renderNumField nf, col, 0)

justifyCells :: [TypstCell] -> [TypstCell]
justifyCells [] = []
justifyCells cs =
  let maxWidth = maximum $ map width cs
  in map (\(C (t, c, _)) -> C (t, c, maxWidth)) cs
  where
    width (C (t, NoCol, _)) = 2 + T.length t
    width (C (t ,_ , _))    = 4 + T.length t

wrapIn :: String -> Text -> Text
wrapIn [open, close] = T.cons open . (`T.snoc` close)
wrapIn _             = id

renderTableTypst :: TmplSectionTypst -> TmplSectionTypst
renderTableTypst tmpl@TmplTableTypst{..} =
  tmpl {tstHeader = Util.head table, tstRows = tail table}
  where
    valCells = case tstVals of
      Left txts     -> map (map cell) txts
      Right doubles -> map (map cellNumField) (insertDeltaRows tstRowDesc doubles)

    runIds = if isRight tstVals
             then insertDeltaHeaders (rawCell <$> tstRunIds)
             else rawCell <$> tstRunIds

    allContent =
        map cell (T.empty : tstRowDesc)
      : zipWith (:) runIds valCells

    table = transpose $ map justifyCells allContent

insertDeltaHeaders :: [TypstCell] -> [TypstCell]
insertDeltaHeaders (base:rs@(_:_)) =
  base : concatMap extraCols rs
  where
    extraCols = (: ["Δ", "Δ%"])
insertDeltaHeaders x = x

insertDeltaRows :: [Text] -> [[NumField]] -> [[NumField]]
insertDeltaRows rowDescs =
  transpose . map go1 . transpose
  where
    noColoring  = (, NoCol)

    go1 []      = error "insertDeltaRows: implementation error - empty row data"
    go1 [r]     = [r]
    go1 (r:rs)  = r : go (numFieldValue r) rowDescs rs

    go _ _ [] = []
    go base (_rowDesc:rds) (x@((val, _), _):xs) =
      let
        absDelta = val - base
        relDelta = if base == 0 then nan else 100 * absDelta / base
        (valAbs, valDelta)
          | abs absDelta < threshold = ((if absDelta < 0.0 then -0.0 else 0.0, Just 3), (if base == 0 then nan else 0.0, Just 0))
          | otherwise                = ((absDelta, Just 3), (relDelta, Just $ if abs relDelta < 1.0 then 1 else 0))
      in x : noColoring valAbs : noColoring valDelta : go base rds xs
    go _ [] _ = error "insertDeltaRows: implementation error - missing row definitions"


data TmplSectionTypst
  = TmplTableTypst
    { tstTitle        :: !Text
    , tstHeader       :: ![TypstCell]
    , tstRows         :: ![[TypstCell]]
    , tstRunIds       :: ![Text]
    , tstRowDesc      :: ![Text]
    , tstVals         :: !(Either [[Text]] [[NumField]])
    }
    deriving Show

instance ToJSON TmplSectionTypst where
  toJSON TmplTableTypst{..} = object
    [ "title"     .= tstTitle
    , "header"    .= tstHeader
    , "rows"      .= tstRows
    ]

liftTmplSectionTypst :: [Text] -> Either [[Text]] [[NumField]] -> Section -> TmplSectionTypst
liftTmplSectionTypst tstRunIds tstVals STable{..} =
  TmplTableTypst
    { tstTitle        = sTitle
    , tstHeader       = []
    , tstRows         = [[]]
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
    => (Field DSelect p a -> Bool) -> a p -> [(Double, Maybe Int)]
extractAverage fieldSelr x =
  fields' <&> f &&& (fmap succ . unWidth . fWidth)
 where
   fields' :: [Field DSelect p a]
   fields' = filter fieldSelr cdfFields

   f :: Field DSelect p a -> Double
   f = mapField x cdfAverageVal


generateTypst ::
            FilePath
         -> Maybe FilePath
         -> (SomeSummary, ClusterPerf, SomeBlockProp) -> [(SomeSummary, ClusterPerf, SomeBlockProp)]
         -> IO Text
generateTypst typstFileName mScalingCSV theBase@(SomeSummary summ, cp, SomeBlockProp bp) rest = do
  ctx <- getReport metas (last templates & trManifest & getComponent "cardano-node" & ciVersion)
  renderTypstTemplate "main_comparison.ede" $ mkTmplEnv ctx templates
 where
   allRuns = theBase : rest

   numFieldUncolored = (`zip` repeat NoCol)

   selSummary :: Field ISelect I a -> Bool
   selSummary = iFields sumFieldsReport

   selAnalyses :: (SomeSummary, ClusterPerf, SomeBlockProp) -> [[NumField]]
   selAnalyses (_, cperf, SomeBlockProp sbp) =
     map numFieldUncolored
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

   mkTmplEnv rc runTempls = fromPairs $
     [ "report"     .= rc
     , "base"       .= Util.head runTempls
     , "runs"       .= runTempls
     , "summary"    .= renderTableTypst summarySection
     , "analyses"   .= map renderTableTypst analysisSections
     , "dictionary" .= metricDictionary
     , "charts"     .= charts
     , "typstFile"  .= typstFileName
     ]
     ++ maybe [] (\csvFile -> ["scalingCSV" .= csvFile]) mScalingCSV


-- | The single-run report: bundles the single table files (as written by the
--   workbench's single-run analysis) found next to the report, by importing them.
--   Missing table files are noted in the report, instead of failing its compilation.
generateTypstReport :: FilePath -> SummaryOne -> ClusterPerf -> BlockProp f -> IO Text
generateTypstReport typstPath summ cp bp = do
  ctx <- getReport [sumMeta summ] (baseTmpl & trManifest & getComponent "cardano-node" & ciVersion)
  summaryFile  <- tableFile (summaryReportSection summ)
  sectionFiles <- mapM tableFile (analysesReportSections cp bp)
  renderTypstTemplate "main_single.ede" $ fromPairs
    [ "report"     .= ctx
    , "base"       .= baseTmpl
    , "runs"       .= [baseTmpl]
    , "summary"    .= summaryFile
    , "analyses"   .= sectionFiles
    , "dictionary" .= metricDictionary
    , "charts"     .= charts
    , "typstFile"  .= FP.takeFileName typstPath
    ]
 where
   baseTmpl = liftTmplRun summ

   -- The table file names are those of the Org mode report tables, with a .typ extension.
   tableFile :: Section -> IO TableFile
   tableFile STable{sTitle, sOrgFile} = do
     let tfName = FP.replaceExtension (T.unpack sOrgFile) "typ"
     tfPresent <- doesFileExist (FP.takeDirectory typstPath </> tfName)
     pure TableFile{tfTitle = sTitle, ..}

data TableFile
  = TableFile
    { tfTitle   :: !Text
    , tfName    :: !FilePath
    , tfPresent :: !Bool
    }

instance ToJSON TableFile where
  toJSON TableFile{..} = object
    [ "title"   .= tfTitle
    , "name"    .= tfName
    , "present" .= tfPresent
    ]

-- | Render one of the bundled Typst templates (report-templates/typst).
renderTypstTemplate :: FilePath -> HM.HashMap Text Value -> IO Text
renderTypstTemplate name env = do
  path <- getDataFileName $ "report-templates" </> "typst" </> name
  tmpl <- parseFile path
  Text.EDE.result
    (error . show)
    (pure . LT.toStrict) $ tmpl >>=
    \x ->
      renderWith mempty x env


--
-- Single tables: the per-run (or multi-run) counterparts of Org mode report tables.
--

-- | One row per metric; the columns as for the Org mode report tables.
generateTypstCDFs :: forall p a. (CDFFields a p, KnownCDF p)
  => RenderConfig -> Anchor -> (Field DSelect p a -> Bool) -> CDF2Aspect -> a p -> IO Text
generateTypstCDFs rc anchor fieldSelr aspect x =
  renderTypstSingle rc anchor
    (cell "metric" : fmap cell (fst (hdrsProjs @Integer)))
    (fields' <&> \f@Field{..} ->
        cell (fShortDesc <> ", " <> renderUnit fUnit)
        : zipWith (\prec v -> cellNumField ((v, prec), NoCol))
                  (colPrecs f)
                  (mapFieldWithKey x (snd hdrsProjs) f))
 where
   fields' :: [Field DSelect p a]
   fields' = filter fieldSelr cdfFields

   hdrsProjs :: forall v. (Divisible v) => ([Text], Field DSelect p a -> CDF p v -> [Double])
   hdrsProjs = aspectColHeadersAndProjections aspect

   -- average, CoV, min, max, stddev, range, precision, size;
   -- field-derived precision as in the comparison report.
   colPrecs :: Field DSelect p a -> [Maybe Int]
   colPrecs Field{fWidth} =
     let prec = succ <$> unWidth fWidth
     in [prec, Just 3, prec, prec, prec, prec, Just 0, Just 0]

generateTypstSummary :: forall f a. (a ~ Summary f, TimelineFields a)
  => RenderConfig -> Anchor -> (Field ISelect I a -> Bool) -> a -> IO Text
generateTypstSummary rc anchor fieldSelr summ =
  renderTypstSingle rc anchor
    ["Parameter", "Value"]
    (filter fieldSelr timelineFields <&> \f ->
        [ cell (fShortDesc f)
        , cell (renderScalarLim (Just 32) summ f) ])

renderTypstSingle :: RenderConfig -> Anchor -> [TypstCell] -> [[TypstCell]] -> IO Text
renderTypstSingle RenderConfig{rcDateVerMetadata, rcRunMetadata} anchor header rows =
  renderTypstTemplate "single_table.ede" $ fromPairs
    [ "anchor" .= object
      ( [ "title"    .= typstEscape (renderAnchorRuns anchor)    | rcRunMetadata ]
     <> [ "subtitle" .= typstEscape (renderAnchorFiltersAndDomains anchor) ]
     <> [ "dateVer"  .= typstEscape (renderAnchorDateVer anchor) | rcDateVerMetadata ]
      )
    , "table" .= object
      [ "header" .= Util.head table
      , "rows"   .= tail table
      ]
    ]
 where
   -- justify column-wise
   table = transpose . map justifyCells . transpose $ header : rows


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
  (dBlockProp metricDictionary & onlyKeys
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
