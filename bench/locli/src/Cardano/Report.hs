{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Report
  ( generateTypst
  , generateTypstCDFs
  , generateTypstSummary
  , generateTypstReport
  , generateTypstProfiling
  , fieldsWarnIfZero
  , fieldsZeroIsUnreported
  , Section (sTypstFile, sTitle)
  , summaryReportSection
  , analysesReportSections
  , insertDeltaRows
  )
where

import           Cardano.Analysis.API
import           Cardano.Analysis.Summary
import           Cardano.Prelude
import           Cardano.Render (Anchor, CDF2Aspect, RenderConfig (..), aspectColHeadersAndProjections,
                                 profilingDataTable, renderAnchorDateVer, renderAnchorFiltersAndDomains, renderAnchorRuns,
                                 renderScalarLim)
import           Cardano.Typst
import           Cardano.Util as Util

import           Prelude (id, tail, zipWith3)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
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


-- | A report section: one table, of the fields selected from one analysis result.
data Section
  = Section
    { sTypstFile :: !FilePath -- ^ The single table file, as written by the workbench's analysis
    , sTitle     :: !Text
    , sTable     :: !SectionTable
    }

-- | The data and the field selection share their type, so a section yields its own values.
data SectionTable where
  TSummary :: TimelineFields a        => !a     -> !(Field ISelect I a -> Bool) -> SectionTable
  TCDFs    :: (CDFFields a p, KnownCDF p) => !(a p) -> !(Field DSelect p a -> Bool) -> SectionTable

summaryReportSection :: KnownCDF f => Summary f -> Section
summaryReportSection summ =
  Section "summary.typ" "Overall run parameters" $
    TSummary summ (iFields sumFieldsReport)

machPerfSection :: MachPerf (CDF I) -> Section
machPerfSection mp =
  Section "clusterperf.report.typ" "Resource Usage" $
    TCDFs mp (dFields mtFieldsReport)

blockPropSections :: KnownCDF f => BlockProp f -> [Section]
blockPropSections bp =
  [ Section "blockprop.control.typ" "Anomaly control" $
    TCDFs bp (dFields bpFieldsControl)

  , Section "blockprop.forger.typ" "Forging" $
    TCDFs bp (dFields bpFieldsForger)

  , Section "blockprop.peers.typ" "Individual peer propagation" $
    TCDFs bp (dFields bpFieldsPeers)

  , Section "blockprop.endtoend.typ" "End-to-end propagation" $
    TCDFs bp (dFields bpFieldsEndToEnd)
  ]

-- | Derived / complementary performance metrics; reports put them on a page of their own.
derivedPerfSections :: MachPerf (CDF I) -> [Section]
derivedPerfSections mp =
  [ Section "clusterperf.snapshots.typ" "Ledger Snapshots" $
    TCDFs mp (dFields mtFieldsSnapshots)

  , Section "clusterperf.txprocessing.typ" "Transaction Processing" $
    TCDFs mp (dFields mtFieldsTxProcessing)
  ]

analysesReportSections :: KnownCDF f => MachPerf (CDF I) -> BlockProp f -> [Section]
analysesReportSections mp bp =
  machPerfSection mp : blockPropSections bp <> derivedPerfSections mp

-- | Metrics whose value 0 most likely indicates a problem (e.g. trace parsing),
--   not a measurement: their cells are rendered as a warning.
fieldsWarnIfZero :: [FieldName]
fieldsWarnIfZero = [ "cdfSnapTaken" ]

-- | Metrics that can't be zero over a whole run if they're measured: a 0 there means the
--   node didn't report the field (optional in the observability spec), so it renders as
--   not available. A running node always does network I/O and writes to storage; the GC
--   fields are only reported by a managed runtime. Not storage reads: with its data in the
--   page cache, a node can read nothing from storage throughout.
fieldsZeroIsUnreported :: [FieldName]
fieldsZeroIsUnreported =
  [ "NetRd", "NetWr", "FsWr"
  , "CentiGC", "CentiMut", "GcsMajor", "GcsMinor", "Alloc", "Live", "Heap", "cdfGcPerTx", "cdfMutPerTx" ]

-- | A metric without data (e.g. missing from an older run's analysis), or one of
--   'fieldsZeroIsUnreported' that is zero throughout.
notAvailable :: forall p a. a p -> Field DSelect p a -> Bool
notAvailable x fld@Field{fId} =
  mapField x cdfSize fld == 0
  || (FieldName fId `elem` fieldsZeroIsUnreported && mapField x allZero fld)
 where
   allZero :: forall v. Divisible v => CDF p v -> Bool
   allZero CDF{cdfRange = Interval lo hi} = toDouble lo == 0 && toDouble hi == 0

sectionRowDescs :: Section -> [Text]
sectionRowDescs = sTable >>> \case
  TSummary _ sel -> filter sel timelineFields <&> fShortDesc
  TCDFs    _ sel -> filter sel cdfFields      <&> \f -> fShortDesc f <> ", " <> renderUnit (fUnit f)

-- | A section's values: the summary's as text, the averages of the CDFs as numbers.
sectionValues :: Section -> Either [Text] [NumField]
sectionValues = sTable >>> \case
  TSummary x sel -> Left  $ extractAsText  sel x
  TCDFs    x sel -> Right $ extractAverage sel x


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
      Right doubles -> map (map cellNumField) (insertDeltaRows doubles)

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

insertDeltaRows :: [[NumField]] -> [[NumField]]
insertDeltaRows =
  transpose . map go1 . transpose
  where
    noColoring  = (, NoCol)

    go1 []      = error "insertDeltaRows: implementation error - empty row data"
    go1 [r]     = [r]
    go1 (r:rs)  = r : go (numFieldValue r) rs

    go _ [] = []
    go base (x@((val, prec), _):xs) =
      let
        absDelta = val - base
        relDelta = if base == 0 then nan else 100 * absDelta / base
        -- the absolute delta is formatted like the values it's taken from
        absPrec  = prec <|> Just 3
        (valAbs, valDelta)
          | abs absDelta < threshold = ((if absDelta < 0.0 then -0.0 else 0.0, absPrec), (if base == 0 then nan else 0.0, Just 0))
          | otherwise                = ((absDelta, absPrec), (relDelta, Just $ if abs relDelta < 1.0 then 1 else 0))
      in x : noColoring valAbs : noColoring valDelta : go base xs


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

-- | A comparison table: titles and rows from the base run's section, values from each run's own.
liftTmplSectionTypst :: [Text] -> Section -> [Section] -> TmplSectionTypst
liftTmplSectionTypst tstRunIds base runs =
  TmplTableTypst
    { tstTitle        = sTitle base
    , tstHeader       = []
    , tstRows         = [[]]
    , tstRowDesc      = sectionRowDescs base
    , ..
    }
  where
    tstVals = case partitionEithers (sectionValues <$> runs) of
      (texts, [])   -> Left texts
      ([], numbers) -> Right numbers
      _             -> error "liftTmplSectionTypst: runs' sections differ in kind"

extractAsText :: forall a. TimelineFields a
  => (Field ISelect I a -> Bool) -> a -> [Text]
extractAsText fieldSelr summ =
  fields' <&> renderSummaryScalar summ
 where
   fields' :: [Field ISelect I a]
   fields' = filter fieldSelr timelineFields

-- | The average of each selected field: not available without data; colored as a warning where applicable.
extractAverage :: forall p a. (CDFFields a p, KnownCDF p)
    => (Field DSelect p a -> Bool) -> a p -> [NumField]
extractAverage fieldSelr x =
  fields' <&> \fld@Field{fId, fWidth} ->
    let avg = if notAvailable x fld then nan else f fld
    in ( (avg, succ <$> unWidth fWidth)
       , if FieldName fId `elem` fieldsWarnIfZero && avg == 0 then Yellow else NoCol )
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
generateTypst typstFileName mScalingCSV theBase rest = do
  ctx <- getReport metas (last templates & trManifest & getComponent "cardano-node" & ciVersion)
  renderTypstTemplate "main_comparison.ede" $ mkTmplEnv ctx templates
 where
   allRuns = theBase : rest

   -- The summary, then the analyses: the same sections, in the same order, for every run.
   sectionsOf :: (SomeSummary, ClusterPerf, SomeBlockProp) -> [Section]
   sectionsOf (SomeSummary ss, cperf, SomeBlockProp sbp) =
     summaryReportSection ss : analysesReportSections cperf sbp

   metas     :: [Metadata]
   templates :: [TmplRun]
   runIds    :: [Text]
   (metas, templates) = unzip
     [(sumMeta ss, liftTmplRun ss) | (SomeSummary ss, _, _) <- allRuns]
   runIds = map ident metas

   (summarySection, analysisSections) =
     case zipWith (liftTmplSectionTypst runIds) (sectionsOf theBase) (transpose $ sectionsOf <$> allRuns) of
       s : as -> (s, as)
       []     -> error "generateTypst: no sections"

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
--   With a Plutus workload calibration CSV, it's rendered as an addendum.
--   For a variance report (the results of 'Cardano.Analysis.Summary.summariseMultiSummary'
--   & co.), the input runs are given, and listed on the title page and in the Manifest.
generateTypstReport :: (KnownCDF f, KnownCDF g)
  => FilePath -> Maybe FilePath -> [Metadata] -> Summary g -> ClusterPerf -> BlockProp f -> IO Text
generateTypstReport typstPath mScalingCSV varianceInputs summ cp bp = do
  ctx <- getReport [sumMeta summ] (baseTmpl & trManifest & getComponent "cardano-node" & ciVersion)
  summaryFile  <- tableFile (summaryReportSection summ)
  sectionFiles <- mapM tableFile (analysesReportSections cp bp)
  renderTypstTemplate "main_single.ede" $ fromPairs $
    [ "report"     .= ctx
    , "base"       .= baseTmpl
    , "runs"       .= [baseTmpl]
    , "summary"    .= summaryFile
    , "analyses"   .= sectionFiles
    , "dictionary" .= metricDictionary
    , "charts"     .= charts
    , "typstFile"  .= FP.takeFileName typstPath
    ]
    ++ maybe [] (\csvFile -> ["scalingCSV" .= csvFile]) mScalingCSV
    ++ [ "variance" .= (varianceInputs <&> \Metadata{tag, ident} -> object ["tag" .= tag, "ident" .= ident])
       | not (null varianceInputs) ]
 where
   baseTmpl = liftTmplRun summ

   tableFile :: Section -> IO TableFile
   tableFile section = do
     let tfName = sTypstFile section
     tfPresent <- doesFileExist (FP.takeDirectory typstPath </> tfName)
     pure TableFile{tfTitle = sTitle section, ..}

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
-- Single tables: one table per file, per run (or multi-run); bundled by the single-run report.
--

-- | One row per metric; columns: the statistics of 'aspectColHeadersAndProjections'.
generateTypstCDFs :: forall p a. (CDFFields a p, KnownCDF p)
  => RenderConfig -> Anchor -> (Field DSelect p a -> Bool) -> CDF2Aspect -> a p -> IO Text
generateTypstCDFs rc anchor fieldSelr aspect x =
  renderTypstSingle rc anchor
    (cell "metric" : fmap cell (fst (hdrsProjs @Integer)))
    (fields' <&> \f@Field{..} ->
        cell (fShortDesc <> ", " <> renderUnit fUnit)
        : zipWith3 (\col prec v -> cellNumField ((v, prec), coloring f col v))
                   [0 :: Int ..]
                   (colPrecs f)
                   (values f))
 where
   -- A metric not available ('notAvailable') renders as such, except for its precision and size.
   values :: Field DSelect p a -> [Double]
   values f
     | notAvailable x f          = (nan <$ [1 .. 6 :: Int]) <> drop 6 (mapFieldWithKey x (snd hdrsProjs) f)
     | otherwise                 = mapFieldWithKey x (snd hdrsProjs) f

   -- Columns 0 and 2: average and min.
   coloring :: Field DSelect p a -> Int -> Double -> Coloring
   coloring Field{fId} col v
     | FieldName fId `elem` fieldsWarnIfZero, col `elem` [0, 2], v == 0 = Yellow
     | otherwise = NoCol

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
        , cell (renderSummaryScalar summ f) ])

-- | A summary value for the Typst tables: fractional numbers (incl. durations) are
--   rounded to 3 decimals by 'renderNumField', as in all other tables; whole-number
--   averages render without decimals, so a fractional one stands out (e.g. hosts
--   disagreeing on a value that should be equal); other values render as in 'renderScalarLim'.
renderSummaryScalar :: a -> Field ISelect I a -> Text
renderSummaryScalar summ f@Field{fSelect} =
  case fSelect of
    IFloat   g -> let x = g summ
                  in if x == fromIntegral @Integer (round x)
                     then renderNumField ((x, Just 0), NoCol)
                     else num x
    IDeltaT  g -> num (realToFrac $ g summ)
    IDeltaTM g -> smaybe "---" (num . realToFrac) (g summ)
    _          -> renderScalarLim (Just 32) summ f
 where
   num x = renderNumField ((x, Just 3), NoCol)

-- | Profiling data (if the run was profiled): one row per function, sorted by time share.
generateTypstProfiling :: RenderConfig -> Anchor -> (ProfileEntry (CDF I) -> Bool) -> ProfilingData (CDF I) -> IO Text
generateTypstProfiling rc anchor flt pd =
  renderTypstSingle rc anchor (cell <$> header) (fmap cell <$> rows)
 where
   (header, rows) = profilingDataTable flt pd

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
-- charts to be plotted
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
