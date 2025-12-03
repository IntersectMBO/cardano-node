{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Report
  ( module Cardano.Report
  )
where

import           Cardano.Analysis.API
import           Cardano.Analysis.Summary
import           Cardano.Prelude
import           Cardano.Render (renderScalarLim)
import           Cardano.Util

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

import Prelude (print, id)


newtype Author   = Author   { unAuthor   :: Text } deriving newtype (FromJSON, ToJSON)
newtype ShortId  = ShortId  { unShortId  :: Text } deriving newtype (FromJSON, ToJSON)
newtype Tag      = Tag      { unTag      :: Text } deriving newtype (FromJSON, ToJSON)

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

analysesReportSections :: MachPerf (CDF I) -> BlockProp f -> [Section]
analysesReportSections mp bp =
  [ STable mp (DSel @MachPerf  $ dFields mtFieldsReport)   "metric"  "average"    "perf" "clusterperf.report.org"
    "Resource Usage"

  , STable bp (DSel @BlockProp $ dFields bpFieldsControl)  "metric"  "average" "control" "blockprop.control.org"
    "Anomaly control"

  , STable bp (DSel @BlockProp $ dFields bpFieldsForger)   "metric"  "average"   "forge" "blockprop.forger.org"
    "Forging"

  , STable bp (DSel @BlockProp $ dFields bpFieldsPeers)    "metric"  "average"   "peers" "blockprop.peers.org"
    "Individual peer propagation"

  , STable bp (DSel @BlockProp $ dFields bpFieldsEndToEnd) "metric"  "average" "end2end" "blockprop.endtoend.org"
    "End-to-end propagation"
  ]


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



data Coloring = None | Green

colorCode :: Coloring -> Text
colorCode None  = T.empty
colorCode Green = "gr"

newtype TypstCell = C (Text, Coloring, Int)

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

data TmplSectionTypst
  = TmplTableTypst
    { tstTitle        :: !Text
    , tstHeader       :: ![TypstCell]
    , tstRows         :: ![[TypstCell]]
    , tstVars         :: ![(Text, Text)]
    }

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

liftTmplSectionTypst :: Section -> TmplSectionTypst
liftTmplSectionTypst =
  \case
    STable{..} ->
      TmplTableTypst
      { tstTitle        = sTitle
      , tstHeader       = []
      , tstRows         = [[]]
      , tstVars         = []
      }

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
     , "charts"     .=
       ((dClusterPerf metricDictionary & onlyKeys
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
          ])
        <>
        (dBlockProp   metricDictionary & onlyKeys
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
          ]))
     ]

   onlyKeys :: [Text] -> Map.Map Text DictEntry -> [DictEntry]
   onlyKeys ks m =
     ks <&>
     \case
       (Nothing, k) -> error $ "Report.generate:  missing metric: " <> show k
       (Just x, _) -> x
     . (flip Map.lookup m &&& identity)


trying :: forall f a. (a ~ Summary f, TimelineFields a)
  => (Field ISelect I a -> Bool) -> a -> [Text]
trying fieldSelr summ =
  fields' <&> renderScalarLim (Just 32) summ
 where
   fields' :: [Field ISelect I a]
   fields' = filter fieldSelr timelineFields

trying2 :: forall f a. (a ~ Summary f, TimelineFields a)
  => (Field ISelect I a -> Bool) -> a -> [Text]
trying2 fieldSelr _ =
  fields' <&> fShortDesc
 where
   fields' :: [Field ISelect I a]
   fields' = filter fieldSelr timelineFields


generateTypst :: InputDir -> Maybe TextInputFile
         -> (SomeSummary, ClusterPerf, SomeBlockProp) -> [(SomeSummary, ClusterPerf, SomeBlockProp)]
         -> IO (ByteString, ByteString, Text)
generateTypst (InputDir _ede) _mReport theBase@(SomeSummary summ, cp, SomeBlockProp bp) rest = do
  print ("YO!" :: String)

  ctx  <- getReport metas (last restTmpls & trManifest & getComponent "cardano-node" & ciVersion)

  -- tmplRaw <- BS.readFile (maybe defaultReportPath unTextInputFile mReport)
  -- tmpl <- parseWith defaultSyntax (includeFile ede) "report" tmplRaw

  tmplMain <- getDataFileName $ "report-templates" </> "typst" </> "main_comparison.ede"
  tmplRaw  <- BS.readFile tmplMain
  tmpl <- parseFile tmplMain

  let tmplEnv           = mkTmplEnv ctx baseTmpl restTmpls
      tmplEnvSerialised = AEP.encodePretty tmplEnv
  Text.EDE.result
    (error . show)
    (pure . (tmplRaw, LBS.toStrict tmplEnvSerialised,) . LT.toStrict) $ tmpl >>=
    \x ->
      renderWith mempty x tmplEnv
 where
   selSummary :: Field ISelect I a -> Bool
   selSummary = iFields sumFieldsReport

   summaryDescr = map cell (T.empty : trying2 selSummary summ)

   summaryContent = [map cell (wrapIn "``" runId : trying selSummary ss) | (SomeSummary ss, _, _) <- theBase : rest, let Metadata{ident=runId} = sumMeta ss]
   summary = transpose $ map justifyCells $ summaryDescr : summaryContent

   summarySection :: TmplSectionTypst
   summarySection = case summary of
      (h:t) -> (liftTmplSectionTypst $ summaryReportSection summ)
        { tstHeader   = h
        , tstRows     = t
        }
      _ -> error "generateTypst: implementation error"

   metas = sumMeta summ : fmap (\(SomeSummary ss, _, _) -> sumMeta ss) rest


   baseTmpl  = liftTmplRun summ
   restTmpls = fmap ((\(SomeSummary ss) -> liftTmplRun ss). fst3) rest

   mkTmplEnv rc b rs = fromPairs
     [ "report"     .= rc
     , "base"       .= b
     , "runs"       .= (b : rs)
     , "summary"    .= summarySection
     , "analyses"   .= (liftTmplSectionTypst <$> analysesReportSections cp bp)
     , "dictionary" .= metricDictionary
     , "charts"     .=
       ((dClusterPerf metricDictionary & onlyKeys
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
          ])
        <>
        (dBlockProp   metricDictionary & onlyKeys
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
          ]))
     ]

   onlyKeys :: [Text] -> Map.Map Text DictEntry -> [DictEntry]
   onlyKeys ks m =
     ks <&>
     \case
       (Nothing, k) -> error $ "Report.generate:  missing metric: " <> show k
       (Just x, _) -> x
     . (flip Map.lookup m &&& identity)
