{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Report
  ( module Cardano.Report
  )
where

import Cardano.Prelude

import Data.ByteString   qualified as BS
import Data.HashMap.Lazy qualified as HM
import Data.Map.Strict   qualified as Map
import Data.Text         qualified as T
import Data.Text.Lazy    qualified as LT
import Data.Time.Clock
import System.FilePath as FS
import System.Posix.User
import System.Environment (lookupEnv)

import Text.EDE hiding (Id)

import Data.CDF
import Cardano.Util
import Cardano.Analysis.API


newtype Author   = Author   { unAuthor   :: Text } deriving newtype (FromJSON, ToJSON)
newtype Revision = Revision { unRevision :: Int }  deriving newtype (FromJSON, ToJSON)
newtype ShortId  = ShortId  { unShortId  :: Text } deriving newtype (FromJSON, ToJSON)

data ReportMeta
  = ReportMeta
    { rmAuthor       :: !Author
    , rmDate         :: !UTCTime
    , rmRevision     :: !Revision
    , rmLocliVersion :: !LocliVersion
    , rmTarget       :: !Version
    }
instance ToJSON ReportMeta where
  toJSON ReportMeta{..} = object
    [ "author"     .= rmAuthor
    , "date"       .= rmDate
    , "revision"   .= rmRevision
    , "locli"      .= rmLocliVersion
    , "target"     .= rmTarget
    ]

getReport :: Version -> Maybe Revision -> IO ReportMeta
getReport rmTarget mrev = do
  rmAuthor <- getGecosFullUsername
              `catch`
              \(_ :: SomeException) ->
                 getFallbackUserId
  rmDate <- getCurrentTime
  let rmRevision = fromMaybe (Revision 1) mrev
      rmLocliVersion = getLocliVersion
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

instance ToJSON Workload where
  toJSON = \case
    WValue               -> "value-only"
    WPlutusLoopCountdown -> "Plutus countdown loop"
    WPlutusLoopSECP      -> "Plutus SECP loop"

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
liftTmplRun Summary{sumWorkload=GeneratorProfile{..}
                   ,sumMeta=meta@Metadata{..}} =
  TmplRun
  { trMeta      = meta
  , trManifest  = manifest & unsafeShortenManifest 5
  , trWorkload  =
    case ( plutusMode       & fromMaybe False
         , plutusLoopScript & fromMaybe "" & FS.takeFileName & FS.dropExtension ) of
         (False, _)                       -> WValue
         (True, "loop")                   -> WPlutusLoopCountdown
         (True, "schnorr-secp256k1-loop") -> WPlutusLoopSECP
         (_, scr) ->
           error $ "Unknown Plutus script:  " <> scr
  }

data TmplRun
  = TmplRun
    { trMeta         :: !Metadata
    , trWorkload     :: !Workload
    , trManifest     :: !Manifest
    }

instance ToJSON TmplRun where
  toJSON TmplRun{trManifest=Manifest{..},..} =
    object
      [ "meta"       .= trMeta
      , "workload"   .= trWorkload
      , "branch"     .= mNodeBranch
      , "ver"        .= ident trMeta
      , "rev"        .=
        object
        [ "node"         .= mNode
        , "network"      .= mNetwork
        , "ledger"       .= mLedger
        , "plutus"       .= mPlutus
        , "crypto"       .= mCrypto
        , "base"         .= mBase
        , "prelude"      .= mPrelude
        ]
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

generate :: InputDir -> Maybe TextInputFile
         -> (SomeSummary, ClusterPerf, SomeBlockProp) -> [(SomeSummary, ClusterPerf, SomeBlockProp)]
         -> IO (ByteString, Text)
generate (InputDir ede) mReport (SomeSummary summ, cp, SomeBlockProp bp) rest = do
  ctx  <- getReport (last restTmpls & trManifest & mNodeApproxVer) Nothing
  tmplRaw <- BS.readFile (maybe defaultReportPath unTextInputFile mReport)
  tmpl <- parseWith defaultSyntax (includeFile ede) "report" tmplRaw
  result (error . show) (pure . (tmplRaw,) . LT.toStrict) $ tmpl >>=
    \x ->
      renderWith fenv x (env ctx baseTmpl restTmpls)
 where
   baseTmpl  =       liftTmplRun        summ
   restTmpls = fmap ((\(SomeSummary ss) -> liftTmplRun ss). fst3) rest

   defaultReportPath = ede <> "/report.ede"
   fenv = HM.fromList
     []
   onlyKeys :: [Text] -> Map.Map Text DictEntry -> [DictEntry]
   onlyKeys ks m =
     ks <&>
     \case
       (Nothing, k) -> error $ "Report.generate:  missing metric: " <> show k
       (Just x, _) -> x
     . (flip Map.lookup m &&& identity)
   env rc b rs = fromPairs
     [ "report"     .= rc
     , "base"       .= b
     , "runs"       .= rs
     , "summary"    .= liftTmplSection (summaryReportSection summ)
     , "analyses"   .= (liftTmplSection <$> analysesReportSections cp bp)
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
