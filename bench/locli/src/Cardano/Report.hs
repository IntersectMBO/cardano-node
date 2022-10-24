{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Report
  ( module Cardano.Report
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON (..), ToJSON (..), object)
import Data.ByteString qualified as BS
import Data.HashMap.Lazy qualified as HM
import Data.List (last)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time.Clock
import System.Posix.User

import Text.EDE

import Data.CDF

import Cardano.Analysis.API
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.Run hiding (Version)
import Cardano.Analysis.Run qualified as Run
import Cardano.Render


newtype Author   = Author   { unAuthor   :: Text } deriving newtype (FromJSON, ToJSON)
newtype Revision = Revision { unRevision :: Int }  deriving newtype (FromJSON, ToJSON)
newtype ShortId  = ShortId  { unShortId  :: Text } deriving newtype (FromJSON, ToJSON)

data Report
  = Report
    { rAuthor       :: !Author
    , rDate         :: !UTCTime
    , rRevision     :: !Revision
    , rLocliVersion :: !Run.Version
    , rTarget       :: !Version
    }
instance ToJSON Report where
  toJSON Report{..} = object
    [ "author"     .= rAuthor
    , "date"       .= rDate
    , "revision"   .= rRevision
    , "locli"      .= rLocliVersion
    , "target"     .= rTarget
    ]

getReport :: Version -> Maybe Revision -> IO Report
getReport rTarget mrev = do
  rAuthor <- (getUserEntryForName =<< getLoginName) <&> Author . T.pack . userGecos
  rDate <- getCurrentTime
  let rRevision = fromMaybe (Revision 1) mrev
      rLocliVersion = Run.getVersion
  pure Report{..}

data Workload
  = WValue
  | WPlutus

instance ToJSON Workload where
  toJSON = \case
    WValue  -> "Value"
    WPlutus -> "Plutus"

data Section where
  STable :: RenderCDFs a p =>
    { sData          :: !(a p)
    , sFieldSelector :: !(Field DSelect p a -> Bool)
    , sDataId        :: !Text
    , sOrgTableSrc   :: !Text
    , sTitle         :: !Text
    } -> Section

defaultReportSections :: MachPerf (CDF I) -> BlockProp I -> [Section]
defaultReportSections machPerf blockProp =
  [ STable machPerf  mtFieldsReport        "perf"    "clusterperf.report.org"
    "Resource Usage"

  , STable blockProp bpFieldSelectForger   "forge"   "blockprop.forger.org"
    "Forging"

  , STable blockProp bpFieldSelectPeers    "peers"   "blockprop.peers.org"
    "Individual peer propagation"

  , STable blockProp bpFieldSelectEndToEnd "end2end" "blockprop.endtoend.org"
    "End-to-end propagation"
  ]

--
-- Representation of a run, structured for template generator's needs.
--
data TmplRun
  = TmplRun
    { trMeta         :: !Metadata
    , trShortId      :: !ShortId
    , trWorkload     :: !Workload
    , trManifest     :: !Manifest
    }

instance ToJSON TmplRun where
  toJSON TmplRun{trManifest=Manifest{..},..} =
    object
      [ "meta"       .= trMeta
      , "id"         .= trShortId
      , "workload"   .= trWorkload
      , "branch"     .= mNodeBranch
      , "ver"        .= mNodeApproxVer
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

liftTmplRun :: Run -> TmplRun
liftTmplRun Run{..} =
  TmplRun
  { trMeta      = metadata
  , trShortId   = ShortId (batch metadata)
  , trWorkload  = WValue
  , trManifest  = manifest metadata & unsafeShortenManifest 5
  }

data TmplSection
  = TmplTable
    { tsTitle       :: !Text
    , tsDataId      :: !Text
    , tsOrgTableSrc :: !Text
    , tsNRows       :: !Int
    }

instance ToJSON TmplSection where
  toJSON TmplTable{..} = object
    [ "title"     .= tsTitle
    , "dataId"    .= tsDataId
    , "orgFile"   .= tsOrgTableSrc
    -- Yes, strange as it is, this is the encoding to ease iteration in ED-E.
    , "rows"      .= T.replicate tsNRows "."
    ]

liftTmplSection :: Section -> TmplSection
liftTmplSection =
  \case
    STable{..} ->
      TmplTable
      { tsTitle       = sTitle
      , tsDataId      = sDataId
      , tsOrgTableSrc = sOrgTableSrc
      , tsNRows       = length $ filterFields sFieldSelector
      }

generate :: InputDir -> Maybe TextInputFile
         -> (ClusterPerf, BlockPropOne, Run) -> [(ClusterPerf, BlockPropOne, Run)]
         -> IO (ByteString, Text)
generate (InputDir ede) mReport (cp, bp, base) rest = do
  ctx  <- getReport (last restTmpls & trManifest & mNodeApproxVer) Nothing
  tmplRaw <- BS.readFile (maybe defaultReportPath unTextInputFile mReport)
  tmpl <- parseWith defaultSyntax (includeFile ede) "report" tmplRaw
  result (error . show) (pure . (tmplRaw,) . LT.toStrict) $ tmpl >>=
    \x ->
      renderWith fenv x (env ctx baseTmpl restTmpls)
 where
   baseTmpl  =       liftTmplRun        base
   restTmpls = fmap (liftTmplRun. thd3) rest

   sections :: [Section]
   sections = defaultReportSections cp bp

   defaultReportPath = ede <> "/report.ede"
   fenv = HM.fromList
     []
   env rc b rs = fromPairs
     [ "report"   .= rc
     , "base"     .= b
     , "runs"     .= rs
     , "sections" .= (liftTmplSection <$> sections)
     ]
