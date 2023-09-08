{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.API.Types (module Cardano.Analysis.API.Types) where

import Cardano.Prelude          hiding (head)

import Data.Text                qualified as T
import Options.Applicative      qualified as Opt

import Data.CDF
import Data.Profile

import Cardano.Logging.Resources.Types

import Cardano.Unlog.LogObject
import Cardano.Util

import Cardano.Analysis.API.Chain
import Cardano.Analysis.API.ChainFilter
import Cardano.Analysis.API.Context
import Cardano.Analysis.API.Ground
import Cardano.Analysis.API.LocliVersion

--
-- * API types
--

-- | Overall analysis summary of a run.
data Summary f where
  Summary ::
    { sumAnalysisTime        :: !UTCTime
    , sumMeta                :: !Metadata
    , sumGenesis             :: !Genesis
    , sumGenesisSpec         :: !GenesisSpec
    , sumWorkload            :: !GeneratorProfile
    , sumHosts               :: !(f (Count Host))
    , sumLogObjectsTotal     :: !(f (Count LogObject))
    , sumFilters             :: !([FilterName], [ChainFilter])
    , sumChainRejectionStats :: ![(ChainFilter, Int)]
    , sumBlocksRejected      :: !(f (Count BlockEvents))
    , sumDomainTime          :: !(DataDomain f RUTCTime)
    , sumStartSpread         :: !(DataDomain f RUTCTime)
    , sumStopSpread          :: !(DataDomain f RUTCTime)
    , sumDomainSlots         :: !(DataDomain (CDF I) SlotNo)
    , sumDomainBlocks        :: !(DataDomain f BlockNo)
    , sumProfilingData       :: !(Maybe (ProfilingData (CDF I)))

    , cdfLogLinesEmitted     :: !(CDF f Int)
    , cdfLogObjectsEmitted   :: !(CDF f Int)
    , cdfLogObjects          :: !(CDF f Int)
    , cdfRuntime             :: !(CDF f NominalDiffTime)
    , cdfLogLineRate         :: !(CDF f Double)
    } -> Summary f
  deriving (Generic)

type SummaryOne   = Summary I
type MultiSummary = Summary (CDF I)
data SomeSummary  = forall f. KnownCDF f => SomeSummary (Summary f)

deriving instance (forall a. FromJSON a => FromJSON (f a)) => FromJSON (Summary f)
deriving instance (forall a.   ToJSON a =>   ToJSON (f a)) =>   ToJSON (Summary f)
deriving instance (forall a.   NFData a =>   NFData (f a)) =>   NFData (Summary f)
deriving instance (forall a.     Show a =>     Show (f a)) =>     Show (Summary f)

instance FromJSON SomeSummary where
  parseJSON x =
    (SomeSummary <$> parseJSON @SummaryOne   x)
    <|>
    (SomeSummary <$> parseJSON @MultiSummary x)
instance FromJSON SomeBlockProp where
  parseJSON x =
    (SomeBlockProp <$> parseJSON @BlockPropOne   x)
    <|>
    (SomeBlockProp <$> parseJSON @MultiBlockProp x)

data HostBlockStats
  = HostBlockStats
    { hbsFiltered  :: !(Count ForgerEvents)
    , hbsRejected  :: !(Count ForgerEvents)
    , hbsUnchained :: !(Count ForgerEvents)
    }
    deriving (Generic, FromJSON, ToJSON)

hbsTotal, hbsChained :: HostBlockStats -> Count ForgerEvents
hbsTotal   HostBlockStats{..} = hbsFiltered + hbsRejected + hbsUnchained
hbsChained HostBlockStats{..} = hbsFiltered + hbsRejected

-- | Results of block propagation analysis.
data BlockProp f
  = BlockProp
    { bpVersion              :: !Cardano.Analysis.API.LocliVersion.LocliVersion
    , bpDomainSlots          :: !(CDFList f (DataDomain I SlotNo))
    , bpDomainBlocks         :: !(CDFList f (DataDomain I BlockNo))
    , bpDomainCDFSlots       :: !(DataDomain f SlotNo)
    , bpDomainCDFBlocks      :: !(DataDomain f BlockNo)
    , cdfForgerStart         :: !(CDF f NominalDiffTime)
    , cdfForgerBlkCtx        :: !(CDF f NominalDiffTime)
    , cdfForgerLgrState      :: !(CDF f NominalDiffTime)
    , cdfForgerLgrView       :: !(CDF f NominalDiffTime)
    , cdfForgerLead          :: !(CDF f NominalDiffTime)
    , cdfForgerTicked        :: !(CDF f NominalDiffTime)
    , cdfForgerMemSnap       :: !(CDF f NominalDiffTime)
    , cdfForgerForge         :: !(CDF f NominalDiffTime)
    , cdfForgerAnnounce      :: !(CDF f NominalDiffTime)
    , cdfForgerAdoption      :: !(CDF f NominalDiffTime)
    , cdfForgerSend          :: !(CDF f NominalDiffTime)
    , cdfForgerAnnounceCum   :: !(CDF f NominalDiffTime)
    , cdfPeerNoticeFirst     :: !(CDF f NominalDiffTime)
    , cdfPeerFetchFirst      :: !(CDF f NominalDiffTime)
    , cdfPeerRequest         :: !(CDF f NominalDiffTime)
    , cdfPeerFetch           :: !(CDF f NominalDiffTime)
    , cdfPeerAnnounce        :: !(CDF f NominalDiffTime)
    , cdfPeerAdoption        :: !(CDF f NominalDiffTime)
    , cdfPeerSend            :: !(CDF f NominalDiffTime)
    , cdfBlocksPerHost       :: !(CDF f Int)
    , cdfBlocksFilteredRatio :: !(CDF f Double)
    , cdfBlocksChainedRatio  :: !(CDF f Double)
    , cdfBlockBattle         :: !(CDF f Int)
    , cdfBlockSize           :: !(CDF f Int)
    , bpPropagation          :: !(Map Text (CDF f NominalDiffTime))
    }
  deriving (Generic)
deriving instance
  ( forall a. FromJSON a => FromJSON (f a)
  , FromJSON (CDFList f (DataDomain I SlotNo))
  , FromJSON (CDFList f (DataDomain I BlockNo))
  ) =>
  FromJSON (BlockProp f)
deriving instance
  ( forall a. ToJSON a => ToJSON (f a)
  , ToJSON (CDFList f (DataDomain I SlotNo))
  , ToJSON (CDFList f (DataDomain I BlockNo))
  ) =>
  ToJSON (BlockProp f)
deriving instance
  ( forall a. Show a => Show (f a)
  , Show (CDFList f (DataDomain I SlotNo))
  , Show (CDFList f (DataDomain I BlockNo))
  ) =>
  Show (BlockProp f)

type BlockPropOne   = BlockProp I
type MultiBlockProp = BlockProp (CDF I)
data SomeBlockProp  = forall f. KnownCDF f => SomeBlockProp (BlockProp f)

-- | The top-level representation of the machine timeline analysis results.
data MachPerf f
  = MachPerf
    { mpVersion            :: !Cardano.Analysis.API.LocliVersion.LocliVersion
    , mpDomainSlots        :: !(CDFList f (DataDomain I SlotNo))
    , mpDomainCDFSlots     :: !(DataDomain f SlotNo)
    , cdfHostSlots         :: !(CDF f Word64)
    -- distributions
    , cdfStarts            :: !(CDF f Word64)
    , cdfLeads             :: !(CDF f Word64)
    , cdfUtxo              :: !(CDF f Word64)
    , cdfDensity           :: !(CDF f Double)
    , cdfStarted           :: !(CDF f NominalDiffTime)
    , cdfBlkCtx            :: !(CDF f NominalDiffTime)
    , cdfLgrState          :: !(CDF f NominalDiffTime)
    , cdfLgrView           :: !(CDF f NominalDiffTime)
    , cdfLeading           :: !(CDF f NominalDiffTime)
    , cdfBlockGap          :: !(CDF f Word64)
    , cdfSpanLensCpu       :: !(CDF f Int)
    , cdfSpanLensCpuEpoch  :: !(CDF f Int)
    , cdfSpanLensCpuRwd    :: !(CDF f Int)
    , mpResourceCDFs       :: !(Resources (CDF f Word64))
    }
  deriving (Generic)

-- | One machine's performance
type    MachPerfOne  = MachPerf I

-- | Bunch'a machines performances
type    ClusterPerf  = MachPerf (CDF I)

-- | Bunch'a bunches'a machine performances.
--   Same as above, since we collapse [CDF I] into CDF I -- just with more statistical confidence.
newtype MultiClusterPerf
  = MultiClusterPerf { unMultiClusterPerf :: ClusterPerf }
  deriving newtype (FromJSON)

-- * BlockProp
--
data Chain
  = Chain
  { cDomSlots       :: !(DataDomain I SlotNo)
  , cDomBlocks      :: !(DataDomain I BlockNo)
  , cRejecta        :: ![BlockEvents]
  , cMainChain      :: ![BlockEvents]
  , cHostBlockStats :: !(Map Host HostBlockStats)
  }

-- | Block's events, as seen by its forger.
data ForgerEvents a
  =  ForgerEvents
  { bfeHost         :: !Host
  , bfeBlock        :: !Hash
  , bfeBlockPrev    :: !Hash
  , bfeBlockNo      :: !BlockNo
  , bfeSlotNo       :: !SlotNo
  , bfeSlotStart    :: !SlotStart
  , bfeEpochNo      :: !EpochNo
  , bfeBlockSize    :: !(SMaybe Int)
  , bfeStarted      :: !(SMaybe a)
  , bfeBlkCtx       :: !(SMaybe a)
  , bfeLgrState     :: !(SMaybe a)
  , bfeLgrView      :: !(SMaybe a)
  , bfeLeading      :: !(SMaybe a)
  , bfeTicked       :: !(SMaybe a)
  , bfeMemSnap      :: !(SMaybe a)
  , bfeForged       :: !(SMaybe a)
  , bfeAnnounced    :: !(SMaybe a)
  , bfeAnnouncedCum :: !(SMaybe a)
  , bfeSending      :: !(SMaybe a)
  , bfeAdopted      :: !(SMaybe a)
  , bfeChainDelta   :: !Int
  , bfeErrs         :: [BPError]
  }
  deriving (Generic, NFData, FromJSON, ToJSON, Show)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beBlock         :: !Hash
  , beBlockPrev     :: !Hash
  , beBlockNo       :: !BlockNo
  , beSlotNo        :: !SlotNo
  , beEpochNo       :: !EpochNo
  , beEpochSafeInt  :: !EpochSafeInt
  , beForge         :: !BlockForge
  , beObservations  :: ![BlockObservation]
  , beForks         :: !(Count BlockEvents)
  , bePropagation   :: !(CDF I NominalDiffTime)
                       -- ^ CDF of slot-start-to-adoptions on cluster
  , beOtherBlocks   :: ![Hash]
  , beErrors        :: ![BPError]
  , beAcceptance    :: ![(ChainFilter, Bool)]
                       -- ^ List of acceptance conditions,
                       --   affecting block's consideration for analysis.
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data BlockForge
  =  BlockForge
  { bfForger       :: !Host
  , bfSlotStart    :: !SlotStart
  , bfBlockGap     :: !NominalDiffTime -- ^ Since previous forge event
  , bfBlockSize    :: !Int             -- ^ Bytes
  , bfStarted      :: !NominalDiffTime -- ^ Since slot start
  , bfBlkCtx       :: !(SMaybe NominalDiffTime) -- ^ Since forge loop start
  , bfLgrState     :: !(SMaybe NominalDiffTime) -- ^ Since block context
  , bfLgrView      :: !(SMaybe NominalDiffTime) -- ^ Since ledger state
  , bfLeading      :: !NominalDiffTime -- ^ Since ledger view OR loop start
  , bfTicked       :: !(SMaybe NominalDiffTime) -- ^ Since leading
  , bfMemSnap      :: !(SMaybe NominalDiffTime) -- ^ Since ticked
  , bfForged       :: !NominalDiffTime -- ^ Since ticked OR loop start
  , bfAnnounced    :: !NominalDiffTime -- ^ Since forging
  , bfAnnouncedCum :: !NominalDiffTime -- ^ Since slot start
  , bfSending      :: !NominalDiffTime -- ^ Since announcement
  , bfAdopted      :: !NominalDiffTime -- ^ Since announcement
  , bfChainDelta   :: !Int             -- ^ ChainDelta during adoption
  }
  deriving (Generic, FromJSON, ToJSON, Show)

allBlockForgeTimes :: Monoid (f Text) =>
  (Text -> NominalDiffTime -> f Text) -> BlockForge -> f Text
allBlockForgeTimes  f BlockForge{..}
  =                 f "bfBlockGap"  bfBlockGap
  <>                f "bfStarted"   bfStarted
  <> smaybe mempty (f "bfBlkCtx")   bfBlkCtx
  <> smaybe mempty (f "bfLgrState") bfLgrState
  <> smaybe mempty (f "bfLgrView")  bfLgrView
  <>                f "bfLeading"   bfLeading
  <> smaybe mempty (f "bfTicked")   bfTicked
  <> smaybe mempty (f "bfMemSnap")  bfMemSnap
  <>                f "bfForged"    bfForged
  <>                f "bfAnnounced" bfAnnounced
  <>                f "bfAnnouncedCum" bfAnnouncedCum
  <>                f "bfSending"   bfSending
  <>                f "bfAdopted"   bfAdopted

data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !SlotStart
  , boNoticed    :: !NominalDiffTime          -- ^ Since slot start
  , boRequested  :: !NominalDiffTime          -- ^ Since noticing
  , boFetched    :: !NominalDiffTime          -- ^ Since requesting
  , boAnnounced  :: !(SMaybe NominalDiffTime) -- ^ Since fetching
  , boSending    :: !(SMaybe NominalDiffTime) -- ^ Since announcement
  , boAdopted    :: !(SMaybe NominalDiffTime) -- ^ Since announcement
  , boChainDelta :: !Int                      -- ^ ChainDelta during adoption
  , boErrorsCrit :: ![BPError]
  , boErrorsSoft :: ![BPError]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

allBlockObservationTimes :: (Monoid (f Text)) =>
  (Text -> NominalDiffTime -> f Text) -> BlockObservation -> f Text
allBlockObservationTimes f BlockObservation{..}
  =                 f "boNoticed"    boNoticed
  <>                f "boRequested"  boRequested
  <>                f "boFetched"    boFetched
  <> smaybe mempty (f "boAnnounced") boAnnounced
  <> smaybe mempty (f "boSending"  ) boSending
  <> smaybe mempty (f "boAdopted"  ) boAdopted

data BPError
  = BPError
  { eHost  :: !Host
  , eBlock :: !Hash
  , eLO    :: !(Maybe LogObject)
  , eDesc  :: !BPErrorKind
  }
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

data Phase
  = Notice
  | Request
  | Fetch
  | Forge
  | Acquire
  | Announce
  | Adopt
  | Send
  deriving (FromJSON, Eq, Generic, NFData, Ord, Show, ToJSON)

data BPErrorKind
  = BPEBefore                !Phase !Phase
  | BPEUnexpectedForObserver !Phase
  | BPEUnexpectedForForger   !Phase
  | BPEUnexpectedAsFirst     !Phase
  | BPENoBlocks
  | BPEDuplicateForge
  | BPEMissingPhase          !Phase
  | BPENegativePhase         !Phase !NominalDiffTime
  | BPEFork                  !Hash
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

-- | From SlotStats collection:
data RunScalars
  = RunScalars
  { rsElapsed       :: Maybe NominalDiffTime
  , rsSubmitted     :: Maybe Word64
  , rsThreadwiseTps :: Maybe [Double]
  }
  deriving stock Generic
  deriving anyclass NFData

-- * MachPerf / ClusterPef
--
deriving instance (forall a. FromJSON a => FromJSON (f a), FromJSON (CDFList f (DataDomain I SlotNo))) => FromJSON (MachPerf f)
deriving instance (forall a.   ToJSON a =>   ToJSON (f a),   ToJSON (CDFList f (DataDomain I SlotNo))) =>   ToJSON (MachPerf f)
deriving instance (forall a.   NFData a =>   NFData (f a),   NFData (CDFList f (DataDomain I SlotNo))) =>   NFData (MachPerf f)
deriving instance (forall a.     Show a =>     Show (f a),     Show (CDFList f (DataDomain I SlotNo))) =>     Show (MachPerf f)

data SlotStats a
  = SlotStats
    { slSlot         :: !SlotNo
    , slEpoch        :: !EpochNo
    , slEpochSlot    :: !EpochSlot
    , slEpochSafeInt :: !EpochSafeInt
    , slStart        :: !SlotStart
    , slCountStarts  :: !Word64
    , slCountBlkCtx  :: !Word64
    , slCountLgrState :: !Word64
    , slCountLgrView :: !Word64
    , slCountLeads   :: !Word64
    , slCountForges  :: !Word64
    , slChainDBSnap  :: !Word64
    , slRejectedTx   :: !Word64
    , slBlockNo      :: !BlockNo
    , slBlockGap     :: !Word64
    , slStarted      :: !(SMaybe a)
    , slBlkCtx       :: !(SMaybe a)
    , slLgrState     :: !(SMaybe a)
    , slLgrView      :: !(SMaybe a)
    , slTicked       :: !(SMaybe a)
    , slMemSnap      :: !(SMaybe a)
    , slLeading      :: !(SMaybe a)
    , slForged       :: !(SMaybe a)
    , slMempoolTxs   :: !Word64
    , slSpanTxsMem   :: !(SMaybe NominalDiffTime)
    , slTxsCollected :: !Word64
    , slTxsAccepted  :: !Word64
    , slTxsRejected  :: !Word64
    , slUtxoSize     :: !Word64
    , slDensity      :: !Double
    , slResources    :: !(SMaybe (Resources Word64))
    , slLogObjects   :: ![LogObject]
    }
  deriving (Generic, Show, ToJSON)
  deriving anyclass NFData

--
-- * Key properties
--
testBlockEvents :: Genesis -> BlockEvents -> ChainFilter -> Bool
testBlockEvents g@Genesis{..}
                BlockEvents{beForge=forge@BlockForge{..}
                           ,beObservations=seen
                           ,..} = \case
  CBlock flt -> case flt of
    BUnitaryChainDelta -> bfChainDelta == 1
    BFullnessGEq f ->
      bfBlockSize > floor ((fromIntegral (maxBlockBodySize protocolParams) :: Double) * f)
    BFullnessLEq f ->
      bfBlockSize < floor ((fromIntegral (maxBlockBodySize protocolParams) :: Double) * f)
    BSizeGEq x -> bfBlockSize >= fromIntegral x
    BSizeLEq x -> bfBlockSize <= fromIntegral x
    BMinimumAdoptions x -> count (isSJust . boAdopted) seen >= fromIntegral x
    BNonNegatives -> null $
                 allBlockForgeTimes       noteFieldIfNeg forge <>
      concatMap (allBlockObservationTimes noteFieldIfNeg) seen
     where noteFieldIfNeg :: Text -> NominalDiffTime -> [Text]
           noteFieldIfNeg f x = [ f | x >= 0 ]
  CSlot flt -> case flt of
    SlotGEq s -> beSlotNo >= s
    SlotLEq s -> beSlotNo <= s
    EpochGEq e -> beEpochNo >= e
    EpochLEq e -> beEpochNo <= e
    SlotHasLeaders -> True
    EpochSafeIntGEq i -> beEpochSafeInt >= i
    EpochSafeIntLEq i -> beEpochSafeInt <= i
    EpSlotGEq s -> snd (g `unsafeParseSlot` beSlotNo) >= s
    EpSlotLEq s -> snd (g `unsafeParseSlot` beSlotNo) <= s

isValidBlockObservation :: BlockObservation -> Bool
isValidBlockObservation BlockObservation{..} =
  -- 1. All phases are present
  null boErrorsCrit
  &&
  -- 2. All timings account for processing of a single block
  boChainDelta == 1

testSlotStats :: Genesis -> SlotStats a -> SlotCond -> Bool
testSlotStats g SlotStats{..} = \case
  SlotGEq  s -> slSlot >= s
  SlotLEq  s -> slSlot <= s
  EpochGEq s -> fromIntegral (unEpochNo slEpoch) >= s
  EpochLEq s -> fromIntegral (unEpochNo slEpoch) <= s
  SlotHasLeaders -> slCountLeads > 0
  EpochSafeIntGEq i -> slEpochSafeInt >= i
  EpochSafeIntLEq i -> slEpochSafeInt <= i
  EpSlotGEq s -> snd (g `unsafeParseSlot` slSlot) >= s
  EpSlotLEq s -> snd (g `unsafeParseSlot` slSlot) <= s

--
-- * Block propagation report subsetting
--
data PropSubset
  = PropFull
  | PropControl
  | PropForger
  | PropPeers
  | PropEndToEnd
  | PropEndToEndBrief
  deriving Show

parsePropSubset :: Opt.Parser PropSubset
parsePropSubset =
  [ Opt.flag' PropFull          (Opt.long "full"       <> Opt.help "Complete propagation data")
  , Opt.flag' PropControl       (Opt.long "control"    <> Opt.help "Only overall control data")
  , Opt.flag' PropForger        (Opt.long "forger"     <> Opt.help "Only forger propagation")
  , Opt.flag' PropPeers         (Opt.long "peers"      <> Opt.help "Only peer propagation")
  , Opt.flag' PropEndToEnd      (Opt.long "end-to-end" <> Opt.help "Only end-to-end propagation")
  , Opt.flag' PropEndToEndBrief (Opt.long "e2e-brief"  <> Opt.help "Only brief end-to-end propagation")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world, begone. 0"

--
-- * Timeline rendering instances
--
renderAdoptionCentile :: Centile -> Text
renderAdoptionCentile = T.pack . printf "cdf%0.2f" . unCentile

adoptionCentiles :: [Centile]
adoptionCentiles =
  [ Centile 0.5, Centile 0.8, Centile 0.9
  , Centile 0.92, Centile 0.94, Centile 0.96, Centile 0.98, Centile 1.0 ]

adoptionCentilesBrief :: [Centile]
adoptionCentilesBrief =
  [ Centile 0.5, Centile 0.9, Centile 0.96 ]

--
-- * Machine performance report subsetting
--
data PerfSubset
  = PerfFull
  | PerfReport
  deriving Show

parsePerfSubset :: Opt.Parser PerfSubset
parsePerfSubset =
  [ Opt.flag' PerfFull   (Opt.long "full"   <> Opt.help "Complete performance data")
  , Opt.flag' PerfReport (Opt.long "report" <> Opt.help "Only report-relevant perf data")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."
