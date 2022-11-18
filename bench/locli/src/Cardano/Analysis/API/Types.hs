{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
module Cardano.Analysis.API.Types (module Cardano.Analysis.API.Types) where

import Util                     (count)
import Cardano.Prelude          hiding (head)

import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Text                qualified as T
import Options.Applicative      qualified as Opt

import Data.CDF

import Cardano.Logging.Resources.Types

import Cardano.Unlog.LogObject  hiding (Text)
import Cardano.Util

import Cardano.Analysis.API.Chain
import Cardano.Analysis.API.ChainFilter
import Cardano.Analysis.API.Context
import Cardano.Analysis.API.Field
import Cardano.Analysis.API.Ground
import Cardano.Analysis.API.LocliVersion

--
-- * API types
--

-- | Overall summary of all analyses.
data Summary where
  Summary ::
    { sumWhen                :: !UTCTime
    , sumLogStreams          :: !(Count [LogObject])
    , sumLogObjects          :: !(Count LogObject)
    , sumFilters             :: !([FilterName], [ChainFilter])
    , sumChainRejectionStats :: ![(ChainFilter, Int)]
    , sumBlocksRejected      :: !(Count BlockEvents)
    , sumDomainSlots         :: !(DataDomain SlotNo)
    , sumDomainBlocks        :: !(DataDomain BlockNo)
    } -> Summary
  deriving (Generic, FromJSON, ToJSON, Show)


-- | Results of block propagation analysis.
data BlockProp f
  = BlockProp
    { bpVersion              :: !Cardano.Analysis.API.LocliVersion.LocliVersion
    , bpDomainSlots          :: !(DataDomain SlotNo)
    , bpDomainBlocks         :: !(DataDomain BlockNo)
    , cdfForgerStarts        :: !(CDF f NominalDiffTime)
    , cdfForgerBlkCtx        :: !(CDF f NominalDiffTime)
    , cdfForgerLgrState      :: !(CDF f NominalDiffTime)
    , cdfForgerLgrView       :: !(CDF f NominalDiffTime)
    , cdfForgerLeads         :: !(CDF f NominalDiffTime)
    , cdfForgerForges        :: !(CDF f NominalDiffTime)
    , cdfForgerAnnouncements :: !(CDF f NominalDiffTime)
    , cdfForgerAdoptions     :: !(CDF f NominalDiffTime)
    , cdfForgerSends         :: !(CDF f NominalDiffTime)
    , cdfPeerNotices         :: !(CDF f NominalDiffTime)
    , cdfPeerRequests        :: !(CDF f NominalDiffTime)
    , cdfPeerFetches         :: !(CDF f NominalDiffTime)
    , cdfPeerAnnouncements   :: !(CDF f NominalDiffTime)
    , cdfPeerAdoptions       :: !(CDF f NominalDiffTime)
    , cdfPeerSends           :: !(CDF f NominalDiffTime)
    , cdfForks               :: !(CDF f Int)
    , cdfSizes               :: !(CDF f Int)
    , bpPropagation          :: !(Map Text (CDF f NominalDiffTime))
    }
  deriving (Generic)
deriving instance (Show     (f NominalDiffTime), Show     (f Int), Show     (f Double), Show     (f (Count BlockEvents))) => Show     (BlockProp f)
deriving instance (FromJSON (f NominalDiffTime), FromJSON (f Int), FromJSON (f Double), FromJSON (f (Count BlockEvents))) => FromJSON (BlockProp f)

type BlockPropOne   = BlockProp I
type MultiBlockProp = BlockProp (CDF I)

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
  , bfLeading      :: !NominalDiffTime -- ^ Since ledger view
  , bfForged       :: !NominalDiffTime -- ^ Since leading
  , bfAnnounced    :: !NominalDiffTime -- ^ Since forging
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
  <>                f "bfForged"    bfForged
  <>                f "bfAnnounced" bfAnnounced
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

-- | The top-level representation of the machine timeline analysis results.
data MachPerf f
  = MachPerf
    { mpVersion             :: !Cardano.Analysis.API.LocliVersion.LocliVersion
    , mpDomainSlots         :: !(DataDomain SlotNo)
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
    , cdfForged            :: !(CDF f NominalDiffTime)
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

deriving newtype instance FromJSON a => FromJSON (I a)
deriving newtype instance ToJSON   a => ToJSON   (I a)
deriving instance (FromJSON (a Double), FromJSON (a Int), FromJSON (a NominalDiffTime), FromJSON (a Word64)) => FromJSON (MachPerf a)
deriving instance (NFData   (a Double), NFData   (a Int), NFData   (a NominalDiffTime), NFData   (a Word64)) => NFData   (MachPerf a)
deriving instance (Show     (a Double), Show     (a Int),   Show   (a NominalDiffTime), Show     (a Word64)) => Show     (MachPerf a)

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
  | PropForger
  | PropPeers
  | PropEndToEnd
  | PropEndToEndBrief
  deriving Show

bpFieldSelectEndToEnd :: Field DSelect p a -> Bool
bpFieldSelectEndToEnd Field{fId} = fId `elem` adoptionCentilesRendered
 where
   adoptionCentilesRendered :: [Text]
   adoptionCentilesRendered = adoptionCentiles <&> renderAdoptionCentile

bpFieldSelectEndToEndBrief :: Field DSelect p a -> Bool
bpFieldSelectEndToEndBrief Field{fId} = fId `elem` adoptionCentilesRendered
 where
   adoptionCentilesRendered :: [Text]
   adoptionCentilesRendered = adoptionCentilesBrief <&> renderAdoptionCentile

propSubsetFn :: PropSubset -> (Field DSelect p a -> Bool)
propSubsetFn = \case
  PropFull          -> const True
  PropForger        -> bpFieldSelectForger
  PropPeers         -> bpFieldSelectPeers
  PropEndToEnd      -> bpFieldSelectEndToEnd
  PropEndToEndBrief -> bpFieldSelectEndToEndBrief

parsePropSubset :: Opt.Parser PropSubset
parsePropSubset =
  [ Opt.flag' PropFull          (Opt.long "full"       <> Opt.help "Complete propagation data")
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


bpFieldSelectForger :: Field DSelect p a -> Bool
bpFieldSelectForger Field{fId} = fId `elem`
  [ "cdfForgerStarts", "cdfForgerBlkCtx", "cdfForgerLgrState", "cdfForgerLgrView", "cdfForgerLeads", "cdfForgerForges", "cdfForgerAnnouncements", "cdfForgerSends", "cdfForgerAdoptions", "cdfForks" ]

bpFieldSelectPeers :: Field DSelect p a -> Bool
bpFieldSelectPeers Field{fId} = fId `elem`
  [ "cdfPeerNotices", "cdfPeerRequests", "cdfPeerFetches", "cdfPeerAnnouncements", "cdfPeerSends", "cdfPeerAdoptions" ]

--
-- * Machine performance report subsetting
--
data PerfSubset
  = PerfFull
  | PerfSummary
  deriving Show

parsePerfSubset :: Opt.Parser PerfSubset
parsePerfSubset =
  [ Opt.flag' PerfFull     (Opt.long "full"    <> Opt.help "Complete performance data")
  , Opt.flag' PerfSummary  (Opt.long "summary" <> Opt.help "Only report-relevant perf data")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."
