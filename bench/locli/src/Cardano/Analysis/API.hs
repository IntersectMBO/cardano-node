{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.API
  ( module Cardano.Analysis.API
  , module Cardano.Util)
where

import Prelude                  ((!!), error)
import Cardano.Prelude          hiding (head)

import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Text   qualified as T
import Data.Text.Short          (toText)
import Data.Time.Clock          (NominalDiffTime)
import Text.Printf              (PrintfArg)

import Cardano.Analysis.Chain
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.Version
import Cardano.Logging.Resources.Types
import Cardano.Unlog.LogObject  hiding (Text)
import Cardano.Unlog.Render
import Cardano.Util

import Data.CDF

--
-- * API types
--

-- | Results of block propagation analysis.
data BlockProp f
  = BlockProp
    { bpVersion             :: !Version
    , bpDomainSlots         :: !(DataDomain SlotNo)
    , bpDomainBlocks        :: !(DataDomain BlockNo)
    , bpForgerChecks        :: !(CDF f NominalDiffTime)
    , bpForgerLeads         :: !(CDF f NominalDiffTime)
    , bpForgerForges        :: !(CDF f NominalDiffTime)
    , bpForgerAdoptions     :: !(CDF f NominalDiffTime)
    , bpForgerAnnouncements :: !(CDF f NominalDiffTime)
    , bpForgerSends         :: !(CDF f NominalDiffTime)
    , bpPeerNotices         :: !(CDF f NominalDiffTime)
    , bpPeerRequests        :: !(CDF f NominalDiffTime)
    , bpPeerFetches         :: !(CDF f NominalDiffTime)
    , bpPeerAdoptions       :: !(CDF f NominalDiffTime)
    , bpPeerAnnouncements   :: !(CDF f NominalDiffTime)
    , bpPeerSends           :: !(CDF f NominalDiffTime)
    , bpPropagation         :: ![(Double, CDF f NominalDiffTime)]
    , bpSizes               :: !(CDF f Int)
    }
  deriving (Generic)
deriving instance (Show     (f NominalDiffTime), Show     (f Int)) => Show     (BlockProp f)
deriving instance (FromJSON (f NominalDiffTime), FromJSON (f Int)) => FromJSON (BlockProp f)
deriving instance (ToJSON   (f NominalDiffTime), ToJSON   (f Int)) => ToJSON   (BlockProp f)

type BlockPropOne = BlockProp I
type BlockProps   = BlockProp (CDF I)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beEpochNo      :: !EpochNo
  , beEpochSafeInt :: !EpochSafeInt
  , beForge        :: !BlockForge
  , beObservations :: [BlockObservation]
  , bePropagation  :: !(DirectCDF NominalDiffTime)
                      -- ^ CDF of slot-start-to-adoptions on cluster
  , beOtherBlocks  :: [Hash]
  , beErrors       :: [BPError]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data BlockForge
  =  BlockForge
  { bfForger       :: !Host
  , bfSlotStart    :: !SlotStart
  , bfBlockGap     :: !NominalDiffTime -- ^ Since previous forge event
  , bfBlockSize    :: !Int             -- ^ Bytes
  , bfChecked      :: !NominalDiffTime -- ^ Since slot start
  , bfLeading      :: !NominalDiffTime -- ^ Since check
  , bfForged       :: !NominalDiffTime -- ^ Since leading
  , bfAdopted      :: !NominalDiffTime -- ^ Since forging
  , bfChainDelta   :: !Int             -- ^ ChainDelta during adoption
  , bfAnnounced    :: !NominalDiffTime -- ^ Since adoption
  , bfSending      :: !NominalDiffTime -- ^ Since announcement
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !SlotStart
  , boNoticed    :: !NominalDiffTime         -- ^ Since slot start
  , boRequested  :: !NominalDiffTime         -- ^ Since noticing
  , boFetched    :: !NominalDiffTime         -- ^ Since requesting
  , boAdopted    :: !(Maybe NominalDiffTime) -- ^ Since fetching
  , boChainDelta :: !Int                     -- ^ ChainDelta during adoption
  , boAnnounced  :: !(Maybe NominalDiffTime) -- ^ Since adoption
  , boSending    :: !(Maybe NominalDiffTime) -- ^ Since announcement
  , boErrorsCrit :: [BPError]
  , boErrorsSoft :: [BPError]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

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
  | Adopt
  | Announce
  | Send
  deriving (FromJSON, Eq, Generic, NFData, Ord, Show, ToJSON)

data BPErrorKind
  = BPEBefore                !Phase !Phase
  | BPEUnexpectedForObserver !Phase
  | BPEUnexpectedForForger   !Phase
  | BPEUnexpectedAsFirst     !Phase
  | BPEDuplicateForge
  | BPEMissingPhase          !Phase
  | BPENegativePhase         !Phase !NominalDiffTime
  | BPEFork                  !Hash
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

data DataDomain a
  = DataDomain
    { ddRawFirst      :: !a
    , ddRawLast       :: !a
    , ddFilteredFirst :: !a
    , ddFilteredLast  :: !a
    , ddRawCount      :: Int
    , ddFilteredCount :: Int
    }
  deriving (Generic, Show, ToJSON, FromJSON)
  deriving anyclass NFData
-- Perhaps:  Plutus.V1.Ledger.Slot.SlotRange = Interval Slot

mkDataDomainInj :: a -> a -> (a -> Int) -> DataDomain a
mkDataDomainInj f l measure = DataDomain f l f l delta delta
  where delta = measure l - measure f

mkDataDomain :: a -> a -> a -> a -> (a -> Int) -> DataDomain a
mkDataDomain f l f' l' measure =
  DataDomain f l f' l' (measure l - measure f) (measure l' - measure f')

dataDomainsMergeInner :: Ord a => [DataDomain a] -> DataDomain a
dataDomainsMergeInner xs =
  DataDomain
  { ddRawFirst      = maximum $ xs <&> ddRawFirst
  , ddRawLast       = minimum $ xs <&> ddRawLast
  , ddFilteredFirst = maximum $ xs <&> ddFilteredFirst
  , ddFilteredLast  = minimum $ xs <&> ddFilteredLast
  , ddRawCount      =     sum $ xs <&> ddRawCount
  , ddFilteredCount =     sum $ xs <&> ddFilteredCount
  }

dataDomainsMergeOuter :: Ord a => [DataDomain a] -> DataDomain a
dataDomainsMergeOuter xs =
  DataDomain
  { ddRawFirst      = minimum $ xs <&> ddRawFirst
  , ddRawLast       = maximum $ xs <&> ddRawLast
  , ddFilteredFirst = minimum $ xs <&> ddFilteredFirst
  , ddFilteredLast  = maximum $ xs <&> ddFilteredLast
  , ddRawCount      =     sum $ xs <&> ddRawCount
  , ddFilteredCount =     sum $ xs <&> ddFilteredCount
  }

-- | The top-level representation of the machine timeline analysis results.
data MachPerf f
  = MachPerf
    { sVersion              :: !Version
    , sDomainSlots          :: !(DataDomain SlotNo)
    -- distributions
    , sMissCDF              :: !(CDF f Double)
    , sLeadsCDF             :: !(CDF f Word64)
    , sUtxoCDF              :: !(CDF f Word64)
    , sDensityCDF           :: !(CDF f Double)
    , sSpanCheckCDF         :: !(CDF f NominalDiffTime)
    , sSpanLeadCDF          :: !(CDF f NominalDiffTime)
    , sSpanForgeCDF         :: !(CDF f NominalDiffTime)
    , sBlocklessCDF         :: !(CDF f Word64)
    , sSpanLensCpuCDF       :: !(CDF f Int)
    , sSpanLensCpuEpochCDF  :: !(CDF f Int)
    , sSpanLensCpuRwdCDF    :: !(CDF f Int)
    , sResourceCDFs         :: !(Resources (CDF f Word64))
    }
  deriving (Generic)

-- | One machine's performance
type    MachPerfOne  = MachPerf I

-- | Bunch'a machines performances
type    ClusterPerf  = MachPerf (CDF I)

-- | Bunch'a bunches'a machine performances.
--   Same as above, since we collapse [CDF I] into CDF I -- just with more statistical confidence.
newtype ClusterPerfs
  = ClusterPerfs { unClusterPerfs :: ClusterPerf }
  deriving newtype (ToJSON, FromJSON)

deriving newtype instance FromJSON a => FromJSON (I a)
deriving newtype instance ToJSON   a => ToJSON   (I a)
deriving instance (FromJSON (a Double), FromJSON (a Int), FromJSON (a NominalDiffTime), FromJSON (a Word64)) => FromJSON (MachPerf a)
deriving instance (NFData   (a Double), NFData   (a Int), NFData   (a NominalDiffTime), NFData   (a Word64)) => NFData   (MachPerf a)
deriving instance (Show     (a Double), Show     (a Int),   Show   (a NominalDiffTime), Show     (a Word64)) => Show     (MachPerf a)
deriving instance (ToJSON   (a Double), ToJSON   (a Int), ToJSON   (a NominalDiffTime), ToJSON   (a Word64)) => ToJSON   (MachPerf a)

data SlotStats
  = SlotStats
    { slSlot         :: !SlotNo
    , slEpoch        :: !EpochNo
    , slEpochSlot    :: !EpochSlot
    , slEpochSafeInt :: !EpochSafeInt
    , slStart        :: !SlotStart
    , slCountChecks  :: !Word64
    , slCountLeads   :: !Word64
    , slCountForges  :: !Word64
    , slChainDBSnap  :: !Word64
    , slRejectedTx   :: !Word64
    , slBlockNo      :: !Word64
    , slBlockless    :: !Word64
    , slSpanCheck    :: !(StrictMaybe NominalDiffTime)
    , slSpanLead     :: !(StrictMaybe NominalDiffTime)
    , slSpanForge    :: !(StrictMaybe NominalDiffTime)
    , slMempoolTxs   :: !Word64
    , slSpanTxsMem   :: !(StrictMaybe NominalDiffTime)
    , slTxsCollected :: !Word64
    , slTxsAccepted  :: !Word64
    , slTxsRejected  :: !Word64
    , slUtxoSize     :: !Word64
    , slDensity      :: !Double
    , slResources    :: !(Resources (Maybe Word64))
    }
  deriving (Generic, Show, ToJSON)
  deriving anyclass NFData

--
-- * Key properties
--
testBlockEvents :: Genesis -> BlockEvents -> ChainFilter -> Bool
testBlockEvents g@Genesis{..}
                BlockEvents{beForge=BlockForge{..},..} = \case
  CBlock flt -> case flt of
    BUnitaryChainDelta -> bfChainDelta == 1
    BFullnessGEq f ->
      bfBlockSize > floor ((fromIntegral (maxBlockBodySize protocolParams) :: Double) * f)
    BFullnessLEq f ->
      bfBlockSize < floor ((fromIntegral (maxBlockBodySize protocolParams) :: Double) * f)
    BSizeGEq x -> bfBlockSize >= fromIntegral x
    BSizeLEq x -> bfBlockSize <= fromIntegral x
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

isValidBlockEvent :: Genesis -> [ChainFilter] -> BlockEvents -> Bool
isValidBlockEvent g criteria be =
  all (testBlockEvents g be) criteria

isValidBlockObservation :: BlockObservation -> Bool
isValidBlockObservation BlockObservation{..} =
  -- 1. All phases are present
  null boErrorsCrit
  &&
  -- 2. All timings account for processing of a single block
  boChainDelta == 1

testSlotStats :: Genesis -> SlotStats -> SlotCond -> Bool
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
-- * Timeline rendering instances
--
bpFieldsForger :: Field DSelect p a -> Bool
bpFieldsForger Field{fId} = elem fId
  [ "fChecked", "fLeading", "fForged", "fAdopted", "fAnnounced", "fSendStart" ]

bpFieldsPeers :: Field DSelect p a -> Bool
bpFieldsPeers Field{fId} = elem fId
  [ "noticedVal", "requestedVal", "fetchedVal", "pAdoptedVal", "pAnnouncedVal", "pSendStartVal" ]

bpFieldsPropagation :: Field DSelect p a -> Bool
bpFieldsPropagation Field{fHead2} = elem fHead2
  [ "0.50", "0.80", "0.90", "0.92", "0.94", "0.96", "0.98", "1.00" ]

instance RenderCDFs BlockProp p where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 "fChecked"      (f!!0) "Checkd" $ DDeltaT bpForgerChecks
    , Field 6 0 "fLeading"      (f!!1) "Leadin" $ DDeltaT bpForgerLeads
    , Field 6 0 "fForged"       (f!!2) "Forge"  $ DDeltaT bpForgerForges
    , Field 6 0 "fAdopted"      (f!!3) "Adopt"  $ DDeltaT bpForgerAdoptions
    , Field 6 0 "fAnnounced"    (f!!4) "Announ" $ DDeltaT bpForgerAnnouncements
    , Field 6 0 "fSendStart"    (f!!5) "Sendin" $ DDeltaT bpForgerSends
    , Field 5 0 "noticedVal"    (p!!0) "Notic"  $ DDeltaT bpPeerNotices
    , Field 5 0 "requestedVal"  (p!!1) "Reque"  $ DDeltaT bpPeerRequests
    , Field 5 0 "fetchedVal"    (p!!2) "Fetch"  $ DDeltaT bpPeerFetches
    , Field 5 0 "pAdoptedVal"   (p!!3) "Adopt"  $ DDeltaT bpPeerAdoptions
    , Field 5 0 "pAnnouncedVal" (p!!4) "Annou"  $ DDeltaT bpPeerAnnouncements
    , Field 5 0 "pSendStartVal" (p!!5) "Send"   $ DDeltaT bpPeerSends
    ] ++
    [ Field 5 0 "propagation"   (r!!i)
            (T.take 4 $ T.pack $ printf "%.04f" ps)
            (DDeltaT ((\(ps', d) ->
                         if ps' == ps then d
                         else error $ printf "Centile mismatch: [%d]: exp=%f act=%f" i ps ps')
                      . fromMaybe
                        (error $ printf "No centile %d/%f in bpPropagation." i ps)
                      . flip atMay i . bpPropagation))
    | (i, Centile ps) <- zip [0::Int ..] (adoptionPcts <> [Centile 1.0]) ] ++
    [ Field 9 0 "sizes"         "Size"  "bytes" $ DInt    bpSizes
    ]
   where
     f = nChunksEachOf 6    7 "--- Forger event Δt: ---"
     p = nChunksEachOf 6    6 "--- Peer event Δt: ---"
     r = nChunksEachOf aLen 6 "Slot-rel. Δt to adoption centile:"
     aLen = length adoptionPcts + 1 -- +1 is for the implied 1.0 centile

adoptionPcts :: [Centile]
adoptionPcts =
  [ Centile 0.5, Centile 0.8, Centile 0.9, Centile 0.92, Centile 0.94, Centile 0.96, Centile 0.98 ]

instance RenderTimeline BlockEvents where
  rtFields _ =
    --  Width LeftPad
    [ Field 5 0 "block"        "block" "no."    $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"     "abs."  "slot#"  $ IWord64 (unSlotNo  . beSlotNo)
    , Field 6 0 "hash"         "block" "hash"   $ IText   (shortHash . beBlock)
    , Field 6 0 "hashPrev"     "prev"  "hash"   $ IText   (shortHash . beBlockPrev)
    , Field 7 0 "forger"       "forger" "host"  $ IText   (toText . unHost . bfForger . beForge)
    , Field 9 0 "blockSize"    "size"  "bytes"  $ IInt    (bfBlockSize . beForge)
    , Field 7 0 "blockGap"     "block" "gap"    $ IDeltaT (bfBlockGap  . beForge)
    , Field 3 0 "forks"         "for"  "-ks"    $ IInt    (count bpeIsFork . beErrors)
    , Field 6 0 "fChecked"      (f!!0) "Check"  $ IDeltaT (bfChecked   . beForge)
    , Field 6 0 "fLeading"      (f!!1) "Lead"   $ IDeltaT (bfLeading   . beForge)
    , Field 6 0 "fForged"       (f!!2) "Forge"  $ IDeltaT (bfForged    . beForge)
    , Field 6 0 "fAdopted"      (f!!3) "Adopt"  $ IDeltaT (bfAdopted   . beForge)
    , Field 6 0 "fAnnounced"    (f!!4) "Announ" $ IDeltaT (bfAnnounced . beForge)
    , Field 6 0 "fSendStart"    (f!!5) "Sendin" $ IDeltaT (bfSending   . beForge)
    , Field 5 0 "valid.observ" "valid" "obsrv"  $ IInt    (length          . valids)
    , Field 5 0 "noticedVal"    (p!!0) "Notic"  $ IDeltaT (af  boNoticed   . valids)
    , Field 5 0 "requestedVal"  (p!!1) "Requd"  $ IDeltaT (af  boRequested . valids)
    , Field 5 0 "fetchedVal"    (p!!2) "Fetch"  $ IDeltaT (af  boFetched   . valids)
    , Field 5 0 "pAdoptedVal"   (p!!3) "Adopt"  $ IDeltaT (af' boAdopted   . valids)
    , Field 5 0 "pAnnouncedVal" (p!!4) "Annou"  $ IDeltaT (af' boAnnounced . valids)
    , Field 5 0 "pSendStartVal" (p!!5) "Send"   $ IDeltaT (af' boSending   . valids)
    , Field 5 0 "pPropag0.5"    (r!!0) "0.5"    $ IDeltaT (percSpec 0.5  . bePropagation)
    , Field 5 0 "pPropag0.96"   (r!!1) "0.96"   $ IDeltaT (percSpec 0.96 . bePropagation)
    , Field 5 0 "pPropag1.0"    (r!!2) "1.0"    $ IDeltaT (percSpec 1.0  . bePropagation)
    , Field 5 0 "errors"        "all"  "errs"   $ IInt    (length . beErrors)
    , Field 3 0 "missAdopt"     (m!!0) "ado"    $ IInt    (count (bpeIsMissing Adopt) . beErrors)
    , Field 3 0 "missAnnou"     (m!!1) "ann"    $ IInt    (count (bpeIsMissing Announce) . beErrors)
    , Field 3 0 "missSend"      (m!!2) "snd"    $ IInt    (count (bpeIsMissing Send) . beErrors)
    , Field 3 0 "negAnnou"      (n!!0) "ann"    $ IInt    (count (bpeIsNegative Announce) . beErrors)
    , Field 3 0 "negSend"       (n!!1) "snd"    $ IInt    (count (bpeIsNegative Send) . beErrors)
    ]
   where
     valids = filter isValidBlockObservation . beObservations
     f = nChunksEachOf 6 7 "--- Forger event Δt: ---"
     p = nChunksEachOf 6 6 "Peer event Δt averages:"
     r = nChunksEachOf 3 6 "Propagation Δt:"
     m = nChunksEachOf 3 4 "Missing"
     n = nChunksEachOf 2 4 "Negative"

     percSpec :: Double -> DirectCDF NominalDiffTime -> NominalDiffTime
     percSpec ps d = unI $ Centile ps `projectCDF` d
       & fromMaybe (error $ printf "No centile %f in distribution." ps)
     af  f = avg . fmap f
     af' f = avg . mapMaybe f
     avg :: [NominalDiffTime] -> NominalDiffTime
     avg [] = 0
     avg xs =  (/ fromInteger (fromIntegral $ length xs)) $ sum xs
     count :: (a -> Bool) -> [a] -> Int
     count f = length . filter f

     bpeIsFork :: BPError -> Bool
     bpeIsFork BPError{eDesc=BPEFork{}} = True
     bpeIsFork _ = False

     bpeIsMissing, bpeIsNegative  :: Phase -> BPError -> Bool
     bpeIsMissing  p BPError{eDesc=BPEMissingPhase p'} = p == p'
     bpeIsMissing  _ _ = False
     bpeIsNegative p BPError{eDesc=BPENegativePhase p' _} = p == p'
     bpeIsNegative _ _ = False

  rtCommentary BlockEvents{..} = ("    " <>) . show <$> beErrors

mtFieldsReport :: Field DSelect p a -> Bool
mtFieldsReport Field{fId} = elem fId
  [ "CPU", "GC", "MUT", "RSS", "Heap", "Live", "Alloc" ]

instance RenderCDFs MachPerf p where
  rdFields =
    --  Width LeftPad
    [ Field 4 0 "missR"       "Miss"  "ratio" $ DFloat                 sMissCDF
    , Field 5 0 "CheckΔ"      (d!!0)  "Check" $ DDeltaT                sSpanCheckCDF
    , Field 5 0 "LeadΔ"       (d!!1)  "Lead"  $ DDeltaT                sSpanLeadCDF
    , Field 5 0 "ForgeΔ"      (d!!2)  "Forge" $ DDeltaT                sSpanForgeCDF
    , Field 4 0 "BlkGap"      "Block" "gap"   $ DWord64                sBlocklessCDF
    , Field 5 0 "chDensity"   "Dens"  "ity"   $ DFloat                 sDensityCDF
    , Field 3 0 "CPU"         "CPU"   "%"     $ DWord64 (rCentiCpu   . sResourceCDFs)
    , Field 3 0 "GC"          "GC"    "%"     $ DWord64 (rCentiGC    . sResourceCDFs)
    , Field 3 0 "MUT"         "MUT"   "%"     $ DWord64 (rCentiMut   . sResourceCDFs)
    , Field 3 0 "GcMaj"       "GC "   "Maj"   $ DWord64 (rGcsMajor   . sResourceCDFs)
    , Field 3 0 "GcMin"       "flt "  "Min"   $ DWord64 (rGcsMinor   . sResourceCDFs)
    , Field 5 0 "RSS"         (m!!0)  "RSS"   $ DWord64 (rRSS        . sResourceCDFs)
    , Field 5 0 "Heap"        (m!!1)  "Heap"  $ DWord64 (rHeap       . sResourceCDFs)
    , Field 5 0 "Live"        (m!!2)  "Live"  $ DWord64 (rLive       . sResourceCDFs)
    , Field 5 0 "Allocd"      "Alloc" "MB"    $ DWord64 (rAlloc      . sResourceCDFs)
    , Field 5 0 "CPULenAll"   (c!!0)  "All"   $ DInt                   sSpanLensCpuCDF
    , Field 5 0 "CPULenEpoch" (c!!1)  "Epoch" $ DInt                   sSpanLensCpuEpochCDF
    ]
   where
     d = nChunksEachOf  3 6 "---- Δt ----"
     m = nChunksEachOf  3 6 "Memory usage, MB"
     c = nChunksEachOf  2 6 "CPU% spans"

instance RenderTimeline SlotStats where
  rtFields _ =
    --  Width LeftPad
    [ Field 5 0 "abs.slot"     "abs."  "slot#"   $ IWord64 (unSlotNo       . slSlot)
    , Field 4 0 "slot"         "  epo" "slot"    $ IWord64 (unEpochSlot    . slEpochSlot)
    , Field 2 0 "epoch"        "ch "   "#"       $ IWord64 (unEpochNo      . slEpoch)
    , Field 3 0 "safetyInt"    "safe"  "int"     $ IWord64 (unEpochSafeInt . slEpochSafeInt)
    , Field 5 0 "block"        "block" "no."     $ IWord64 slBlockNo
    , Field 5 0 "blockGap"     "block" "gap"     $ IWord64 slBlockless
    , Field 3 0 "leadChecks"   "lead"  "chk"     $ IWord64 slCountChecks
    , Field 3 0 "leadShips"    "ship"  "win"     $ IWord64 slCountLeads
    , Field 3 0 "forges"       "For"   "ge"      $ IWord64 slCountForges
    , Field 4 0 "CDBSnap"      "CDB"   "snap"    $ IWord64 slChainDBSnap
    , Field 3 0 "rejTxs"       "rej"   "txs"     $ IWord64 slRejectedTx
    , Field 7 0 "checkSpan"    "check" "span"    $ IText (smaybe "" show.slSpanCheck)
    , Field 5 0 "leadSpan"     "lead"  "span"    $ IText (smaybe "" show.slSpanLead)
    , Field 5 0 "forgeSpan"    "forg"  "span"    $ IText (smaybe "" show.slSpanForge)
    , Field 4 0 "mempoolTxSpan" (t 4!!0) "span"  $ IText (smaybe "" show.slSpanTxsMem)
    , Field 4 0 "txsColl"     (t 4!!1) "cold"    $ IWord64 slTxsCollected
    , Field 4 0 "txsAcc"      (t 4!!2) "accd"    $ IWord64 slTxsAccepted
    , Field 4 0 "txsRej"      (t 4!!3) "rejd"    $ IWord64 slTxsRejected
    , Field 5 1 "chDensity"    "chain" "dens."   $ IFloat  slDensity
    , Field 3 0 "CPU%"        (c 3!!0) "all"     $ IText (d 3.rCentiCpu.slResources)
    , Field 3 0 "GC%"         (c 3!!1) "GC"      $ IText (d 3.fmap (min 999).rCentiGC.slResources)
    , Field 3 0 "MUT%"        (c 3!!2) "mut"     $ IText (d 3.fmap (min 999).rCentiMut.slResources)
    , Field 3 0 "majFlt"      (g 3!!0) "maj"     $ IText (d 3.rGcsMajor.slResources)
    , Field 3 0 "minFlt"      (g 3!!1) "min"     $ IText (d 3.rGcsMinor.slResources)
    , Field 6 0 "productiv"   "Produc" "tivity"  $ IText
      (\SlotStats{..}->
          f 4 $ calcProd <$> (min 6 . -- workaround for ghc-8.10.2
                              fromIntegral <$> rCentiMut slResources :: Maybe Double)
          <*> (fromIntegral <$> rCentiCpu slResources))
    , Field 5 0 "rssMB"       (m 5!!0) "RSS"     $ IText (d 5.rRSS  .slResources)
    , Field 5 0 "heapMB"      (m 5!!1) "Heap"    $ IText (d 5.rHeap .slResources)
    , Field 5 0 "liveMB"      (m 5!!2) "Live"    $ IText (d 5.rLive .slResources)
    , Field 5 0 "allocatedMB"  "Allocd" "MB"     $ IText (d 5.rAlloc.slResources)
    , Field 6 0 "allocMut"     "Alloc/" "mutSec" $ IText
      (\SlotStats{..}->
          d 5 $
          (ceiling :: Double -> Int)
          <$> ((/) <$> (fromIntegral . (100 *) <$> rAlloc slResources)
                <*> (fromIntegral . max 1 . (1024 *) <$> rCentiMut slResources)))
    , Field 7 0 "mempoolTxs"   "Mempool" "txs"   $ IWord64 slMempoolTxs
    , Field 9 0 "utxoEntries"  "UTxO"  "entries" $ IWord64 slUtxoSize
    -- , Field 10 0 "absSlotTime" "Absolute" "slot time" $ IText
    --   (\SlotStats{..}->
    --      T.pack $ " " `splitOn` show slStart !! 1)
    ]
   where
     t w = nChunksEachOf 4 (w + 1) "mempool tx"
     c w = nChunksEachOf 3 (w + 1) "%CPU"
     g w = nChunksEachOf 2 (w + 1) "GCs"
     m w = nChunksEachOf 3 (w + 1) "Memory use, MB"

     d, f :: PrintfArg a => Int -> Maybe a -> Text
     d width = \case
       Just x  -> T.pack $ printf ("%"<>"" --(if exportMode then "0" else "")
                                      <>show width<>"d") x
       Nothing -> mconcat (replicate width "-")
     f width = \case
       Just x  -> T.pack $ printf ("%0."<>show width<>"f") x
       Nothing -> mconcat (replicate width "-")

     calcProd :: Double -> Double -> Double
     calcProd mut' cpu' = if cpu' == 0 then 1 else mut' / cpu'
