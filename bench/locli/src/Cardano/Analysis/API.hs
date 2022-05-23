{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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

import Data.Distribution

--
-- * API types
--

-- | Results of block propagation analysis.
data BlockPropagation
  = BlockPropagation
    { bpVersion             :: !Version
    , bpDomainSlots         :: !(DataDomain SlotNo)
    , bpDomainBlocks        :: !(DataDomain BlockNo)
    , bpForgerChecks        :: !(Distribution Float NominalDiffTime)
    , bpForgerLeads         :: !(Distribution Float NominalDiffTime)
    , bpForgerForges        :: !(Distribution Float NominalDiffTime)
    , bpForgerAdoptions     :: !(Distribution Float NominalDiffTime)
    , bpForgerAnnouncements :: !(Distribution Float NominalDiffTime)
    , bpForgerSends         :: !(Distribution Float NominalDiffTime)
    , bpPeerNotices         :: !(Distribution Float NominalDiffTime)
    , bpPeerRequests        :: !(Distribution Float NominalDiffTime)
    , bpPeerFetches         :: !(Distribution Float NominalDiffTime)
    , bpPeerAdoptions       :: !(Distribution Float NominalDiffTime)
    , bpPeerAnnouncements   :: !(Distribution Float NominalDiffTime)
    , bpPeerSends           :: !(Distribution Float NominalDiffTime)
    , bpPropagation         :: ![(Float, Distribution Float NominalDiffTime)]
    , bpSizes               :: !(Distribution Float Int)
    }
  deriving (Generic, FromJSON, ToJSON, Show)

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
  , bePropagation  :: !(Distribution Float NominalDiffTime)
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

-- | The top-level representation of the machine timeline analysis results.
data MachTimeline' f
  = MachTimeline
    { sVersion                  :: !Version
    , sDomain                   :: !(f (DataDomain SlotNo))
    -- distributions
    , sMissDistrib              :: !(f (Distribution Float Float))
    , sLeadsDistrib             :: !(f (Distribution Float Word64))
    , sUtxoDistrib              :: !(f (Distribution Float Word64))
    , sDensityDistrib           :: !(f (Distribution Float Float))
    , sSpanCheckDistrib         :: !(f (Distribution Float NominalDiffTime))
    , sSpanLeadDistrib          :: !(f (Distribution Float NominalDiffTime))
    , sSpanForgeDistrib         :: !(f (Distribution Float NominalDiffTime))
    , sBlocklessDistrib         :: !(f (Distribution Float Word64))
    , sSpanLensCPU85Distrib     :: !(f (Distribution Float Int))
    , sSpanLensCPU85EBndDistrib :: !(f (Distribution Float Int))
    , sSpanLensCPU85RwdDistrib  :: !(f (Distribution Float Int))
    , sResourceDistribs         :: !(f (Resources (Distribution Float Word64)))
    }
  deriving (Generic)

type MachTimeline = MachTimeline' Identity
deriving instance NFData MachTimeline
deriving instance Show   MachTimeline
deriving instance ToJSON MachTimeline

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
    , slDensity      :: !Float
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
bpFieldsForger :: DField a -> Bool
bpFieldsForger Field{fId} = elem fId
  [ "fChecked", "fLeading", "fForged", "fAdopted", "fAnnounced", "fSendStart" ]

bpFieldsPeers :: DField a -> Bool
bpFieldsPeers Field{fId} = elem fId
  [ "noticedVal", "requestedVal", "fetchedVal", "pAdoptedVal", "pAnnouncedVal", "pSendStartVal" ]

bpFieldsPropagation :: DField a -> Bool
bpFieldsPropagation Field{fHead2} = elem fHead2
  [ "0.50", "0.80", "0.90", "0.92", "0.94", "0.96", "0.98", "1.00" ]

instance RenderDistributions BlockPropagation where
  rdFields _ =
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
                         else error $ printf "Percspec mismatch: [%d]: exp=%f act=%f" i ps ps')
                      . fromMaybe
                        (error $ printf "No percentile %d/%f in bpPropagation." i ps)
                      . flip atMay i . bpPropagation))
    | (i, Perc ps) <- zip [0::Int ..] (adoptionPcts <> [Perc 1.0]) ] ++
    [ Field 9 0 "sizes"         "Size"  "bytes" $ DInt    bpSizes
    ]
   where
     f = nChunksEachOf 6    7 "--- Forger event Δt: ---"
     p = nChunksEachOf 6    6 "--- Peer event Δt: ---"
     r = nChunksEachOf aLen 6 "Slot-rel. Δt to adoption centile:"
     aLen = length adoptionPcts + 1 -- +1 is for the implied 1.0 percentile

adoptionPcts :: [PercSpec Float]
adoptionPcts =
  [ Perc 0.5, Perc 0.8, Perc 0.9, Perc 0.92, Perc 0.94, Perc 0.96, Perc 0.98 ]

instance RenderTimeline BlockEvents where
  rtFields _ =
    --  Width LeftPad
    [ Field 5 0 "block"        "block" "no."    $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"     "abs."  "slot#"  $ IWord64 (unSlotNo  . beSlotNo)
    , Field 6 0 "hash"         "block" "hash"   $ IText   (shortHash . beBlock)
    , Field 6 0 "hashPrev"     "prev"  "hash"   $ IText   (shortHash . beBlockPrev)
    , Field 7 0 "forger"       "forger" "host"  $ IText  (toText . unHost . bfForger . beForge)
    , Field 9 0 "blockSize"    "size"  "bytes"  $ IInt    (bfBlockSize . beForge)
    , Field 7 0 "blockGap"     "block" "gap"    $ IDeltaT (bfBlockGap  . beForge)
    , Field 3 0 "forks"         "for"  "-ks"    $ IInt   (count bpeIsFork . beErrors)
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

     percSpec :: Float -> Distribution Float NominalDiffTime -> NominalDiffTime
     percSpec ps d = dPercSpec (Perc ps) d
       & fromMaybe (error $ printf "No percentile %f in distribution." ps)
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

mtFieldsReport :: DField a -> Bool
mtFieldsReport Field{fId} = elem fId
  [ "CPU", "GC", "MUT", "RSS", "Heap", "Live", "Alloc" ]

instance RenderDistributions MachTimeline where
  rdFields _ =
    --  Width LeftPad
    [ Field 4 0 "missR"    "Miss"  "ratio"  $ DFloat   (i . sMissDistrib)
    , Field 5 0 "CheckΔ"   (d!!0)  "Check"  $ DDeltaT  (i . sSpanCheckDistrib)
    , Field 5 0 "LeadΔ"    (d!!1)  "Lead"   $ DDeltaT  (i . sSpanLeadDistrib)
    , Field 5 0 "ForgeΔ"   (d!!2)  "Forge"  $ DDeltaT  (i . sSpanForgeDistrib)
    , Field 4 0 "BlkGap"   "Block" "gap"    $ DWord64  (i . sBlocklessDistrib)
    , Field 5 0 "chDensity" "Dens" "ity"    $ DFloat   (i . sDensityDistrib)
    , Field 3 0 "CPU"      "CPU"   "%"      $ DWord64 (rCentiCpu .i. sResourceDistribs)
    , Field 3 0 "GC"       "GC"    "%"      $ DWord64 (rCentiGC .i. sResourceDistribs)
    , Field 3 0 "MUT"      "MUT"    "%"     $ DWord64 (fmap (min 999) . rCentiMut .i. sResourceDistribs)
    , Field 3 0 "GcMaj"    "GC "   "Maj"    $ DWord64 (rGcsMajor .i. sResourceDistribs)
    , Field 3 0 "GcMin"    "flt "  "Min"    $ DWord64 (rGcsMinor .i. sResourceDistribs)
    , Field 5 0 "RSS"      (m!!0)  "RSS"    $ DWord64 (rRSS .i. sResourceDistribs)
    , Field 5 0 "Heap"     (m!!1)  "Heap"   $ DWord64 (rHeap .i. sResourceDistribs)
    , Field 5 0 "Live"     (m!!2)  "Live"   $ DWord64 (rLive .i. sResourceDistribs)
    , Field 5 0 "Allocd"   "Alloc" "MB"     $ DWord64 (rAlloc .i. sResourceDistribs)
    , Field 5 0 "CPU85%LensAll"  (c!!0) "All"   $ DInt     (i . sSpanLensCPU85Distrib)
    , Field 5 0 "CPU85%LensEBnd" (c!!1) "EBnd"  $ DInt     (i . sSpanLensCPU85EBndDistrib)
    ]
   where
     i = runIdentity
     d = nChunksEachOf  3 6 "---- Δt ----"
     m = nChunksEachOf  3 6 "Memory usage, MB"
     c = nChunksEachOf  2 6 "CPU85% spans"

instance RenderTimeline SlotStats where
  rtFields _ =
    --  Width LeftPad
    [ Field 5 0 "abs.slot"     "abs."  "slot#"   $ IWord64 (unSlotNo . slSlot)
    , Field 4 0 "slot"         "  epo" "slot"    $ IWord64 (unEpochSlot . slEpochSlot)
    , Field 2 0 "epoch"        "ch "   "#"       $ IWord64 (unEpochNo . slEpoch)
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
                              fromIntegral <$> rCentiMut slResources :: Maybe Float)
          <*> (fromIntegral <$> rCentiCpu slResources))
    , Field 5 0 "rssMB"       (m 5!!0) "RSS"     $ IText (d 5.rRSS  .slResources)
    , Field 5 0 "heapMB"      (m 5!!1) "Heap"    $ IText (d 5.rHeap .slResources)
    , Field 5 0 "liveMB"      (m 5!!2) "Live"    $ IText (d 5.rLive .slResources)
    , Field 5 0 "allocatedMB"  "Allocd" "MB"     $ IText (d 5.rAlloc.slResources)
    , Field 6 0 "allocMut"     "Alloc/" "mutSec" $ IText
      (\SlotStats{..}->
          d 5 $
          (ceiling :: Float -> Int)
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

     calcProd :: Float -> Float -> Float
     calcProd mut' cpu' = if cpu' == 0 then 1 else mut' / cpu'
