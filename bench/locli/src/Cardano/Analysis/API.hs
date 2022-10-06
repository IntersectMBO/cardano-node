{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.API
  ( module Cardano.Analysis.API
  , module Cardano.Util)
where

import Prelude                  ((!!))
import Util                     (count)
import Cardano.Prelude          hiding (head)

import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Text                qualified as T
import Data.Text.Short          (toText)
import Data.Time.Clock          (NominalDiffTime)
import Options.Applicative      qualified as Opt
import Text.Printf              (PrintfArg)

import Data.CDF

import Cardano.Analysis.Chain
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.Version
import Cardano.Logging.Resources.Types
import Cardano.Render
import Cardano.Unlog.LogObject  hiding (Text)
import Cardano.Util

--
-- * API types
--

-- | Results of block propagation analysis.
data BlockProp f
  = BlockProp
    { bpVersion             :: !Cardano.Analysis.Version.Version
    , bpDomainSlots         :: !(DataDomain SlotNo)
    , bpDomainBlocks        :: !(DataDomain BlockNo)
    , bpForgerStarts        :: !(CDF f NominalDiffTime)
    , bpForgerBlkCtx        :: !(CDF f NominalDiffTime)
    , bpForgerLgrState      :: !(CDF f NominalDiffTime)
    , bpForgerLgrView       :: !(CDF f NominalDiffTime)
    , bpForgerLeads         :: !(CDF f NominalDiffTime)
    , bpForgerForges        :: !(CDF f NominalDiffTime)
    , bpForgerAnnouncements :: !(CDF f NominalDiffTime)
    , bpForgerAdoptions     :: !(CDF f NominalDiffTime)
    , bpForgerSends         :: !(CDF f NominalDiffTime)
    , bpPeerNotices         :: !(CDF f NominalDiffTime)
    , bpPeerRequests        :: !(CDF f NominalDiffTime)
    , bpPeerFetches         :: !(CDF f NominalDiffTime)
    , bpPeerAnnouncements   :: !(CDF f NominalDiffTime)
    , bpPeerAdoptions       :: !(CDF f NominalDiffTime)
    , bpPeerSends           :: !(CDF f NominalDiffTime)
    , bpPropagation         :: ![(Double, CDF f NominalDiffTime)]
    , bpSizes               :: !(CDF f Int)
    }
  deriving (Generic)
deriving instance (Show     (f NominalDiffTime), Show     (f Int)) => Show     (BlockProp f)
deriving instance (FromJSON (f NominalDiffTime), FromJSON (f Int)) => FromJSON (BlockProp f)
deriving instance (ToJSON   (f NominalDiffTime), ToJSON   (f Int)) => ToJSON   (BlockProp f)

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
  , beObservations  :: [BlockObservation]
  , bePropagation   :: !(CDF I NominalDiffTime)
                       -- ^ CDF of slot-start-to-adoptions on cluster
  , beOtherBlocks   :: [Hash]
  , beErrors        :: [BPError]
  , beNegAcceptance :: [ChainFilter] -- ^ List of negative acceptance conditions,
                                     --   preventing block's consideration for analysis.
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data BlockForge
  =  BlockForge
  { bfForger       :: !Host
  , bfSlotStart    :: !SlotStart
  , bfBlockGap     :: !NominalDiffTime -- ^ Since previous forge event
  , bfBlockSize    :: !Int             -- ^ Bytes
  , bfStarted      :: !NominalDiffTime -- ^ Since slot start
  , bfBlkCtx       :: !(Maybe NominalDiffTime) -- ^ Since forge loop start
  , bfLgrState     :: !(Maybe NominalDiffTime) -- ^ Since block context
  , bfLgrView      :: !(Maybe NominalDiffTime) -- ^ Since ledger state
  , bfLeading      :: !NominalDiffTime -- ^ Since ledger view
  , bfForged       :: !NominalDiffTime -- ^ Since leading
  , bfAnnounced    :: !NominalDiffTime -- ^ Since forging
  , bfSending      :: !NominalDiffTime -- ^ Since announcement
  , bfAdopted      :: !NominalDiffTime -- ^ Since announcement
  , bfChainDelta   :: !Int             -- ^ ChainDelta during adoption
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !SlotStart
  , boNoticed    :: !NominalDiffTime         -- ^ Since slot start
  , boRequested  :: !NominalDiffTime         -- ^ Since noticing
  , boFetched    :: !NominalDiffTime         -- ^ Since requesting
  , boAnnounced  :: !(Maybe NominalDiffTime) -- ^ Since fetching
  , boSending    :: !(Maybe NominalDiffTime) -- ^ Since announcement
  , boAdopted    :: !(Maybe NominalDiffTime) -- ^ Since announcement
  , boChainDelta :: !Int                     -- ^ ChainDelta during adoption
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

-- | The top-level representation of the machine timeline analysis results.
data MachPerf f
  = MachPerf
    { sVersion              :: !Cardano.Analysis.Version.Version
    , sDomainSlots          :: !(DataDomain SlotNo)
    -- distributions
    , sMissCDF              :: !(CDF f Double)
    , sLeadsCDF             :: !(CDF f Word64)
    , sUtxoCDF              :: !(CDF f Word64)
    , sDensityCDF           :: !(CDF f Double)
    , sStartedCDF           :: !(CDF f NominalDiffTime)
    , sBlkCtxCDF            :: !(CDF f NominalDiffTime)
    , sLgrStateCDF          :: !(CDF f NominalDiffTime)
    , sLgrViewCDF           :: !(CDF f NominalDiffTime)
    , sLeadingCDF           :: !(CDF f NominalDiffTime)
    , sForgedCDF            :: !(CDF f NominalDiffTime)
    , sBlockGapCDF          :: !(CDF f Word64)
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
newtype MultiClusterPerf
  = MultiClusterPerf { unMultiClusterPerf :: ClusterPerf }
  deriving newtype (ToJSON, FromJSON)

deriving newtype instance FromJSON a => FromJSON (I a)
deriving newtype instance ToJSON   a => ToJSON   (I a)
deriving instance (FromJSON (a Double), FromJSON (a Int), FromJSON (a NominalDiffTime), FromJSON (a Word64)) => FromJSON (MachPerf a)
deriving instance (NFData   (a Double), NFData   (a Int), NFData   (a NominalDiffTime), NFData   (a Word64)) => NFData   (MachPerf a)
deriving instance (Show     (a Double), Show     (a Int),   Show   (a NominalDiffTime), Show     (a Word64)) => Show     (MachPerf a)
deriving instance (ToJSON   (a Double), ToJSON   (a Int), ToJSON   (a NominalDiffTime), ToJSON   (a Word64)) => ToJSON   (MachPerf a)

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
    , slResources    :: !(Resources (Maybe Word64))
    }
  deriving (Generic, Show, ToJSON)
  deriving anyclass NFData

--
-- * Key properties
--
testBlockEvents :: Genesis -> BlockEvents -> ChainFilter -> Bool
testBlockEvents g@Genesis{..}
                BlockEvents{beForge=BlockForge{..}
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
    BMinimumAdoptions x -> count (isJust . boAdopted) seen >= fromIntegral x
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

bpFieldSelectForger :: Field DSelect p a -> Bool
bpFieldSelectForger Field{fId} = elem fId
  [ "fStarted", "fLeading", "fForged", "fAnnounced", "fAdopted", "fSendStart" ]

bpFieldSelectPeers :: Field DSelect p a -> Bool
bpFieldSelectPeers Field{fId} = elem fId
  [ "pNoticed", "pRequested", "pFetched", "pAnnounced", "pAdopted", "pSendStart" ]

bpFieldSelectEndToEnd :: Field DSelect p a -> Bool
bpFieldSelectEndToEnd Field{fHead2} = elem fHead2 adoptionCentilesRendered
 where
   adoptionCentilesRendered :: [Text]
   adoptionCentilesRendered = adoptionCentiles <&> T.drop 4 . renderAdoptionCentile

bpFieldSelectEndToEndBrief :: Field DSelect p a -> Bool
bpFieldSelectEndToEndBrief Field{fHead2} = elem fHead2 adoptionCentilesRendered
 where
   adoptionCentilesRendered :: [Text]
   adoptionCentilesRendered = adoptionCentilesBrief <&> T.drop 4 . renderAdoptionCentile

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

parseCDF2Aspect :: Opt.Parser CDF2Aspect
parseCDF2Aspect =
  [ Opt.flag' OfOverallDataset  (Opt.long "overall"    <> Opt.help "Overall dataset statistical summary")
  , Opt.flag' OfInterCDF        (Opt.long "inter-cdf"  <> Opt.help "Inter-sample (i.e. inter-CDF) stats")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world, begone. 1"

--
-- * Timeline rendering instances
--
renderAdoptionCentile :: Centile -> Text
renderAdoptionCentile = T.pack . printf "prop%0.2f" . unCentile

adoptionCentiles :: [Centile]
adoptionCentiles =
  [ Centile 0.5, Centile 0.8, Centile 0.9
  , Centile 0.92, Centile 0.94, Centile 0.96, Centile 0.98, Centile 1.0 ]

adoptionCentilesBrief :: [Centile]
adoptionCentilesBrief =
  [ Centile 0.5, Centile 0.9, Centile 0.96 ]

instance RenderCDFs BlockProp p where
  rdFields =
    --  Width LeftPad
    [ Field 4 0 "fStarted"      (f!!0) "Loop" (DDeltaT bpForgerStarts)        "Started forge loop iteration"
    , Field 4 0 "fBlkCtx"       (f!!1) "BkCt" (DDeltaT bpForgerBlkCtx)        "Acquired block context"
    , Field 4 0 "fLgrState"     (f!!2) "LgSt" (DDeltaT bpForgerLgrState)      "Acquired ledger state"
    , Field 4 0 "fLgrView"      (f!!3) "LgVi" (DDeltaT bpForgerLgrView)       "Acquired ledger view"
    , Field 4 0 "fLeading"      (f!!4) "Lead" (DDeltaT bpForgerLeads)         "Leadership check duration"
    , Field 4 0 "fForged"       (f!!5) "Forg" (DDeltaT bpForgerForges)        "Leadership to forged"
    , Field 4 0 "fAnnounced"    (f!!6) "Anno" (DDeltaT bpForgerAnnouncements) "Forged to announced"
    , Field 4 0 "fSendStart"    (f!!7) "Send" (DDeltaT bpForgerSends)         "Announced to sending"
    , Field 4 0 "fAdopted"      (f!!8) "Adop" (DDeltaT bpForgerAdoptions)     "Announced to self-adopted"
    , Field 4 0 "pNoticed"      (p!!0) "Noti" (DDeltaT bpPeerNotices)         "First peer notice"
    , Field 4 0 "pRequested"    (p!!1) "Requ" (DDeltaT bpPeerRequests)        "Notice to fetch request"
    , Field 4 0 "pFetched"      (p!!2) "Fetc" (DDeltaT bpPeerFetches)         "Fetch duration"
    , Field 4 0 "pAnnounced"    (p!!3) "Anno" (DDeltaT bpPeerAnnouncements)   "Fetched to announced"
    , Field 4 0 "pSendStart"    (p!!4) "Send" (DDeltaT bpPeerSends)           "Announced to sending"
    , Field 4 0 "pAdopted"      (p!!5) "Adop" (DDeltaT bpPeerAdoptions)       "Announced to adopted"
    ] ++
    [ Field 4 0 (renderAdoptionCentile ct)
                                (r!!i)
                                       (T.take 4 $ T.pack $ printf "%.04f" centi)
            (DDeltaT ((\(centi', d) ->
                         if centi' == centi then d
                         else error $ printf "Centile mismatch: [%d]: exp=%f act=%f"
                                             i centi centi')
                      . fromMaybe
                        (error $ printf "No centile %d/%f in bpPropagation." i centi)
                      . flip atMay i . bpPropagation))
            (T.pack $ printf "%.2f adoption" centi)
    | (i, ct@(Centile centi)) <- zip [0::Int ..] adoptionCentiles ] ++
    [ Field 9 0 "sizes"         "Size"  "bytes" (DInt    bpSizes) ""
    ]
   where
     f = nChunksEachOf 9    5 ",-------------------- Forger event Δt: --------------------."
     p = nChunksEachOf 6    5 ",------- Peer event Δt: -------."
     r = nChunksEachOf aLen 5 ",---- Slot-rel. Δt to adoption centile: ----."
     aLen = length adoptionCentiles

instance RenderTimeline BlockEvents where
  rtFields _ =
    --  Width LeftPad
    [ Field 5 0 "block"        "block" "no."    (IWord64 (unBlockNo . beBlockNo)) ""
    , Field 5 0 "abs.slot"     "abs."  "slot#"  (IWord64 (unSlotNo  . beSlotNo)) ""
    , Field 6 0 "hash"         "block" "hash"   (IText   (shortHash . beBlock)) ""
    , Field 6 0 "hashPrev"     "prev"  "hash"   (IText   (shortHash . beBlockPrev)) ""
    , Field 7 0 "forger"       "forger" "host"  (IText   (toText . unHost . bfForger . beForge)) ""
    , Field 6 0 "blockSize"    "size"  "bytes"  (IInt    (bfBlockSize . beForge)) ""
    , Field 5 0 "blockGap"     "block" "gap"    (IDeltaT (bfBlockGap  . beForge)) ""
    , Field 3 0 "forks"         "for"  "-ks"    (IInt    (count bpeIsFork . beErrors)) ""
    , Field 4 0 "fStarted"      (f!!0) "Start"  (IDeltaT (bfStarted   . beForge)) ""
    , Field 4 0 "fBlkCtx"       (f!!1) "BlkCtx" (IText (maybe "?" show.bfBlkCtx  .beForge)) ""
    , Field 4 0 "fLgrState"     (f!!2) "LgrSta" (IText (maybe "?" show.bfLgrState.beForge)) ""
    , Field 4 0 "fLgrView"      (f!!3) "LgrVie" (IText (maybe "?" show.bfLgrView .beForge)) ""
    , Field 4 0 "fLeading"      (f!!4) "Lead"   (IDeltaT (bfLeading   . beForge)) ""
    , Field 4 0 "fForged"       (f!!5) "Forge"  (IDeltaT (bfForged    . beForge)) ""
    , Field 4 0 "fAnnounced"    (f!!6) "Announ" (IDeltaT (bfAnnounced . beForge)) ""
    , Field 4 0 "fSendStart"    (f!!7) "Sendin" (IDeltaT (bfSending   . beForge)) ""
    , Field 4 0 "fAdopted"      (f!!8) "Adopt"  (IDeltaT (bfAdopted   . beForge)) ""
    , Field 4 0 "noticedVal"    (p!!0) "Notic"  (IDeltaT (af  boNoticed   . valids)) ""
    , Field 4 0 "requestedVal"  (p!!1) "Requd"  (IDeltaT (af  boRequested . valids)) ""
    , Field 4 0 "fetchedVal"    (p!!2) "Fetch"  (IDeltaT (af  boFetched   . valids)) ""
    , Field 4 0 "pAnnouncedVal" (p!!3) "Annou"  (IDeltaT (af' boAnnounced . valids)) ""
    , Field 4 0 "pSendStartVal" (p!!4) "Send"   (IDeltaT (af' boSending   . valids)) ""
    , Field 4 0 "pAdoptedVal"   (p!!5) "Adopt"  (IDeltaT (af' boAdopted   . valids)) ""
    , Field 4 0 "pPropag0.5"    (r!!0) "0.5"    (IDeltaT (percSpec 0.5  . bePropagation)) ""
    , Field 4 0 "pPropag0.8"    (r!!1) "0.8"    (IDeltaT (percSpec 0.8  . bePropagation)) ""
    , Field 4 0 "pPropag0.96"   (r!!2) "0.96"   (IDeltaT (percSpec 0.96 . bePropagation)) ""
    , Field 4 0 "pPropag1.0"    (r!!3) "1.0"    (IDeltaT (snd . cdfRange . bePropagation)) ""
    , Field 3 0 "valid"         "va-"  "lid"    (IText   (bool "-" "+" . (== 0) . length . beNegAcceptance)) ""
    , Field 3 0 "valid.observ" "good"  "obsv"   (IInt    (length          . valids)) ""
    , Field 5 0 "errors"        "all"  "errs"   (IInt    (length . beErrors)) ""
    , Field 3 0 "missNotic"     (m!!0) "ntc"    (IInt    (count (bpeIsMissing Notice) . beErrors)) ""
    , Field 3 0 "missReque"     (m!!1) "req"    (IInt    (count (bpeIsMissing Request) . beErrors)) ""
    , Field 3 0 "missFetch"     (m!!2) "fch"    (IInt    (count (bpeIsMissing Fetch) . beErrors)) ""
    , Field 3 0 "missAnnou"     (m!!3) "ann"    (IInt    (count (bpeIsMissing Announce) . beErrors)) ""
    , Field 3 0 "missAdopt"     (m!!4) "ado"    (IInt    (count (bpeIsMissing Adopt) . beErrors)) ""
    , Field 3 0 "missSend"      (m!!5) "snd"    (IInt    (count (bpeIsMissing Send) . beErrors)) ""
    , Field 3 0 "negAnnou"      (n!!0) "ann"    (IInt    (count (bpeIsNegative Announce) . beErrors)) ""
    , Field 3 0 "negSend"       (n!!1) "snd"    (IInt    (count (bpeIsNegative Send) . beErrors)) ""
    ]
   where
     valids = filter isValidBlockObservation . beObservations
     f = nChunksEachOf 9 5 "--------- Forger event Δt: ---------"
     p = nChunksEachOf 6 5 "-- Peer event Δt averages: --"
     r = nChunksEachOf 4 5 "Propagation Δt:"
     m = nChunksEachOf 6 4 "Missing"
     n = nChunksEachOf 2 4 "Negative"

     percSpec :: Double -> CDF I NominalDiffTime -> NominalDiffTime
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

  data RTComments BlockEvents
    = BEErrors
    | BEFilterOuts
    deriving Show

  rtCommentary BlockEvents{..} =
    \case
      BEErrors     -> ("           " <>) . show <$> beErrors
      BEFilterOuts -> ("           " <>) . show <$> beNegAcceptance

--
-- * Machine performance report subsetting
--
data PerfSubset
  = PerfFull
  | PerfSummary
  deriving Show

mtFieldsReport :: Field DSelect p a -> Bool
mtFieldsReport Field{fId} = elem fId
  [ "cpuProcess", "cpuGC", "cpuMutator", "cpuSpanLenAll", "memRSS", "rtsHeap", "rtsLive", "rtsAllocation" ]

perfSubsetFn :: PerfSubset -> (Field DSelect p a -> Bool)
perfSubsetFn = \case
  PerfFull    -> const True
  PerfSummary -> mtFieldsReport

parsePerfSubset :: Opt.Parser PerfSubset
parsePerfSubset =
  [ Opt.flag' PerfFull     (Opt.long "full"    <> Opt.help "Complete performance data")
  , Opt.flag' PerfSummary  (Opt.long "summary" <> Opt.help "Only report-relevant perf data")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world."

instance RenderCDFs MachPerf p where
  rdFields =
    --  Width LeftPad
    [ Field 4 0 "missRatio"     "Miss"  "ratio" (DFloat             sMissCDF)       "Leadership checks miss ratio"
    , Field 4 0 "checkΔ"        (d!!0)  "Start" (DDeltaT            sStartedCDF)  "Forge loop tardiness"
    , Field 4 0 "blkCtΔ"        (d!!1)  "BlkCt" (DDeltaT            sBlkCtxCDF)  "Block context acquisition delay"
    , Field 4 0 "lgrStΔ"        (d!!2)  "LgrSt" (DDeltaT            sLgrStateCDF)  "Ledger state acquisition delay"
    , Field 4 0 "lgrViΔ"        (d!!3)  "LgrVi" (DDeltaT            sLgrViewCDF)  "Ledger view acquisition delay"
    , Field 4 0 "leadΔ"         (d!!4)  "Lead"  (DDeltaT            sLeadingCDF)   "Leadership check duration"
    , Field 4 0 "forgeΔ"        (d!!5)  "Forge" (DDeltaT            sForgedCDF)  "Leading to block forged"
    , Field 4 0 "blockGap"      "Block" "gap"   (DWord64            sBlockGapCDF)  "Interblock gap"
    , Field 5 0 "chainDensity"  "Dens"  "ity"   (DFloat             sDensityCDF)    "Chain density"
    , Field 3 0 "cpuProcess"    "CPU"   "%"     (DWord64 (rCentiCpu.sResourceCDFs)) "Process CPU usage pct"
    , Field 3 0 "cpuGC"         "GC"    "%"     (DWord64 (rCentiGC .sResourceCDFs)) "RTS GC CPU usage pct"
    , Field 3 0 "cpuMutator"    "MUT"   "%"     (DWord64 (rCentiMut.sResourceCDFs)) "RTS Mutator CPU usage pct"
    , Field 3 0 "gcMajor"       "GC "   "Maj"   (DWord64 (rGcsMajor.sResourceCDFs)) "Major GCs Hz"
    , Field 3 0 "gcMinor"       "flt "  "Min"   (DWord64 (rGcsMinor.sResourceCDFs)) "Minor GCs Hz"
    , Field 5 0 "memRSS"        (m!!0)  "RSS"   (DWord64 (rRSS     .sResourceCDFs)) "Kernel RSS MB"
    , Field 5 0 "rtsHeap"       (m!!1)  "Heap"  (DWord64 (rHeap    .sResourceCDFs)) "RTS heap size MB"
    , Field 5 0 "rtsLiveBytes"  (m!!2)  "Live"  (DWord64 (rLive    .sResourceCDFs)) "RTS GC live bytes MB"
    , Field 5 0 "rtsAllocation" "Alloc" "MB"    (DWord64 (rAlloc   .sResourceCDFs)) "RTS alloc rate MB sec"
    , Field 5 0 "cpuSpanLenAll" (c!!0)  "All"   (DInt              sSpanLensCpuCDF) "CPU 85pct spans"
    , Field 5 0 "cpuSpanLenEp"  (c!!1)  "Epoch" (DInt         sSpanLensCpuEpochCDF) "CPU spans at Ep boundary"
    ]
   where
     d = nChunksEachOf  6 5 "----------- Δt -----------"
     m = nChunksEachOf  3 6 "Memory usage, MB"
     c = nChunksEachOf  2 6 "CPU% spans"

instance RenderTimeline (SlotStats NominalDiffTime) where
  data RTComments (SlotStats NominalDiffTime)
    deriving Show

  rtFields _ =
    --  Width LeftPad
    [ Field 5 0 "abs.slot"     "abs."  "slot#"   (IWord64 (unSlotNo      .slSlot)) ""
    , Field 4 0 "slot"         "  epo" "slot"    (IWord64 (unEpochSlot   .slEpochSlot)) ""
    , Field 2 0 "epoch"        "ch "   "#"       (IWord64 (unEpochNo     .slEpoch)) ""
    , Field 3 0 "safetyInt"    "safe"  "int"     (IWord64 (unEpochSafeInt.slEpochSafeInt)) ""
    , Field 5 0 "block"        "block" "no."     (IWord64 (unBlockNo.slBlockNo)) ""
    , Field 5 0 "blockGap"     "block" "gap"     (IWord64 slBlockGap) ""
    , Field 3 0 "forgeLoop"    "forg"  "loo"     (IWord64 slCountStarts) ""
    , Field 3 0 "blockCtx"     "blok"  "ctx"     (IWord64 slCountBlkCtx) ""
    , Field 3 0 "ledgerState"  "ledg"  "sta"     (IWord64 slCountLgrState) ""
    , Field 3 0 "ledgerView"   "ledg"  "viw"     (IWord64 slCountLgrView) ""
    , Field 3 0 "leadShips"    "ship"  "win"     (IWord64 slCountLeads) ""
    , Field 3 0 "forges"       "For"   "ge"      (IWord64 slCountForges) ""
    , Field 4 0 "CDBSnap"      "CDB"   "snap"    (IWord64 slChainDBSnap) ""
    , Field 3 0 "rejTxs"       "rej"   "txs"     (IWord64 slRejectedTx) ""
    , Field 7 0 "startDelay"   "loop"  "start"   (IText (smaybe "" show.slStarted)) ""
    , Field 5 0 "blkCtx"       "block" "ctx"     (IText (smaybe "" show.slBlkCtx)) ""
    , Field 5 0 "lgrState"     "ledgr" "state"   (IText (smaybe "" show.slLgrState)) ""
    , Field 5 0 "lgrView"      "ledgr" "view"    (IText (smaybe "" show.slLgrView)) ""
    , Field 5 0 "leadChecked"  "ledsh" "chekd"   (IText (smaybe "" show.slLeading)) ""
    , Field 5 0 "forge"        "forge" "done"    (IText (smaybe "" show.slForged)) ""
    , Field 4 0 "mempoolTxSpan" (t 4!!0) "span"  (IText (smaybe "" show.slSpanTxsMem)) ""
    , Field 4 0 "txsColl"     (t 4!!1) "cold"    (IWord64 slTxsCollected) ""
    , Field 4 0 "txsAcc"      (t 4!!2) "accd"    (IWord64 slTxsAccepted) ""
    , Field 4 0 "txsRej"      (t 4!!3) "rejd"    (IWord64 slTxsRejected) ""
    , Field 5 1 "chDensity"    "chain" "dens."   (IFloat  slDensity) ""
    , Field 3 0 "CPU%"        (c 3!!0) "all"     (IText (d 3.rCentiCpu.slResources)) ""
    , Field 3 0 "GC%"         (c 3!!1) "GC"      (IText (d 3.fmap (min 999).rCentiGC.slResources)) ""
    , Field 3 0 "MUT%"        (c 3!!2) "mut"     (IText (d 3.fmap (min 999).rCentiMut.slResources)) ""
    , Field 3 0 "majFlt"      (g 3!!0) "maj"     (IText (d 3.rGcsMajor.slResources)) ""
    , Field 3 0 "minFlt"      (g 3!!1) "min"     (IText (d 3.rGcsMinor.slResources)) ""
    , Field 6 0 "productiv"   "Produc" "tivity"  (IText
      (\SlotStats{..}->
          f 4 $ calcProd <$> (min 6 . -- workaround for ghc-8.10.2
                              fromIntegral <$> rCentiMut slResources :: Maybe Double)
          <*> (fromIntegral <$> rCentiCpu slResources))) ""
    , Field 5 0 "rssMB"       (m 5!!0) "RSS"     (IText (d 5.rRSS  .slResources)) ""
    , Field 5 0 "heapMB"      (m 5!!1) "Heap"    (IText (d 5.rHeap .slResources)) ""
    , Field 5 0 "liveMB"      (m 5!!2) "Live"    (IText (d 5.rLive .slResources)) ""
    , Field 5 0 "allocatedMB"  "Allocd" "MB"     (IText (d 5.rAlloc.slResources)) ""
    , Field 6 0 "allocMut"     "Alloc/" "mutSec" (IText
      (\SlotStats{..}->
          d 5 $
          (ceiling :: Double -> Int)
          <$> ((/) <$> (fromIntegral . (100 *) <$> rAlloc slResources)
                <*> (fromIntegral . max 1 . (1024 *) <$> rCentiMut slResources)))) ""
    , Field 7 0 "mempoolTxs"   "Mempool" "txs"   (IWord64 slMempoolTxs) ""
    , Field 9 0 "utxoEntries"  "UTxO"  "entries" (IWord64 slUtxoSize) ""
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
