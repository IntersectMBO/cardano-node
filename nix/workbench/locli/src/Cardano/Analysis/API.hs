{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.API (module Cardano.Analysis.API) where

import Prelude                  ((!!))
import Cardano.Prelude          hiding (head)

import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Text.Short          (toText)
import Data.Time.Clock          (NominalDiffTime)

import Ouroboros.Network.Block  (BlockNo(..), SlotNo(..))

import Cardano.Analysis.Profile
import Cardano.Unlog.LogObject  hiding (Text)
import Cardano.Unlog.Render

import Data.Distribution

--
-- * API types
--

-- | Results of block propagation analysis.
data BlockPropagation
  = BlockPropagation
    { bpForgerForges        :: !(Distribution Float NominalDiffTime)
    , bpForgerAdoptions     :: !(Distribution Float NominalDiffTime)
    , bpForgerAnnouncements :: !(Distribution Float NominalDiffTime)
    , bpForgerSends         :: !(Distribution Float NominalDiffTime)
    , bpPeerNotices         :: !(Distribution Float NominalDiffTime)
    , bpPeerRequests        :: !(Distribution Float NominalDiffTime)
    , bpPeerFetches         :: !(Distribution Float NominalDiffTime)
    , bpPeerAdoptions       :: !(Distribution Float NominalDiffTime)
    , bpPeerAnnouncements   :: !(Distribution Float NominalDiffTime)
    , bpPeerSends           :: !(Distribution Float NominalDiffTime)
    , bpPropagation         :: !(Distribution Float NominalDiffTime)
    , bpSizes               :: !(Distribution Float Int)
    , bpChainBlockEvents    :: [BlockEvents]
    }
  deriving (Generic, FromJSON, ToJSON, Show)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beForge        :: !BlockForge
  , beObservations :: [BlockObservation]
  , bePropagation  :: !NominalDiffTime -- ^ Slot start to last adoption on cluster
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
  , bfForged       :: !NominalDiffTime -- ^ Since slot start
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
  , boErrors     :: [BPError]
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

--
-- * Key properties
--
isValidBlockEvent :: BlockEvents -> Bool
isValidBlockEvent BlockEvents{..} = (== 1) . bfChainDelta $ beForge

isValidBlockObservation :: BlockObservation -> Bool
isValidBlockObservation BlockObservation{..} =
  -- 1. All phases are present
  null boErrors
  &&
  -- 2. All timings account for processing of a single block
  boChainDelta == 1

--
-- * Instances
--
instance RenderDistributions BlockPropagation where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 "forged"        (f!!0) "Forge"  $ DDeltaT bpForgerForges
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"  $ DDeltaT bpForgerAdoptions
    , Field 6 0 "fAnnounced"    (f!!2) "Announ" $ DDeltaT bpForgerAnnouncements
    , Field 6 0 "fSendStart"    (f!!3) "Sendin" $ DDeltaT bpForgerSends
    , Field 5 0 "noticedVal"    (p!!0) "Notic"  $ DDeltaT bpPeerNotices
    , Field 5 0 "requestedVal"  (p!!1) "Reque"  $ DDeltaT bpPeerRequests
    , Field 5 0 "fetchedVal"    (p!!2) "Fetch"  $ DDeltaT bpPeerFetches
    , Field 5 0 "pAdoptedVal"   (p!!3) "Adopt"  $ DDeltaT bpPeerAdoptions
    , Field 5 0 "pAnnouncedVal" (p!!4) "Annou"  $ DDeltaT bpPeerAnnouncements
    , Field 5 0 "pSendStartVal" (p!!5) "Send"   $ DDeltaT bpPeerSends
    , Field 5 0 "propagation"   "Propa" "gatio" $ DDeltaT bpPropagation
    , Field 9 0 "sizes"         "Size"  "bytes" $ DInt    bpSizes
    ]
   where
     f = nChunksEachOf 4 7 "Forger event Δt:"
     p = nChunksEachOf 6 6 "Peer event Δt:"

instance RenderTimeline BlockEvents where
  rtFields =
    --  Width LeftPad
    [ Field 5 0 "block"        "block" "no."    $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"     "abs."  "slot#"  $ IWord64 (unSlotNo  . beSlotNo)
    , Field 6 0 "hash"         "block" "hash"   $ IText   (shortHash . beBlock)
    , Field 6 0 "hashPrev"     "prev"  "hash"   $ IText   (shortHash . beBlockPrev)
    , Field 7 0 "forger"       "forger" "host"  $ IText  (toText . unHost . bfForger . beForge)
    , Field 9 0 "blockSize"    "size"  "bytes"  $ IInt    (bfBlockSize . beForge)
    , Field 7 0 "blockGap"     "block" "gap"    $ IDeltaT (bfBlockGap  . beForge)
    , Field 3 0 "forks"         "for"  "-ks"    $ IInt   (count bpeIsFork . beErrors)
    , Field 6 0 "forged"        (f!!0) "Forge"  $ IDeltaT (bfForged    . beForge)
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"  $ IDeltaT (bfAdopted   . beForge)
    , Field 6 0 "fAnnounced"    (f!!2) "Announ" $ IDeltaT (bfAnnounced . beForge)
    , Field 6 0 "fSendStart"    (f!!3) "Sendin" $ IDeltaT (bfSending   . beForge)
    , Field 5 0 "valid.observ" "valid" "obsrv"  $ IInt    (length          . valids)
    , Field 5 0 "noticedVal"    (p!!0) "Notic"  $ IDeltaT (af  boNoticed   . valids)
    , Field 5 0 "requestedVal"  (p!!1) "Requd"  $ IDeltaT (af  boRequested . valids)
    , Field 5 0 "fetchedVal"    (p!!2) "Fetch"  $ IDeltaT (af  boFetched   . valids)
    , Field 5 0 "pAdoptedVal"   (p!!3) "Adopt"  $ IDeltaT (af' boAdopted   . valids)
    , Field 5 0 "pAnnouncedVal" (p!!4) "Annou"  $ IDeltaT (af' boAnnounced . valids)
    , Field 5 0 "pSendStartVal" (p!!5) "Send"   $ IDeltaT (af' boSending   . valids)
    , Field 5 0 "pPropagation" "Propa" "gatio"  $ IDeltaT bePropagation
    , Field 5 0 "errors"        "all"  "errs"   $ IInt    (length . beErrors)
    , Field 3 0 "missAdopt"     (m!!0) "ado"    $ IInt    (count (bpeIsMissing Adopt) . beErrors)
    , Field 3 0 "missAnnou"     (m!!1) "ann"    $ IInt    (count (bpeIsMissing Announce) . beErrors)
    , Field 3 0 "missSend"      (m!!2) "snd"    $ IInt    (count (bpeIsMissing Send) . beErrors)
    , Field 3 0 "negAnnou"      (n!!0) "ann"    $ IInt    (count (bpeIsNegative Announce) . beErrors)
    , Field 3 0 "negSend"       (n!!1) "snd"    $ IInt    (count (bpeIsNegative Send) . beErrors)
    ]
   where
     valids = filter isValidBlockObservation . beObservations
     f = nChunksEachOf 4 7 "Forger event Δt:"
     p = nChunksEachOf 6 6 "Peer event Δt averages:"
     m = nChunksEachOf 3 4 "Missing"
     n = nChunksEachOf 2 4 "Negative"
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
