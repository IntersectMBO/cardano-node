{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Avoid lambda" -}
module Cardano.Analysis.BlockProp (module Cardano.Analysis.BlockProp) where

import           Prelude (String, (!!), error, head, id, show, tail)
import           Cardano.Prelude hiding (head, show)

import           Control.Arrow ((***), (&&&))
import Control.DeepSeq qualified as DS
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as AE
import           Data.Bifunctor
import           Data.Function (on)
import           Data.List (dropWhileEnd, intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, mapMaybe, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Short (toText)
import           Data.Tuple (swap)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

import           Text.Printf (printf)

import           Ouroboros.Network.Block (BlockNo(..), SlotNo(..))

import           Data.Accum
import           Data.Distribution
import           Cardano.Analysis.Profile
import           Cardano.Unlog.LogObject hiding (Text)
import           Cardano.Unlog.Render
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats

import qualified Debug.Trace as D


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
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

instance RenderDistributions BlockPropagation where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 "forged"        (f!!0) "Forge"  $ DDeltaT bpForgerForges
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"  $ DDeltaT bpForgerAdoptions
    , Field 6 0 "fAnnounced"    (f!!2) "Announ" $ DDeltaT bpForgerAnnouncements
    , Field 6 0 "fSendStart"    (f!!3) "Sendin" $ DDeltaT bpForgerSends
    , Field 4 1 "noticedVal"    (p!!0) "Notic"  $ DDeltaT bpPeerNotices
    , Field 4 1 "requestedVal"  (p!!1) "Reque"  $ DDeltaT bpPeerRequests
    , Field 4 1 "fetchedVal"    (p!!2) "Fetch"  $ DDeltaT bpPeerFetches
    , Field 4 1 "pAdoptedVal"   (p!!3) "Adopt"  $ DDeltaT bpPeerAdoptions
    , Field 4 1 "pAnnouncedVal" (p!!4) "Annou"  $ DDeltaT bpPeerAnnouncements
    , Field 4 1 "pSendStartVal" (p!!5) "Send"   $ DDeltaT bpPeerSends
    , Field 4 1 "propagation"   "Propa" "gatio" $ DDeltaT bpPropagation
    , Field 4 1 "sizes"         "Size"  "kB"    $ DInt    bpSizes
    ]
   where
     f = nChunksEachOf 4 7 "Forger event Δt:"
     p = nChunksEachOf 6 5 "Peer event Δt:"

data BPError
  = BPError
  { eHost  :: !Host
  , eBlock :: !Hash
  , eLO    :: !(Maybe LogObject)
  , eDesc  :: !BPErrorKind
  }
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

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

bpeIsFork, bpeIsMissingAny, bpeIsNegativeAny  :: BPError -> Bool
bpeIsFork BPError{eDesc=BPEFork{}} = True
bpeIsFork _ = False
bpeIsMissingAny BPError{eDesc=BPEMissingPhase{}} = True
bpeIsMissingAny _ = False
bpeIsNegativeAny BPError{eDesc=BPENegativePhase{}} = True
bpeIsNegativeAny _ = False

bpeIsMissing, bpeIsNegative  :: Phase -> BPError -> Bool
bpeIsMissing  p BPError{eDesc=BPEMissingPhase p'} = p == p'
bpeIsMissing  _ _ = False
bpeIsNegative p BPError{eDesc=BPENegativePhase p' _} = p == p'
bpeIsNegative _ _ = False

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

-- | Block's events, as seen by its forger.
data ForgerEvents a
  =  ForgerEvents
  { bfeHost         :: !Host
  , bfeBlock        :: !Hash
  , bfeBlockPrev    :: !Hash
  , bfeBlockNo      :: !BlockNo
  , bfeSlotNo       :: !SlotNo
  , bfeSlotStart    :: !SlotStart
  , bfeBlockSize    :: !(Maybe Int)
  , bfeForged       :: !(Maybe a)
  , bfeAdopted      :: !(Maybe a)
  , bfeChainDelta   :: !Int
  , bfeAnnounced    :: !(Maybe a)
  , bfeSending      :: !(Maybe a)
  , bfeErrs         :: [BPError]
  }
  deriving (Generic, NFData, AE.FromJSON, AE.ToJSON, Show)

type ForgerEventsAbs = ForgerEvents UTCTime
type ForgerEventsRel = ForgerEvents NominalDiffTime

bfePrevBlock :: ForgerEvents a -> Maybe Hash
bfePrevBlock x = case bfeBlockNo x of
  0 -> Nothing
  _ -> Just $ bfeBlockPrev x

-- | Block's events, as seen by an observer.
data ObserverEvents a
  =  ObserverEvents
  { boeHost       :: !Host
  , boeBlock      :: !Hash
  , boeBlockNo    :: !BlockNo
  , boeSlotNo     :: !SlotNo
  , boeSlotStart  :: !SlotStart
  , boeNoticed    :: !(Maybe a)
  , boeRequested  :: !(Maybe a)
  , boeFetched    :: !(Maybe a)
  , boeAdopted    :: !(Maybe a)
  , boeChainDelta :: !Int
  , boeAnnounced  :: !(Maybe a)
  , boeSending    :: !(Maybe a)
  , boeErrs       :: [BPError]
  }
  deriving (Generic, NFData, AE.FromJSON, AE.ToJSON, Show)

type ObserverEventsAbs = ObserverEvents UTCTime
type ObserverEventsRel = ObserverEvents NominalDiffTime

mbePhaseIndex :: Map Phase (MachBlockEvents a -> Maybe a)
mbePhaseIndex = Map.fromList
  [ (Notice,     mbeNoticed)
  , (Request,    mbeRequested)
  , (Fetch,      mbeAcquired)
  , (Forge,      mbeAcquired)
  , (Acquire,    mbeAcquired)
  , (Adopt,      mbeAdopted)
  , (Announce,   mbeAnnounced)
  , (Send,       mbeSending)
  ]

mbeGetProjection :: Phase -> (MachBlockEvents a -> Maybe a)
mbeGetProjection k =
  Map.lookup k mbePhaseIndex
  & fromMaybe (error $ "Unknown phase: " <> show k)

-- | Sum of observer and forger events alike.
data MachBlockEvents a
  = MFE (ForgerEvents a)
  | MOE (ObserverEvents a)
  | MBE  BPError
  deriving (Generic, NFData)

mbeForgP, mbeObsvP, mbeErrP :: MachBlockEvents a -> Bool
mbeForgP = \case
  MFE{} -> True
  _ -> False
mbeObsvP = \case
  MOE{} -> True
  _ -> False
mbeErrP = \case
  MBE{} -> True
  _ -> False

mapMbe :: (ForgerEvents a -> b) -> (ObserverEvents a -> b) -> (BPError -> b)
       -> MachBlockEvents a -> b
mapMbe f o e = \case
  MFE x -> f x
  MOE x -> o x
  MBE x -> e x

mapMbeErrs :: ([BPError] -> [BPError]) -> MachBlockEvents a -> MachBlockEvents a
mapMbeErrs f = mapMbe (\x -> MFE x { bfeErrs=f $ bfeErrs x } )
                      (\x -> MOE x { boeErrs=f $ boeErrs x } )
                      MBE

partitionMbes :: [MachBlockEvents a] -> ([ForgerEvents a], [ObserverEvents a], [BPError])
partitionMbes = go [] [] []
  where
    go :: [ForgerEvents a] -> [ObserverEvents a] -> [BPError] -> [MachBlockEvents a] -> ([ForgerEvents a], [ObserverEvents a], [BPError])
    go as bs cs [] = (reverse as, reverse bs, reverse cs)
    go as bs cs (MFE a:xs) = go (a:as) bs cs xs
    go as bs cs (MOE b:xs) = go as (b:bs) cs xs
    go as bs cs (MBE c:xs) = go as bs (c:cs) xs

errorMbes :: [MachBlockEvents a] -> [BPError]
errorMbes = go []
  where
    go :: [BPError] -> [MachBlockEvents a] -> [BPError]
    go cs [] = reverse cs
    go cs (MBE c:xs) = go (c:cs) xs
    go cs (_:xs)     = go    cs  xs

trimapMbe ::
     (ForgerEvents a -> ForgerEvents a)
  -> (ObserverEvents a -> ObserverEvents a)
  -> (BPError -> BPError)
  -> MachBlockEvents a -> MachBlockEvents a
trimapMbe f o e = mapMbe (MFE . f) (MOE . o) (MBE . e)

bimapMbe ::
     (ForgerEvents a -> ForgerEvents a)
  -> (ObserverEvents a -> ObserverEvents a)
  -> MachBlockEvents a -> MachBlockEvents a
bimapMbe f o = trimapMbe f o id

bimapMbe' ::
     (ForgerEvents   a -> Either BPError (ForgerEvents   a))
  -> (ObserverEvents a -> Either BPError (ObserverEvents a))
  -> MachBlockEvents a -> MachBlockEvents a
bimapMbe' f o = \case
  MFE x -> either MBE MFE (f x)
  MOE x -> either MBE MOE (o x)
  x@MBE{} -> x

ordBlockEv :: MachBlockEvents a -> MachBlockEvents a -> Ordering
ordBlockEv l r
  | (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) l r = GT
  | (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) r l = LT
  | mbeForgP l = GT
  | mbeForgP r = LT
  | mbeObsvP l = GT
  | mbeObsvP r = LT
  | otherwise  = EQ

mbeSlotStart :: MachBlockEvents a -> SlotStart
mbeSlotStart = mapMbe bfeSlotStart boeSlotStart (SlotStart . const zeroUTCTime)

mbeNoticed, mbeRequested, mbeAcquired, mbeAdopted, mbeAnnounced, mbeSending :: MachBlockEvents a -> Maybe a
mbeNoticed   = mapMbe (const Nothing)  boeNoticed   (const Nothing)
mbeRequested = mapMbe (const Nothing)  boeRequested (const Nothing)
mbeAcquired  = mapMbe bfeForged        boeFetched   (const Nothing)
mbeAdopted   = mapMbe bfeAdopted       boeAdopted   (const Nothing)
mbeAnnounced = mapMbe bfeAnnounced     boeAnnounced (const Nothing)
mbeSending   = mapMbe bfeSending       boeSending   (const Nothing)

mbeBlockSize :: MachBlockEvents a -> Maybe Int
mbeBlockSize = mapMbe bfeBlockSize (const Nothing) (const Nothing)

mbeHost :: MachBlockEvents a -> Host
mbeHost = mapMbe bfeHost boeHost eHost

mbeBlock :: MachBlockEvents a -> Hash
mbeBlock = mapMbe bfeBlock boeBlock eBlock

mbeBlockNo :: MachBlockEvents a -> BlockNo
mbeBlockNo = mapMbe bfeBlockNo boeBlockNo (const (-1))

mbeError :: MachBlockEvents a -> Maybe BPError
mbeError = mapMbe (const Nothing) (const Nothing) Just

mbeFailed :: MachBlockEvents a -> Bool
mbeFailed = isJust . mbeError

-- | Machine's private view of all the blocks.
type MachBlockMap a
  =  Map.Map Hash (MachBlockEvents a)

blockMapMaxBlock :: MachBlockMap a -> MachBlockEvents a
blockMapMaxBlock = maximumBy ordBlockEv . Map.elems

blockMapBlock :: Hash -> MachBlockMap a -> MachBlockEvents a
blockMapBlock h =
  fromMaybe (error $ "Invariant failed:  missing hash " <> show h) . Map.lookup h

data BlockForge
  =  BlockForge
  { bfForger       :: !Host
  , bfSlotStart    :: !SlotStart
  , bfBlockGap     :: !NominalDiffTime
  , bfBlockSize    :: !Int
  , bfForged       :: !NominalDiffTime
  , bfAdopted      :: !NominalDiffTime
  , bfChainDelta   :: !Int -- ^ ChainDelta during adoption
  , bfAnnounced    :: !NominalDiffTime
  , bfSending      :: !NominalDiffTime
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | A completed, compactified version of ObserverEvents.
data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !SlotStart
  , boNoticed    :: !NominalDiffTime
  , boRequested  :: !NominalDiffTime
  , boFetched    :: !NominalDiffTime
  , boAdopted    :: !(Maybe NominalDiffTime)
  , boChainDelta :: !Int -- ^ ChainDelta during adoption
  , boAnnounced  :: !(Maybe NominalDiffTime)
  , boSending    :: !(Maybe NominalDiffTime)
  , boErrors     :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beForge        :: !BlockForge
  , beObservations :: [BlockObservation]
  , bePropagation  :: !NominalDiffTime -- ^ Slot start to final adoption
  , beOtherBlocks  :: [Hash]
  , beErrors       :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

beForgedAt :: BlockEvents -> UTCTime
beForgedAt BlockEvents{beForge=BlockForge{..}} =
  bfForged `afterSlot` bfSlotStart

instance RenderTimeline BlockEvents where
  rtFields =
    --  Width LeftPad
    [ Field 5 0 "block"        "block" "no."    $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"     "abs."  "slot#"  $ IWord64 (unSlotNo  . beSlotNo)
    , Field 6 0 "hash"         "block" "hash"   $ IText   (shortHash . beBlock)
    , Field 6 0 "hashPrev"     "prev"  "hash"   $ IText   (shortHash . beBlockPrev)
    , Field 7 0 "forger"       "forger" "host"  $ IText  (toText . unHost . bfForger . beForge)
    , Field 6 0 "blockSize"    "size"  "kB"     $ IInt    ((`div` 1024) . bfBlockSize . beForge)
    , Field 7 0 "blockGap"     "block" "gap"    $ IDeltaT (bfBlockGap  . beForge)
    , Field 6 0 "forged"        (f!!0) "Forge"  $ IDeltaT (bfForged    . beForge)
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"  $ IDeltaT (bfAdopted   . beForge)
    , Field 6 0 "fAnnounced"    (f!!2) "Announ" $ IDeltaT (bfAnnounced . beForge)
    , Field 6 0 "fSendStart"    (f!!3) "Sendin" $ IDeltaT (bfSending   . beForge)
    , Field 5 0 "valid.observ"  "valid" "obsrv" $ IInt    (length          . valids)
    , Field 5 0 "noticedVal"    (p!!0) "Notic"  $ IDeltaT (af  boNoticed   . valids)
    , Field 5 0 "requestedVal"  (p!!1) "Requd"  $ IDeltaT (af  boRequested . valids)
    , Field 5 0 "fetchedVal"    (p!!2) "Fetch"  $ IDeltaT (af  boFetched   . valids)
    , Field 5 0 "pAdoptedVal"   (p!!3) "Adopt"  $ IDeltaT (af' boAdopted   . valids)
    , Field 5 0 "pAnnouncedVal" (p!!4) "Annou"  $ IDeltaT (af' boAnnounced . valids)
    , Field 5 0 "pSendStartVal" (p!!5) "Send"   $ IDeltaT (af' boSending   . valids)
    , Field 5 0 "pPropagation" "Propa" "gation" $ IDeltaT bePropagation
    , Field 5 0 "errors"        "all"  "errs"   $ IInt    (length . beErrors)
    , Field 5 0 "forks"         ""     "forks"  $ IInt   (count bpeIsFork . beErrors)
    , Field 5 0 "missAdopt"     (m!!0) "adopt"  $ IInt    (count (bpeIsMissing Adopt) . beErrors)
    , Field 5 0 "missAnnou"     (m!!1) "annou"  $ IInt    (count (bpeIsMissing Announce) . beErrors)
    , Field 5 0 "missSend"      (m!!2) "send"   $ IInt    (count (bpeIsMissing Send) . beErrors)
    , Field 5 0 "negAnnou"      (n!!0) "annou"  $ IInt    (count (bpeIsNegative Announce) . beErrors)
    , Field 5 0 "negSend"       (n!!1) "send"   $ IInt    (count (bpeIsNegative Send) . beErrors)
    ]
   where
     valids = filter isValidBlockObservation . beObservations
     f = nChunksEachOf 4 7 "Forger event Δt:"
     p = nChunksEachOf 6 6 "Peer event Δt averages:"
     m = nChunksEachOf 3 6 "Missing phase"
     n = nChunksEachOf 2 6 "Negative phase"
     af  f = avg . fmap f
     af' f = avg . mapMaybe f
     avg :: [NominalDiffTime] -> NominalDiffTime
     avg [] = 0
     avg xs =  (/ fromInteger (fromIntegral $ length xs)) $ sum xs
     count :: (a -> Bool) -> [a] -> Int
     count f = length . filter f
  rtCommentary BlockEvents{..} = ("    " <>) . T.pack . show <$> beErrors

mapChainToBlockEventCDF ::
  (Real a, ToRealFrac a Float)
  => [PercSpec Float]
  -> [BlockEvents]
  -> (BlockEvents -> Maybe a)
  -> Distribution Float a
mapChainToBlockEventCDF percs cbe proj =
  computeDistribution percs $ mapMaybe proj cbe

mapChainToPeerBlockObservationCDF ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockObservation -> Maybe NominalDiffTime)
  -> String
  -> Distribution Float NominalDiffTime
mapChainToPeerBlockObservationCDF percs chainBlockEvents proj desc =
  computeDistribution percs allObservations
 where
   allObservations :: [NominalDiffTime]
   allObservations =
     concat $
     filter isValidBlockEvent chainBlockEvents
     <&> blockObservations

   blockObservations :: BlockEvents -> [NominalDiffTime]
   blockObservations be =
     proj `mapMaybe` beObservations be

isValidBlockEvent :: BlockEvents -> Bool
isValidBlockEvent BlockEvents{..} = (== 1) . bfChainDelta $ beForge

isValidBlockObservation :: BlockObservation -> Bool
isValidBlockObservation BlockObservation{..} =
  -- 1. All phases are present
  null boErrors
  &&
  -- 2. All timings account for processing of a single block
  boChainDelta == 1

blockProp :: ChainInfo -> [(JsonLogfile, [LogObject])] -> IO BlockPropagation
blockProp ci xs = do
  putStrLn ("blockProp: recovering block event maps" :: String)
  doBlockProp =<< mapConcurrently
    (\x ->
        evaluate $ DS.force $
        blockEventMapsFromLogObjects ci x)
    xs

doBlockProp :: [MachBlockMap UTCTime] -> IO BlockPropagation
doBlockProp eventMaps = do
  putStrLn ("tip block: "    <> show tipBlock :: String)
  putStrLn ("chain length: " <> show (length chain) :: String)
  pure BlockPropagation
    { bpForgerForges        = forgerEventsCDF   (Just . bfForged    . beForge)
    , bpForgerAdoptions     = forgerEventsCDF   ((\x ->
                                                    if bfChainDelta x == 1
                                                    then Just (bfAdopted x)
                                                    else Nothing)   . beForge)
    , bpForgerAnnouncements = forgerEventsCDF   (Just . bfAnnounced . beForge)
    , bpForgerSends         = forgerEventsCDF   (Just . bfSending   . beForge)
    , bpPeerNotices         = observerEventsCDF (Just . boNoticed)   "noticed"
    , bpPeerRequests        = observerEventsCDF (Just . boRequested) "requested"
    , bpPeerFetches         = observerEventsCDF (Just . boFetched)   "fetched"
    , bpPeerAdoptions       = observerEventsCDF boAdopted            "adopted"
    , bpPeerAnnouncements   = observerEventsCDF boAnnounced          "announced"
    , bpPeerSends           = observerEventsCDF boSending            "sending"
    , bpPropagation         = forgerEventsCDF   (Just . bePropagation)
    , bpSizes               = forgerEventsCDF   (Just . bfBlockSize . beForge)
    , bpChainBlockEvents    = chain
    }
 where
   forgerEventsCDF   :: (Real a, ToRealFrac a Float) => (BlockEvents -> Maybe a) -> Distribution Float a
   forgerEventsCDF   = mapChainToBlockEventCDF           stdPercentiles chain
   observerEventsCDF = mapChainToPeerBlockObservationCDF stdPercentiles chain

   chain          :: [BlockEvents]
   chain          = rebuildChain (fmap deltifyEvents <$> eventMaps) tipHash
                    & computeChainBlockGaps

   adoptionMap    :: [Map Hash UTCTime]
   adoptionMap    =  Map.mapMaybe mbeAdopted <$> eventMaps

   heightMap      :: Map BlockNo (Set Hash)
   heightMap      = foldr (\em acc ->
                             Map.foldr
                             (\mbe -> Map.alter
                                      (maybe (Just $ Set.singleton (mbeBlock mbe))
                                             (Just . Set.insert (mbeBlock mbe)))
                                      (mbeBlockNo mbe))
                             acc em)
                    mempty eventMaps
   tipBlock       = getBlockForge eventMaps tipHash
   tipHash        = rewindChain eventMaps 1 (mbeBlock finalBlockEv)
   finalBlockEv   = maximumBy ordBlockEv $ blockMapMaxBlock <$> eventMaps

   rewindChain :: [MachBlockMap a] -> Int -> Hash -> Hash
   rewindChain eventMaps count tip = go tip count
    where go tip = \case
            0 -> tip
            n -> go (bfeBlockPrev $ getBlockForge eventMaps tip) (n - 1)

   getBlockForge :: [MachBlockMap a] -> Hash -> ForgerEvents a
   getBlockForge xs h =
     mapMaybe (Map.lookup h) xs
     & find mbeForgP
     & fromMaybe
        (error $ mconcat
         [ "Invariant failed: couldn't find a forge for hash ", show h
         , "\nErrors:\n", show (intercalate "\n" $ fmap show $ errorMbes $ mapMaybe (Map.lookup h) xs)
         ])
     & mapMbe id (error "Silly invariant failed.") (error "Silly invariant failed.")

   rebuildChain :: [MachBlockMap NominalDiffTime] -> Hash -> [BlockEvents]
   rebuildChain machBlockMaps tip = go (Just tip) []
    where go Nothing  acc = acc
          go (Just h) acc =
            case partitionMbes $ mapMaybe (Map.lookup h) machBlockMaps of
              ([], _, ers) -> error $ mconcat
                [ "No forger for hash ", show h
                , "\nErrors:\n"
                ] ++ intercalate "\n" (show <$> ers)
              blkEvs@(forgerEv:_, oEvs, ers) ->
                go (bfePrevBlock forgerEv) (liftBlockEvents forgerEv oEvs ers : acc)

   computeChainBlockGaps :: [BlockEvents] -> [BlockEvents]
   computeChainBlockGaps [] = error "computeChainBlockGaps on an empty chain"
   computeChainBlockGaps lst@(hd:_) =
     snd $ mapAccumL step (unSlotStart . bfSlotStart $ beForge hd) lst
    where
      step :: UTCTime -> BlockEvents -> (UTCTime, BlockEvents)
      step prevForge x@(beForgedAt -> at) =
        (at, x { beForge = (beForge x) { bfBlockGap = at `diffUTCTime` prevForge } })

   liftBlockEvents :: ForgerEventsRel -> [ObserverEvents NominalDiffTime] -> [BPError] -> BlockEvents
   liftBlockEvents ForgerEvents{bfeHost=host, ..} os errs =
     BlockEvents
     { beBlock      = bfeBlock
     , beBlockPrev  = bfeBlockPrev
     , beBlockNo    = bfeBlockNo
     , beSlotNo     = bfeSlotNo
     , beForge =
       BlockForge
       { bfForger     = host
       , bfSlotStart  = bfeSlotStart
       , bfBlockGap   = 0 -- To be filled in after chain is rebuilt.
       , bfBlockSize  = bfeBlockSize & handleMiss "Size"
       , bfForged     = bfeForged    & handleMiss "Δt Forged"
       , bfAdopted    = bfeAdopted   & handleMiss "Δt Adopted (forger)"
       , bfChainDelta = bfeChainDelta
       , bfAnnounced  = bfeAnnounced & handleMiss "Δt Announced (forger)"
       , bfSending    = bfeSending   & handleMiss "Δt Sending (forger)"
       }
     , beObservations =
         os <&> \ObserverEvents{..} ->
           BlockObservation
           { boObserver   = boeHost
           , boSlotStart  = bfeSlotStart
           , boNoticed    = boeNoticed   & handleMiss "Δt Noticed (peer)"
           , boRequested  = boeRequested & handleMiss "Δt Requested (peer)"
           , boFetched    = boeFetched   & handleMiss "Δt Fetched (peer)"
           , boAdopted    = boeAdopted
           , boChainDelta = boeChainDelta
           , boAnnounced  = boeAnnounced
           , boSending    = boeSending
           , boErrors     = boeErrs
           }
     , bePropagation  = lastAdoption `sinceSlot` bfeSlotStart
     , beOtherBlocks  = otherBlocks
     , beErrors =
         errs
         <> (otherBlocks <&>
             \blk ->
               fail' (findForger blk) bfeBlock $ BPEFork blk)
         <> bfeErrs
         <> concatMap boeErrs os
     }
    where
      lastAdoption = maximum $ Map.lookup bfeBlock `mapMaybe` adoptionMap

      otherBlocks = Map.lookup bfeBlockNo heightMap
                    & handleMiss "height map"
                    & Set.delete bfeBlock
                    & Set.toList

      findForger :: Hash -> Host
      findForger hash =
        mapMaybe (Map.lookup hash) eventMaps
        & find mbeForgP
        & fmap (mapMbe bfeHost (error "Invariant failed") (error "Invariant failed"))
        & fromMaybe (Host "?")

      fail' :: Host -> Hash -> BPErrorKind -> BPError
      fail' host hash desc = BPError host hash Nothing desc

      handleMiss :: String -> Maybe a -> a
      handleMiss slotDesc = fromMaybe $ error $ mconcat
       [ "While processing ", show bfeBlockNo, " hash ", show bfeBlock
       , " forged by ", show (unHost host)
       , " -- missing: ", slotDesc
       ]

-- | Given a single machine's log object stream, recover its block map.
blockEventMapsFromLogObjects :: ChainInfo -> (JsonLogfile, [LogObject]) -> MachBlockMap UTCTime
blockEventMapsFromLogObjects ci (f@(unJsonLogfile -> fp), xs) =
  trace ("processing " <> fp)
  $ if Map.size machBlockMap == 0
    then error $ mconcat
         ["No block events in ",fp," : ","LogObject count: ",show (length xs)]
    else machBlockMap
 where
   machBlockMap = foldl (blockPropMachEventsStep ci f) mempty xs

blockPropMachEventsStep :: ChainInfo -> JsonLogfile -> MachBlockMap UTCTime -> LogObject -> MachBlockMap UTCTime
blockPropMachEventsStep ci (JsonLogfile fp) bMap lo = case lo of
  -- 0. Notice (observer only)
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = Map.lookup loBlock bMap
    in if isJust mbe0 then bMap else
      MOE
       (ObserverEvents
        loHost loBlock loBlockNo loSlotNo
        (slotStart ci loSlotNo) (Just loAt)
        Nothing Nothing Nothing 0 Nothing Nothing [])
      & doInsert loBlock
  -- 1. Request (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientRequested{loBlock,loLength}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Request)
    in if isJust (mbeRequested mbe0) then bMap else
      bimapMbe'
      (const . Left $ fail' loHost loBlock $ BPEUnexpectedForForger Request)
      (\x -> Right x { boeRequested=Just loAt, boeChainDelta=loLength `max` boeChainDelta x })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Fetch (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Fetch)
    in if isJust (mbeAcquired mbe0) then bMap else
      bimapMbe'
      (const . Left $ fail' loHost loBlock (BPEUnexpectedForForger Fetch))
      (\x -> Right x { boeFetched=Just loAt })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Forge (forger only)
  LogObject{loAt, loHost, loBody=LOBlockForged{loBlock,loPrev,loBlockNo,loSlotNo}} ->
    Map.lookup loBlock bMap
    <&> bimapMbe'
          (const.Left $
           BPError loHost loBlock (Just lo) BPEDuplicateForge)
          (const.Left $
           BPError loHost loBlock (Just lo) (BPEUnexpectedForObserver Forge))
    & fromMaybe
      (MFE $ ForgerEvents
        { bfeHost         = loHost
        , bfeBlock        = loBlock
        , bfeBlockPrev    = loPrev
        , bfeBlockNo      = loBlockNo
        , bfeSlotNo       = loSlotNo
        , bfeSlotStart    = slotStart ci loSlotNo
        , bfeBlockSize    = Nothing
        , bfeForged       = Just loAt
        , bfeAdopted      = Nothing
        , bfeChainDelta   = 0
        , bfeAnnounced    = Nothing
        , bfeSending      = Nothing
        , bfeErrs         = []
        })
    & doInsert loBlock
  -- 3. Adopt
  LogObject{loAt, loHost,
            loBody=LOBlockAddedToCurrentChain{loBlock,loLength,loSize}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Adopt)
    in
      if isJust (mbeAdopted mbe0) && isJust (mbeBlockSize mbe0)
      then bMap else
      mbe0
      & (if isJust (mbeAdopted mbe0) then id else
         bimapMbe
         (\x -> x { bfeAdopted=Just loAt, bfeChainDelta=loLength })
         (\x -> x { boeAdopted=Just loAt, boeChainDelta=loLength `max` boeChainDelta x}))
      & (if isJust (mbeBlockSize mbe0) || isNothing loSize then id else
         bimapMbe
         (\x -> x { bfeBlockSize=loSize })
         id)
      & doInsert loBlock
  -- 4. Announce
  LogObject{loAt, loHost, loBody=LOChainSyncServerSendHeader{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Announce)
    in if isJust (mbeAnnounced mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeAnnounced=Just loAt })
      (\x -> x { boeAnnounced=Just loAt })
      mbe0
      & doInsert loBlock
  -- 5. Sending started
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSending{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Send)
    in if isJust (mbeSending mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeSending=Just loAt })
      (\x -> x { boeSending=Just loAt })
      mbe0
      & doInsert loBlock
  _ -> bMap
 where
   fail' :: Host -> Hash -> BPErrorKind -> BPError
   fail' host hash desc = BPError host hash (Just lo) desc

   fail :: Host -> Hash -> BPErrorKind -> MachBlockEvents a
   fail host hash desc = MBE $ fail' host hash desc

   doInsert :: Hash -> MachBlockEvents UTCTime -> MachBlockMap UTCTime
   doInsert k x = Map.insert k x bMap

deltifyEvents :: MachBlockEvents UTCTime -> MachBlockEvents NominalDiffTime
deltifyEvents (MBE e) = MBE e
deltifyEvents (MFE x@ForgerEvents{..}) =
  MFE x
  { bfeForged    = bfeForged <&> (`sinceSlot` bfeSlotStart)
  , bfeAdopted   = diffUTCTime <$> bfeAdopted   <*> bfeForged
  , bfeAnnounced = diffUTCTime <$> bfeAnnounced <*> bfeAdopted
  , bfeSending   = diffUTCTime <$> bfeSending   <*> bfeAnnounced
  } & \case
  v@(MFE x') -> MFE x' { bfeErrs = collectEventErrors v
                         [Forge, Adopt, Announce, Send] }
  _ -> error "Impossible"
deltifyEvents (MOE x@ObserverEvents{..}) =
  MOE x
  { boeNoticed   = boeNoticed <&> (`sinceSlot` boeSlotStart)
  , boeRequested = diffUTCTime <$> boeRequested <*> boeNoticed
  , boeFetched   = diffUTCTime <$> boeFetched   <*> boeRequested
  , boeAdopted   = diffUTCTime <$> boeAdopted   <*> boeFetched
  , boeAnnounced = diffUTCTime <$> boeAnnounced <*> boeAdopted
  , boeSending   = diffUTCTime <$> boeSending   <*> boeAnnounced
  } & \case
  v@(MOE x') -> MOE x' { boeErrs = collectEventErrors v
                         [Notice, Request, Fetch, Adopt, Announce, Send] }
  _ -> error "Impossible"

collectEventErrors :: MachBlockEvents NominalDiffTime -> [Phase] -> [BPError]
collectEventErrors mbe phases =
  [ BPError (mbeHost mbe) (mbeBlock mbe) Nothing $
    case (miss, proj) of
      (,) True _       -> BPEMissingPhase phase
      (,) _ (Just neg) -> BPENegativePhase phase neg
      _ -> error "Impossible."
  | phase <- phases
  , let proj = mbeGetProjection phase mbe
  , let miss = isNothing proj
  , let neg  = ((< 0) <$> proj) == Just True
  , miss || neg
  ]
