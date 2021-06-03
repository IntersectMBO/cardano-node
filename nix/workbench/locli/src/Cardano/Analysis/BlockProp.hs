{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}
module Cardano.Analysis.BlockProp (module Cardano.Analysis.BlockProp) where

import           Prelude (String, (!!), error, head, id, show, tail)
import           Cardano.Prelude hiding (head, show)

import           Control.Arrow ((&&&), (***))
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (toJSON)
import qualified Data.Aeson as AE
import           Data.Function (on)
import           Data.Either (partitionEithers, isLeft, isRight)
import           Data.List (dropWhileEnd)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Tuple (swap)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map

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
    , bpPeerNotices         :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerFetches         :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerAdoptions       :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerAnnouncements   :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerSends           :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpChainBlockEvents    :: [BlockEvents]
    }
  deriving Show

instance RenderDistributions BlockPropagation where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 "forged"        (f!!0) "Forge"   $ DDeltaT bpForgerForges
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"   $ DDeltaT bpForgerAdoptions
    , Field 6 0 "fAnnounced"    (f!!2) "Ann-ce"  $ DDeltaT bpForgerAnnouncements
    , Field 6 0 "fSendStart"    (f!!3) "Sending" $ DDeltaT bpForgerSends
    , Field 4 1 "noticedVal"    (p!!0) " Noti"   $ DDeltaT (fst . bpPeerNotices)
    , Field 4 0 "noticedCoV"    (p!!1) "ced  "   $ DDeltaT (snd . bpPeerNotices)
    , Field 4 1 "fetchedVal"    (p!!2) " Fetc"   $ DDeltaT (fst . bpPeerFetches)
    , Field 4 0 "fetchedCoV"    (p!!3) "hed  "   $ DDeltaT (snd . bpPeerFetches)
    , Field 4 1 "pAdoptedVal"   (p!!4) " Adop"   $ DDeltaT (fst . bpPeerAdoptions)
    , Field 4 0 "pAdoptedCoV"   (p!!5) "ted  "   $ DDeltaT (snd . bpPeerAdoptions)
    , Field 4 1 "pAnnouncedVal" (p!!6) "Annou" $ DDeltaT (fst . bpPeerAnnouncements)
    , Field 4 0 "pAnnouncedCoV" (p!!7) "nced " $ DDeltaT (snd . bpPeerAnnouncements)
    , Field 4 1 "pSendStartVal" (p!!8) " Send" $ DDeltaT (fst . bpPeerSends)
    , Field 4 0 "pSendStartCoV" (p!!9) "ing  " $ DDeltaT (snd . bpPeerSends)
    ]
   where
     f = nChunksEachOf  4 7 "Forger event Δt:"
     p = nChunksEachOf 10 5 "Peer event Δt, and coefficients of variation:"

instance AE.ToJSON BlockPropagation where
  toJSON BlockPropagation{..} = AE.Array $ Vec.fromList
    [ extendObject "kind" "forgerForges"        $ toJSON bpForgerForges
    , extendObject "kind" "forgerAdoptions"     $ toJSON bpForgerAdoptions
    , extendObject "kind" "forgerAnnouncements" $ toJSON bpForgerAnnouncements
    , extendObject "kind" "forgerSends"         $ toJSON bpForgerSends
    , extendObject "kind" "peerNoticesMean"       $ toJSON (fst bpPeerNotices)
    , extendObject "kind" "peerNoticesCoV"        $ toJSON (snd bpPeerNotices)
    , extendObject "kind" "peerFetchesMean"       $ toJSON (fst bpPeerFetches)
    , extendObject "kind" "peerFetchesCoV"        $ toJSON (snd bpPeerFetches)
    , extendObject "kind" "peerAdoptionsMean"     $ toJSON (fst bpPeerAdoptions)
    , extendObject "kind" "peerAdoptionsCoV"      $ toJSON (snd bpPeerAdoptions)
    , extendObject "kind" "peerAnnouncementsMean" $ toJSON (fst bpPeerAnnouncements)
    , extendObject "kind" "peerAnnouncementsCoV"  $ toJSON (snd bpPeerAnnouncements)
    , extendObject "kind" "peerSendsMean"         $ toJSON (fst bpPeerSends)
    , extendObject "kind" "peerSendsCoV"          $ toJSON (snd bpPeerSends)
    ]

-- | Block's events, as seen by its forger.
data BlockForgerEvents
  =  BlockForgerEvents
  { bfeHost       :: !Host
  , bfeBlock      :: !Hash
  , bfeBlockPrev  :: !Hash
  , bfeBlockNo    :: !BlockNo
  , bfeSlotNo     :: !SlotNo
  , bfeSlotStart  :: !UTCTime
  , bfeForged     :: !(Maybe NominalDiffTime)
  , bfeAdopted    :: !(Maybe NominalDiffTime)
  , bfeChainDelta :: !Int
  , bfeAnnounced  :: !(Maybe NominalDiffTime)
  , bfeSending    :: !(Maybe NominalDiffTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

bfePrevBlock :: BlockForgerEvents -> Maybe Hash
bfePrevBlock x = case bfeBlockNo x of
  0 -> Nothing
  _ -> Just $ bfeBlockPrev x

-- | Block's events, as seen by an observer.
data BlockObserverEvents
  =  BlockObserverEvents
  { boeHost       :: !Host
  , boeBlock      :: !Hash
  , boeBlockNo    :: !BlockNo
  , boeSlotNo     :: !SlotNo
  , boeSlotStart  :: !UTCTime
  , boeNoticed    :: !(Maybe NominalDiffTime)
  , boeFetched    :: !(Maybe NominalDiffTime)
  , boeAdopted    :: !(Maybe NominalDiffTime)
  , boeChainDelta :: !Int
  , boeAnnounced  :: !(Maybe NominalDiffTime)
  , boeSending    :: !(Maybe NominalDiffTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | Sum of observer and forger events alike.
type MachBlockEvents = Either BlockObserverEvents BlockForgerEvents

ordBlockEv :: MachBlockEvents -> MachBlockEvents -> Ordering
ordBlockEv l r
  | (on (>) $ either boeBlockNo bfeBlockNo) l r = GT
  | (on (>) $ either boeBlockNo bfeBlockNo) r l = LT
  | isRight l = GT
  | isRight r = LT
  | otherwise = EQ

mbeSlotStart :: MachBlockEvents -> UTCTime
mbeSlotStart = either boeSlotStart bfeSlotStart

mbeNoticed, mbeAcquired, mbeAdopted, mbeAnnounced, mbeSending :: MachBlockEvents -> Maybe NominalDiffTime
mbeNoticed   = either boeNoticed   (const $ Just 0)
mbeAcquired  = either boeFetched   bfeForged
mbeAdopted   = either boeAdopted   bfeAdopted
mbeAnnounced = either boeAnnounced bfeAnnounced
mbeSending   = either boeSending   bfeSending

mbeBlockHash :: MachBlockEvents -> Hash
mbeBlockHash = either boeBlock bfeBlock

mbeSetAdopted :: MachBlockEvents -> Int -> NominalDiffTime -> MachBlockEvents
mbeSetAdopted   mbe d v = either (\x -> Left x { boeAdopted=Just v, boeChainDelta=d })   (\x -> Right x { bfeAdopted=Just v, bfeChainDelta=d }) mbe

mbeSetAnnounced, mbeSetSending, mbeSetNoticed, mbeSetFetched :: MachBlockEvents -> NominalDiffTime -> MachBlockEvents
mbeSetAnnounced mbe v = either (\x -> Left x { boeAnnounced=Just v }) (\x -> Right x { bfeAnnounced=Just v }) mbe
mbeSetSending   mbe v = either (\x -> Left x { boeSending=Just v })   (\x -> Right x { bfeSending=Just v }) mbe
mbeSetNoticed   mbe v = either (\x -> Left x { boeNoticed=Just v })   (const $ error "mbeSetNoticed on a BFE.") mbe
mbeSetFetched   mbe v = either (\x -> Left x { boeFetched=Just v })   (const $ error "mbeSetFetched on a BFE.") mbe

-- | Machine's private view of all the blocks.
type MachBlockMap
  =  Map.Map Hash MachBlockEvents

blockMapHost :: MachBlockMap -> Host
blockMapHost = either boeHost bfeHost . head . Map.elems

blockMapMaxBlock :: MachBlockMap -> MachBlockEvents
blockMapMaxBlock = maximumBy ordBlockEv . Map.elems

blockMapBlock :: Hash -> MachBlockMap -> MachBlockEvents
blockMapBlock h =
  fromMaybe (error $ "Invariant failed:  missing hash " <> show h) . Map.lookup h

-- | A completed, compactified version of BlockObserverEvents.
data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !UTCTime
  , boNoticed    :: !NominalDiffTime
  , boFetched    :: !NominalDiffTime
  , boAdopted    :: !(Maybe NominalDiffTime)
  , boChainDelta :: !Int -- ^ ChainDelta during adoption
  , boAnnounced  :: !(Maybe NominalDiffTime)
  , boSending    :: !(Maybe NominalDiffTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beForger       :: !Host
  , beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beSlotStart    :: !UTCTime
  , beForged       :: !NominalDiffTime
  , beAdopted      :: !NominalDiffTime
  , beChainDelta   :: !Int -- ^ ChainDelta during adoption
  , beAnnounced    :: !NominalDiffTime
  , beSending      :: !NominalDiffTime
  , beObservations :: [BlockObservation]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

instance RenderTimeline BlockEvents where
  rtFields =
    --  Width LeftPad
    [ Field 5 0 "block"     "block" "no."     $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"  "abs."  "slot#"   $ IWord64 (unSlotNo . beSlotNo)
    ]
   where
     f w = nChunksEachOf 4 (w + 1) "Block forging events"
     p w = nChunksEachOf 4 (w + 1) "Non-forging peer observation events"

mapChainToForgerEventCDF ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockEvents -> Maybe NominalDiffTime)
  -> Distribution Float NominalDiffTime
mapChainToForgerEventCDF percs cbe proj =
  computeDistribution percs (mapMaybe proj cbe)

mapChainToPeerBlockObservationCDFs ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockObservation -> Maybe NominalDiffTime)
  -> String
  -> (Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
mapChainToPeerBlockObservationCDFs percs cbe proj desc =
  (means, covs)
 where
   means, covs :: Distribution Float NominalDiffTime
   (,) means covs = computeDistributionStats desc
                      (fmap realToFrac <$> allDistributions)
                    & either error id
                    & join (***) (fmap realToFrac)

   allDistributions :: [Distribution Float NominalDiffTime]
   allDistributions = computeDistribution percs <$> allObservations

   allObservations :: [[NominalDiffTime]]
   allObservations = blockObservations <$> cbe

   blockObservations :: BlockEvents -> [NominalDiffTime]
   blockObservations be = mapMaybe proj (beObservations be)

blockProp :: ChainInfo -> [(JsonLogfile, [LogObject])] -> IO BlockPropagation
blockProp ci xs = do
  putStrLn ("blockProp: recovering block event maps" :: String)
  doBlockProp =<< mapConcurrently (pure . blockEventsFromLogObjects ci) xs

doBlockProp :: [MachBlockMap] -> IO BlockPropagation
doBlockProp eventMaps = do
  putStrLn ("tip block: " <> show tipBlock :: String)
  putStrLn ("chain length: " <> show (length chain) :: String)
  pure $ BlockPropagation
    (forgerEventsCDF    (Just . beForged))
    (forgerEventsCDF    (\x -> if beChainDelta x == 1 then Just (beAdopted x)
                               else Nothing))
    (forgerEventsCDF    (Just . beAnnounced))
    (forgerEventsCDF    (Just . beSending))
    (observerEventsCDFs (Just . boNoticed) "peer noticed")
    (observerEventsCDFs (Just . boFetched) "peer fetched")
    (observerEventsCDFs (\x -> if boChainDelta x == 1 then boAdopted x
                               else Nothing) "peer adopted")
    (observerEventsCDFs boAnnounced "peer announced")
    (observerEventsCDFs boSending   "peer sending")
    chain
 where
   forgerEventsCDF    = mapChainToForgerEventCDF           stdPercentiles chain
   observerEventsCDFs = mapChainToPeerBlockObservationCDFs stdPercentiles chain

   chain          = rebuildChain eventMaps tipHash
   tipBlock       = getBlockForge eventMaps tipHash
   tipHash        = rewindChain eventMaps 1 (mbeBlockHash finalBlockEv)
   finalBlockEv   = maximumBy ordBlockEv $ blockMapMaxBlock <$> eventMaps

   rewindChain :: [MachBlockMap] -> Int -> Hash -> Hash
   rewindChain eventMaps count tip = go tip count
    where go tip = \case
            0 -> tip
            n -> go (bfeBlockPrev $ getBlockForge eventMaps tip) (n - 1)

   getBlockForge :: [MachBlockMap] -> Hash -> BlockForgerEvents
   getBlockForge xs h =
     either (error "Invariant failed: finalBlockEv isn't a forge.") id $
            maximumBy ordBlockEv $
            mapMaybe (Map.lookup h) xs

   rebuildChain :: [MachBlockMap] -> Hash -> [BlockEvents]
   rebuildChain machBlockMaps tip = go (Just tip) []
    where go Nothing  acc = acc
          go (Just h) acc =
            let blkEvs@(forgerEv, _) = collectAllBlockEvents machBlockMaps h
            in go (bfePrevBlock forgerEv)
                  (liftBlockEvents blkEvs : acc)

   collectAllBlockEvents :: [MachBlockMap] -> Hash -> (BlockForgerEvents, [BlockObserverEvents])
   collectAllBlockEvents xs blk =
     partitionEithers (mapMaybe (Map.lookup blk) xs)
     & swap & first head

   liftBlockEvents :: (BlockForgerEvents, [BlockObserverEvents]) -> BlockEvents
   liftBlockEvents (BlockForgerEvents{..}, os) =
     BlockEvents
     { beForger     = bfeHost
     , beBlock      = bfeBlock
     , beBlockPrev  = bfeBlockPrev
     , beBlockNo    = bfeBlockNo
     , beSlotNo     = bfeSlotNo
     , beSlotStart  = bfeSlotStart
     , beForged     = bfeForged    & miss "Forged"
     , beAdopted    = bfeAdopted   & miss "Adopted (forger)"
     , beChainDelta = bfeChainDelta
     , beAnnounced  = bfeAnnounced & miss "Announced (forger)"
     , beSending    = bfeSending   & miss "Sending (forger)"
     , beObservations = catMaybes $
       os <&> \BlockObserverEvents{..}->
         BlockObservation
           <$> Just boeHost
           <*> Just bfeSlotStart
           <*> boeNoticed
           <*> boeFetched
           <*> Just boeAdopted
           <*> Just boeChainDelta
           <*> Just boeAnnounced
           <*> Just boeSending
     }
    where
      miss :: String -> Maybe a -> a
      miss slotDesc = fromMaybe $ error $ mconcat
       [ "While processing ", show bfeBlockNo, " hash ", show bfeBlock
       , " forged by ", show bfeHost
       , " -- missing slot: ", slotDesc
       ]

-- | Given a single machine's log object stream, recover its block map.
blockEventsFromLogObjects :: ChainInfo -> (JsonLogfile, [LogObject]) -> MachBlockMap
blockEventsFromLogObjects ci (fp, xs) =
  foldl (blockPropMachEventsStep ci fp) mempty xs

blockPropMachEventsStep :: ChainInfo -> JsonLogfile -> MachBlockMap -> LogObject -> MachBlockMap
blockPropMachEventsStep ci fp bMap = \case
  LogObject{loAt, loHost, loBody=LOBlockForged{loBlock,loPrev,loBlockNo,loSlotNo}} ->
    mbmGetForger loHost loBlock bMap "LOBlockForged"
    & fromMaybe
      (Right $ BlockForgerEvents
        loHost loBlock loPrev loBlockNo loSlotNo (slotStart ci loSlotNo)
        (Just $ loAt `diffUTCTime` slotStart ci loSlotNo) Nothing 0 Nothing Nothing)
    & doInsert loBlock
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = Map.lookup loBlock bMap
    in if isJust mbe0 then bMap else
      (Left $
       BlockObserverEvents
         loHost loBlock loBlockNo loSlotNo (slotStart ci loSlotNo)
         (Just $ loAt `diffUTCTime` slotStart ci loSlotNo) Nothing Nothing 0 Nothing Nothing)
      & doInsert loBlock
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    let mbe0 = Left $ mbmGetObserver loHost loBlock bMap "LOBlockFetchClientCompletedFetch"
    in
      mDeltaT loAt mbe0 [mbeNoticed]
      & fromMaybe (err loHost loBlock "LOBlockFetchClientCompletedFetch leads fetching")
      & mbeSetFetched mbe0
      & doInsert loBlock
  LogObject{loAt, loHost, loBody=LOBlockAddedToCurrentChain{loBlock,loChainLengthDelta}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (err loHost loBlock "LOBlockAddedToCurrentChain leads")
    in if isJust (mbeAdopted mbe0) then bMap else
      mDeltaT loAt mbe0 [mbeNoticed, mbeAcquired]
      & fromMaybe (err loHost loBlock "LOBlockAddedToCurrentChain leads acquirement")
      & mbeSetAdopted mbe0 loChainLengthDelta
      & doInsert loBlock
  LogObject{loAt, loHost, loBody=LOChainSyncServerSendHeader{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (err loHost loBlock "LOChainSyncServerSendHeader leads")
    in if isJust (mbeAnnounced mbe0) then bMap else
      mDeltaT loAt mbe0 [mbeNoticed, mbeAcquired, mbeAdopted]
      & fromMaybe (err loHost loBlock "LOChainSyncServerSendHeader leads adoption")
      & mbeSetAnnounced mbe0
      & doInsert loBlock
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSending{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (err loHost loBlock "LOBlockFetchServerSending leads")
    in if isJust (mbeSending mbe0) then bMap else
      mDeltaT loAt mbe0 [mbeNoticed, mbeAcquired, mbeAdopted, mbeAnnounced]
      & fromMaybe (err loHost loBlock "LOBlockFetchServerSending leads announcement")
      & mbeSetSending mbe0
      & doInsert loBlock
  _ -> bMap
 where
   doInsert :: Hash -> MachBlockEvents -> MachBlockMap
   doInsert k x = Map.insert k x bMap

   mDeltaT :: UTCTime -> MachBlockEvents -> [MachBlockEvents -> Maybe NominalDiffTime] -> Maybe NominalDiffTime
   mDeltaT t mbe mdtProjs =
     (t `diffUTCTime`) <$> foldM (\tv mdt -> flip addUTCTime tv <$> mdt)
                                 (mbeSlotStart mbe)
                                 (($ mbe) <$> mdtProjs)

   err :: Host -> Hash -> String -> a
   err ho ha desc = error $ mconcat
     [ "In file ", show fp
     , ", for host ", show ho
     , ", for block ", show ha
     , ": ", desc
     ]

   mbmGetForger :: Host -> Hash -> MachBlockMap -> String -> Maybe MachBlockEvents
   mbmGetForger ho ha m eDesc = Map.lookup ha m <&>
     either (const $ err ho ha (eDesc <> " after a BlockObserverEvents")) Right

   mbmGetObserver :: Host -> Hash -> MachBlockMap -> String -> BlockObserverEvents
   mbmGetObserver ho ha m eDesc = case Map.lookup ha m of
     Just (Left x) -> x
     Just (Right x) -> err ho ha (eDesc <> " after a BlockForgerEvents")
     Nothing ->  err ho ha (eDesc <> " leads")
