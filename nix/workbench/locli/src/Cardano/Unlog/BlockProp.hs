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
module Cardano.Unlog.BlockProp (module Cardano.Unlog.BlockProp) where

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

import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Time

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
    }
  deriving Show

instance RenderDistributions BlockPropagation where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 (f!!0) "Forge"   $ DDeltaT bpForgerForges
    , Field 6 0 (f!!1) "Adopt"   $ DDeltaT bpForgerAdoptions
    , Field 6 0 (f!!2) "Ann-ce"  $ DDeltaT bpForgerAnnouncements
    , Field 6 0 (f!!3) "Sent"    $ DDeltaT bpForgerSends
    , Field 4 1 (p!!0) " Noti" $ DDeltaT (fst . bpPeerNotices)
    , Field 4 0 (p!!1) "ced  " $ DDeltaT (snd . bpPeerNotices)
    , Field 4 1 (p!!2) " Fetc" $ DDeltaT (fst . bpPeerFetches)
    , Field 4 0 (p!!3) "hed  " $ DDeltaT (snd . bpPeerFetches)
    , Field 4 1 (p!!4) " Adop" $ DDeltaT (fst . bpPeerAdoptions)
    , Field 4 0 (p!!5) "ted  " $ DDeltaT (snd . bpPeerAdoptions)
    , Field 4 1 (p!!6) "Annou" $ DDeltaT (fst . bpPeerAnnouncements)
    , Field 4 0 (p!!7) "nced " $ DDeltaT (snd . bpPeerAnnouncements)
    , Field 4 1 (p!!8) "   Se" $ DDeltaT (fst . bpPeerSends)
    , Field 4 0 (p!!9) "nt   " $ DDeltaT (snd . bpPeerSends)
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
  , bfeForged     :: !(Maybe UTCTime)
  , bfeAdopted    :: !(Maybe UTCTime)
  , bfeChainDelta :: !Int
  , bfeAnnounced  :: !(Maybe UTCTime)
  , bfeSent       :: !(Maybe UTCTime)
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
  , boeNoticed    :: !(Maybe UTCTime)
  , boeFetched    :: !(Maybe UTCTime)
  , boeAdopted    :: !(Maybe UTCTime)
  , boeChainDelta :: !Int
  , boeAnnounced  :: !(Maybe UTCTime)
  , boeSent       :: !(Maybe UTCTime)
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

mbeAdopted, mbeAnnounced, mbeSent, mbeNoticed :: MachBlockEvents -> Maybe UTCTime
mbeAdopted = \case
  Left  BlockObserverEvents{..} -> boeAdopted
  Right BlockForgerEvents  {..} -> bfeAdopted
mbeAnnounced = \case
  Left  BlockObserverEvents{..} -> boeAnnounced
  Right BlockForgerEvents  {..} -> bfeAnnounced
mbeSent = \case
  Left  BlockObserverEvents{..} -> boeSent
  Right BlockForgerEvents  {..} -> bfeSent
mbeNoticed = \case
  Left  BlockObserverEvents{..} -> boeNoticed
  Right BlockForgerEvents  {}   -> error "Called mbeNoticed on a BFE."

mbeBlockHash :: MachBlockEvents -> Hash
mbeBlockHash = either boeBlock bfeBlock

mbeSetAdoptedDelta :: UTCTime -> Int -> MachBlockEvents -> MachBlockEvents
mbeSetAdoptedDelta   v d = either (\x -> Left x { boeAdopted=Just v, boeChainDelta=d })   (\x -> Right x { bfeAdopted=Just v, bfeChainDelta=d })

mbeSetAnnounced, mbeSetSent :: UTCTime -> MachBlockEvents -> MachBlockEvents
mbeSetAnnounced v = either (\x -> Left x { boeAnnounced=Just v }) (\x -> Right x { bfeAnnounced=Just v })
mbeSetSent      v = either (\x -> Left x { boeSent=Just v })      (\x -> Right x { bfeSent=Just v })

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
  , boNoticed    :: !UTCTime
  , boFetched    :: !UTCTime
  , boAdopted    :: !(Maybe UTCTime)
  , boChainDelta :: !Int -- ^ ChainDelta during adoption
  , boAnnounced  :: !(Maybe UTCTime)
  , boSent       :: !(Maybe UTCTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beForger       :: !Host
  , beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beSlotStart    :: !UTCTime
  , beForged       :: !UTCTime
  , beAdopted      :: !UTCTime
  , beChainDelta   :: !Int -- ^ ChainDelta during adoption
  , beAnnounced    :: !UTCTime
  , beSent         :: !UTCTime
  , beObservations :: [BlockObservation]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON)

-- | Ordered list of all block events of a chain.
type ChainBlockEvents
  =  [BlockEvents]

mapChainToForgerEventCDF ::
     [PercSpec Float]
  -> ChainBlockEvents
  -> (BlockEvents -> Maybe UTCTime)
  -> Distribution Float NominalDiffTime
mapChainToForgerEventCDF percs cbe proj =
  computeDistribution percs (mapMaybe blockDelta cbe)
 where
   blockDelta :: BlockEvents -> Maybe NominalDiffTime
   blockDelta be = (`Time.diffUTCTime` beSlotStart be) <$> proj be

mapChainToPeerBlockObservationCDFs ::
     [PercSpec Float]
  -> ChainBlockEvents
  -> (BlockObservation -> Maybe UTCTime)
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
   blockObservations be =
     (`Time.diffUTCTime` beSlotStart be) <$> mapMaybe proj (beObservations be)

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
    (forgerEventsCDF    (Just . beSent))
    (observerEventsCDFs (Just . boNoticed) "peer noticed")
    (observerEventsCDFs (Just . boFetched) "peer fetched")
    (observerEventsCDFs (\x -> if boChainDelta x == 1 then boAdopted x
                               else Nothing) "peer adopted")
    (observerEventsCDFs boAnnounced "peer announced")
    (observerEventsCDFs boSent      "peer sent")
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

   rebuildChain :: [MachBlockMap] -> Hash -> ChainBlockEvents
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
     , beSent       = bfeSent      & miss "Sent (forger)"
     , beObservations = catMaybes $
       os <&> \BlockObserverEvents{..}->
         BlockObservation
           <$> Just boeHost
           <*> boeNoticed
           <*> boeFetched
           <*> Just boeAdopted
           <*> Just boeChainDelta
           <*> Just boeAnnounced
           <*> Just boeSent
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
    flip (Map.insert loBlock) bMap $
    Right (mbmGetForger loHost loBlock bMap "LOBlockForged"
           & fromMaybe (mbe0forger loHost loBlock loPrev loBlockNo loSlotNo))
          { bfeForged = Just loAt }
  LogObject{loAt, loHost, loBody=LOBlockAddedToCurrentChain{loBlock,loChainLengthDelta}} ->
    let mbe0 = Map.lookup loBlock bMap & fromMaybe
               (err loHost loBlock "LOBlockAddedToCurrentChain leads")
    in if isJust $ mbeAdopted mbe0 then bMap
       else Map.insert loBlock (mbeSetAdoptedDelta loAt loChainLengthDelta mbe0) bMap
  LogObject{loAt, loHost, loBody=LOChainSyncServerSendHeader{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap & fromMaybe
               (err loHost loBlock "LOChainSyncServerSendHeader leads")
    in if isJust $ mbeAnnounced mbe0 then bMap
       else Map.insert loBlock (mbeSetAnnounced loAt mbe0) bMap
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSend{loBlock}} ->
    -- D.trace (printf "mbeSetSent %s %s" (show loHost :: String) (show loBlock :: String)) $
    let mbe0 = Map.lookup loBlock bMap & fromMaybe
               (err loHost loBlock "LOBlockFetchServerSend leads")
    in if isJust $ mbeSent mbe0 then bMap
       else Map.insert loBlock (mbeSetSent loAt mbe0) bMap
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = Map.lookup loBlock bMap
    in if isJust mbe0 then bMap
       else Map.insert loBlock
              (Left $
               (mbe0observ loHost loBlock loBlockNo loSlotNo)
               { boeNoticed = Just loAt })
              bMap
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    flip (Map.insert loBlock) bMap $
    Left (mbmGetObserver loHost loBlock bMap  "LOBlockFetchClientCompletedFetch")
         { boeFetched = Just loAt }
  _ -> bMap
 where
   err :: Host -> Hash -> String -> a
   err ho ha desc = error $ mconcat
     [ "In file ", show fp
     , ", for host ", show ho
     , ", for block ", show ha
     , ": ", desc
     ]
   mbe0observ :: Host -> Hash -> BlockNo -> SlotNo -> BlockObserverEvents
   mbe0observ ho ha bn sn =
     BlockObserverEvents ho ha bn sn (slotStart ci sn)
     Nothing Nothing Nothing 0 Nothing Nothing
   mbe0forger :: Host -> Hash -> Hash -> BlockNo -> SlotNo -> BlockForgerEvents
   mbe0forger ho ha hp bn sn =
     BlockForgerEvents ho ha hp bn sn (slotStart ci sn)
     Nothing Nothing 0 Nothing Nothing
   mbmGetObserver :: Host -> Hash -> MachBlockMap -> String -> BlockObserverEvents
   mbmGetObserver ho ha m eDesc = case Map.lookup ha m of
     Just (Left x) -> x
     Just (Right x) -> err ho ha (eDesc <> " after a BlockForgerEvents")
     Nothing ->  err ho ha (eDesc <> " leads")
   mbmGetForger :: Host -> Hash -> MachBlockMap -> String -> Maybe BlockForgerEvents
   mbmGetForger ho ha m eDesc = Map.lookup ha m <&>
     either (const $ err ho ha (eDesc <> " after a BlockObserverEvents")) id
