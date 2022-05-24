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
module Cardano.Analysis.BlockProp
  (MachView, buildMachViews, rebuildChain, filterChain, blockProp)
where

import Prelude                  (String, (!!), error, head, last, id, show, tail)
import Cardano.Prelude          hiding (head, show)

import Control.Arrow            ((***), (&&&))
import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Bifunctor
import Data.Function            (on)
import Data.List                (dropWhileEnd, intercalate)
import Data.Map.Strict          (Map)
import Data.Map.Strict          qualified as Map
import Data.Maybe               (catMaybes, mapMaybe, isNothing)
import Data.Set                 (Set)
import Data.Set                 qualified as Set
import Data.Text                qualified as T
import Data.Text.Short          (toText)
import Data.Tuple               (swap)
import Data.Vector              (Vector)
import Data.Vector              qualified as Vec

import Data.Time.Clock          (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

import Text.Printf              (printf)

import Cardano.Slotting.Slot    (EpochNo(..), SlotNo(..))
import Ouroboros.Network.Block  (BlockNo(..))

import Data.Accum
import Data.Distribution

import Cardano.Analysis.API
import Cardano.Analysis.Chain
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Ground
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.LogObject  hiding (Text)
import Cardano.Unlog.Render
import Cardano.Unlog.Resources
import Cardano.Util


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
  , bfeBlockSize    :: !(Maybe Int)
  , bfeChecked      :: !(Maybe a)
  , bfeLeading      :: !(Maybe a)
  , bfeForged       :: !(Maybe a)
  , bfeAdopted      :: !(Maybe a)
  , bfeChainDelta   :: !Int
  , bfeAnnounced    :: !(Maybe a)
  , bfeSending      :: !(Maybe a)
  , bfeErrs         :: [BPError]
  }
  deriving (Generic, NFData, FromJSON, ToJSON, Show)

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
  , boeErrorsCrit :: [BPError]
  , boeErrorsSoft :: [BPError]
  }
  deriving (Generic, NFData, FromJSON, ToJSON, Show)

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
  deriving (Generic, NFData, FromJSON, ToJSON)

mbeForgP, mbeObsvP :: MachBlockEvents a -> Bool
mbeForgP = \case
  MFE{} -> True
  _ -> False
mbeObsvP = \case
  MOE{} -> True
  _ -> False

mapMbe :: (ForgerEvents a -> b) -> (ObserverEvents a -> b) -> (BPError -> b)
       -> MachBlockEvents a -> b
mapMbe f o e = \case
  MFE x -> f x
  MOE x -> o x
  MBE x -> e x

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

-- | Machine's private view of all the blocks.
type MachBlockMap a
  =  Map.Map Hash (MachBlockEvents a)

data MachView
  = MachView
  { mvBlocks  :: !(MachBlockMap UTCTime)
  , mvChecked :: Maybe UTCTime
  , mvLeading :: Maybe UTCTime
  }
  deriving (FromJSON, Generic, NFData, ToJSON)

blockMapMaxBlock :: MachBlockMap a -> MachBlockEvents a
blockMapMaxBlock = maximumBy ordBlockEv . Map.elems

beForgedAt :: BlockEvents -> UTCTime
beForgedAt BlockEvents{beForge=BlockForge{..}} =
  bfForged `afterSlot` bfSlotStart

mapChainToBlockEventCDF ::
  (Real a, ToRealFrac a Double)
  => [PercSpec Double]
  -> [BlockEvents]
  -> (BlockEvents -> Maybe a)
  -> Distribution Double a
mapChainToBlockEventCDF percs cbes proj =
  computeDistribution percs $ mapMaybe proj cbes

mapChainToPeerBlockObservationCDF ::
     [PercSpec Double]
  -> [BlockEvents]
  -> (BlockObservation -> Maybe NominalDiffTime)
  -> String
  -> Distribution Double NominalDiffTime
mapChainToPeerBlockObservationCDF percs cbes proj desc =
  computeDistribution percs allObservations
 where
   allObservations :: [NominalDiffTime]
   allObservations =
     concat $ cbes <&> blockObservations

   blockObservations :: BlockEvents -> [NominalDiffTime]
   blockObservations be =
     proj `mapMaybe` filter isValidBlockObservation (beObservations be)

buildMachViews :: Run -> [(JsonLogfile, [LogObject])] -> IO [(JsonLogfile, MachView)]
buildMachViews run = mapConcurrentlyPure (fst &&& blockEventMapsFromLogObjects run)

rebuildChain :: Run -> [(JsonLogfile, MachView)] -> IO [BlockEvents]
rebuildChain run@Run{genesis} xs@(fmap snd -> machViews) = do
  progress "tip" $ Q $ show $ bfeBlock tipBlock
  pure
    $ computeChainBlockGaps
    $ doRebuildChain (fmap deltifyEvents <$> eventMaps) tipHash
 where
   eventMaps      = mvBlocks <$> machViews

   finalBlockEv   = maximumBy ordBlockEv $ blockMapMaxBlock <$> eventMaps

   tipHash        = rewindChain eventMaps 1 (mbeBlock finalBlockEv)
   tipBlock       = getBlockForge eventMaps tipHash

   computeChainBlockGaps :: [BlockEvents] -> [BlockEvents]
   computeChainBlockGaps [] = error "computeChainBlockGaps on an empty chain"
   computeChainBlockGaps lst@(hd:_) =
     snd $ mapAccumL step (unSlotStart . bfSlotStart $ beForge hd) lst
    where
      step :: UTCTime -> BlockEvents -> (UTCTime, BlockEvents)
      step prevForge x@(beForgedAt -> at) =
        (at, x { beForge = (beForge x) { bfBlockGap = at `diffUTCTime` prevForge } })

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

   doRebuildChain :: [MachBlockMap NominalDiffTime] -> Hash -> [BlockEvents]
   doRebuildChain machBlockMaps tip = go (Just tip) []
    where go Nothing  acc = acc
          go (Just h) acc =
            case partitionMbes $ mapMaybe (Map.lookup h) machBlockMaps of
              ([], _, ers) -> error $ mconcat
                [ "No forger for hash ", show h
                , "\nErrors:\n"
                ] ++ intercalate "\n" (show <$> ers)
              blkEvs@(forgerEv:_, oEvs, ers) ->
                go (bfePrevBlock forgerEv) (liftBlockEvents forgerEv oEvs ers : acc)

   liftBlockEvents :: ForgerEvents NominalDiffTime -> [ObserverEvents NominalDiffTime] -> [BPError] -> BlockEvents
   liftBlockEvents ForgerEvents{bfeHost=host, ..} os errs =
     BlockEvents
     { beBlock        = bfeBlock
     , beBlockPrev    = bfeBlockPrev
     , beBlockNo      = bfeBlockNo
     , beSlotNo       = bfeSlotNo
     , beEpochNo      = bfeEpochNo
     , beEpochSafeInt = slotEpochSafeInt genesis (snd $ genesis `unsafeParseSlot` bfeSlotNo)
     , beForge =
       BlockForge
       { bfForger     = host
       , bfSlotStart  = bfeSlotStart
       , bfBlockGap   = 0 -- To be filled in after chain is rebuilt.
       , bfBlockSize  = bfeBlockSize & handleMiss "Size"
       , bfChecked    = bfeChecked   & handleMiss "Δt Checked"
       , bfLeading    = bfeLeading   & handleMiss "Δt Leading"
       , bfForged     = bfeForged    & handleMiss "Δt Forged"
       , bfAdopted    = bfeAdopted   & handleMiss "Δt Adopted (forger)"
       , bfChainDelta = bfeChainDelta
       , bfAnnounced  = (bfeAnnounced <|> Just 0.05) -- Temporary hack until ChainSync tracing is fixed
                        & handleMiss "Δt Announced (forger)"
       , bfSending    = bfeSending   & handleMiss "Δt Sending (forger)"
       }
     , beObservations =
         catMaybes $
         os <&> \ObserverEvents{..}->
           BlockObservation
             <$> Just boeHost
             <*> Just bfeSlotStart
             <*> boeNoticed
             <*> boeRequested
             <*> boeFetched
             <*> Just boeAdopted
             <*> Just boeChainDelta
             <*> Just boeAnnounced
             <*> Just boeSending
             <*> Just boeErrorsCrit
             <*> Just boeErrorsSoft
     , bePropagation  = computeDistribution adoptionPcts adoptions
     , beOtherBlocks  = otherBlocks
     , beErrors =
         errs
         <> (otherBlocks <&>
             \blk ->
               fail' (findForger blk) bfeBlock $ BPEFork blk)
         <> bfeErrs
         <> concatMap boeErrorsCrit os
         <> concatMap boeErrorsSoft os
     }
    where
      adoptions    =
        (fmap (`sinceSlot` bfeSlotStart) . Map.lookup bfeBlock) `mapMaybe` adoptionMap

      otherBlocks = Map.lookup bfeBlockNo heightMap
                    & handleMiss "height map"
                    & Set.delete bfeBlock
                    & Set.toList

      findForger :: Hash -> Host
      findForger hash =
        maybe
          (Host "?")
          (mapMbe bfeHost (error "Invariant failed") (error "Invariant failed"))
          (mapMaybe (Map.lookup hash) eventMaps
           & find mbeForgP)

      fail' :: Host -> Hash -> BPErrorKind -> BPError
      fail' host hash desc = BPError host hash Nothing desc

      handleMiss :: String -> Maybe a -> a
      handleMiss slotDesc = fromMaybe $ error $ mconcat
       [ "While processing ", show bfeBlockNo, " hash ", show bfeBlock
       , " forged by ", show (unHost host)
       , " -- missing: ", slotDesc
       ]

filterChain :: Run -> ([ChainFilter], [FilterName]) -> [BlockEvents]
            -> IO (DataDomain SlotNo, DataDomain BlockNo, [BlockEvents])
filterChain Run{genesis} (flts, fltNames) chain = do
  progress "filtered-chain-slot-domain"  $ J domSlot
  progress "filtered-chain-block-domain" $ J domBlock
  pure (domSlot, domBlock, fltrd)
 where
   fltrd = filter (isValidBlockEvent genesis flts) chain &
           \case
             [] -> error $ mconcat
                   ["All ", show (length chain)
                   , " blocks dropped by chain filters: ", show flts]
             xs -> xs
   (blk0,  blkL)  = (head chain, last chain)
   (blk0V, blkLV) = (head fltrd, last fltrd)
   domSlot = DataDomain
             (blk0  & beSlotNo)  (blkL  & beSlotNo)
             (blk0V & beSlotNo)  (blkLV & beSlotNo)
             (fromIntegral . unSlotNo $ beSlotNo blkL  - beSlotNo blk0)
             (fromIntegral . unSlotNo $ beSlotNo blkLV - beSlotNo blk0V)
   domBlock = DataDomain
              (blk0  & beBlockNo) (blkL  & beBlockNo)
              (blk0V & beBlockNo) (blkLV & beBlockNo)
              (length chain) (length fltrd)

blockProp :: Run -> [BlockEvents] -> DataDomain SlotNo -> DataDomain BlockNo -> IO BlockPropagation
blockProp run@Run{genesis} chain domSlot domBlock = do
  progress "block-propagation" $ J (domSlot, domBlock)
  pure $ BlockPropagation
    { bpDomainSlots         = domSlot
    , bpDomainBlocks        = domBlock
    , bpForgerChecks        = forgerEventsCDF   (Just . bfChecked   . beForge)
    , bpForgerLeads         = forgerEventsCDF   (Just . bfLeading   . beForge)
    , bpForgerForges        = forgerEventsCDF   (Just . bfForged    . beForge)
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
    , bpPropagation         =
      [ (p', forgerEventsCDF (Just . dPercSpec' "bePropagation" p . bePropagation))
      | p@(Perc p') <- adoptionPcts <> [Perc 1.0] ]
    , bpSizes               = forgerEventsCDF   (Just . bfBlockSize . beForge)
    , bpVersion             = getVersion
    }
 where
   forgerEventsCDF   :: (Real a, ToRealFrac a Double) => (BlockEvents -> Maybe a) -> Distribution Double a
   forgerEventsCDF   = mapChainToBlockEventCDF           stdPercSpecs chain
   observerEventsCDF = mapChainToPeerBlockObservationCDF stdPercSpecs chain

-- | Given a single machine's log object stream, recover its block map.
blockEventMapsFromLogObjects :: Run -> (JsonLogfile, [LogObject]) -> MachView
blockEventMapsFromLogObjects run (f@(unJsonLogfile -> fp), xs) =
  trace ("processing " <> fp)
  $ if Map.size (mvBlocks view) == 0
    then error $ mconcat
         ["No block events in ",fp," : ","LogObject count: ",show (length xs)]
    else view
 where
   view = foldl' (blockPropMachEventsStep run f) initial xs
   initial =
     MachView
     { mvBlocks  = mempty
     , mvChecked = Nothing
     , mvLeading = Nothing
     }

blockPropMachEventsStep :: Run -> JsonLogfile -> MachView -> LogObject -> MachView
blockPropMachEventsStep run@Run{genesis} (JsonLogfile fp) mv@MachView{..} lo = case lo of
  -- 0. Notice (observer only)
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = getBlock loBlock
    in if isJust mbe0 then mv else
      MOE
       (ObserverEvents
        loHost loBlock loBlockNo loSlotNo
        (slotStart genesis loSlotNo) (Just loAt)
        Nothing Nothing Nothing 0 Nothing Nothing [] [])
      & doInsert loBlock
  -- 1. Request (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientRequested{loBlock,loLength}} ->
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Request)
    in if isJust (mbeRequested mbe0) then mv else
      bimapMbe'
      (const . Left $ fail' loHost loBlock $ BPEUnexpectedForForger Request)
      (\x -> Right x { boeRequested=Just loAt, boeChainDelta=loLength `max` boeChainDelta x })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Fetch (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Fetch)
    in if isJust (mbeAcquired mbe0) then mv else
      bimapMbe'
      (const . Left $ fail' loHost loBlock (BPEUnexpectedForForger Fetch))
      (\x -> Right x { boeFetched=Just loAt })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Forge (forger only)
  LogObject{loAt, loHost, loBody=LOBlockForged{loBlock,loPrev,loBlockNo,loSlotNo}} ->
    getBlock loBlock
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
        , bfeSlotStart    = slotStart genesis loSlotNo
        , bfeEpochNo      = fst $ genesis `unsafeParseSlot` loSlotNo
        , bfeBlockSize    = Nothing
        , bfeChecked      = mvChecked
        , bfeLeading      = mvLeading
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
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Adopt)
    in
      if isJust (mbeAdopted mbe0) && isJust (mbeBlockSize mbe0)
      then mv else
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
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Announce)
    in if isJust (mbeAnnounced mbe0) then mv else
      bimapMbe
      (\x -> x { bfeAnnounced=Just loAt })
      (\x -> x { boeAnnounced=Just loAt })
      mbe0
      & doInsert loBlock
  -- 5. Sending started
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSending{loBlock}} ->
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Send)
    in if isJust (mbeSending mbe0) then mv else
      bimapMbe
      (\x -> x { bfeSending=Just loAt })
      (\x -> x { boeSending=Just loAt })
      mbe0
      & doInsert loBlock
  LogObject{loAt, loBody=LOTraceStartLeadershipCheck{}} ->
    mv { mvChecked = Just loAt }
  LogObject{loAt, loBody=LOTraceLeadershipDecided _ leading} ->
    if not leading then mv
    else mv { mvLeading = Just loAt }
  _ -> mv
 where
   fail' :: Host -> Hash -> BPErrorKind -> BPError
   fail' host hash desc = BPError host hash (Just lo) desc

   fail :: Host -> Hash -> BPErrorKind -> MachBlockEvents a
   fail host hash desc = MBE $ fail' host hash desc

   getBlock :: Hash -> Maybe (MachBlockEvents UTCTime)
   getBlock k = Map.lookup k mvBlocks

   doInsert :: Hash -> MachBlockEvents UTCTime -> MachView
   doInsert k x = mv { mvBlocks = Map.insert k x mvBlocks }

deltifyEvents :: MachBlockEvents UTCTime -> MachBlockEvents NominalDiffTime
deltifyEvents (MBE e) = MBE e
deltifyEvents (MFE x@ForgerEvents{..}) =
  MFE x
  { bfeChecked   = bfeChecked  <&> (`sinceSlot` bfeSlotStart)
  , bfeLeading   = diffUTCTime <$> bfeLeading   <*> bfeChecked
  , bfeForged    = diffUTCTime <$> bfeForged    <*> bfeLeading
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
  v@(MOE x') ->
    MOE x' { boeErrorsCrit = collectEventErrors v [Notice, Request, Fetch, Adopt]
           , boeErrorsSoft = collectEventErrors v [Announce, Send]}
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
