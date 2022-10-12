{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Avoid lambda" -}
module Cardano.Analysis.BlockProp
  ( summariseMultiBlockProp
  , MachView
  , buildMachViews
  , rebuildChain
  , blockProp)
where

import Prelude                  (String, (!!), error, head, last, id, show, tail)
import Cardano.Prelude          hiding (head, show)

import Control.Arrow            ((***), (&&&))
import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Bifunctor
import Data.Function            (on)
import Data.List                (dropWhileEnd, intercalate, partition)
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
import Data.CDF

import Cardano.Analysis.API
import Cardano.Analysis.Chain
import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Render
import Cardano.Unlog.LogObject  hiding (Text)
import Cardano.Unlog.Resources
import Cardano.Util


summariseMultiBlockProp :: [Centile] -> [BlockPropOne] -> Either CDFError MultiBlockProp
summariseMultiBlockProp _ [] = error "Asked to summarise empty list of BlockPropOne"
summariseMultiBlockProp centiles bs@(headline:_) = do
  cdfForgerStarts           <- cdf2OfCDFs comb $ bs <&> cdfForgerStarts
  cdfForgerBlkCtx           <- cdf2OfCDFs comb $ bs <&> cdfForgerBlkCtx
  cdfForgerLgrState         <- cdf2OfCDFs comb $ bs <&> cdfForgerLgrState
  cdfForgerLgrView          <- cdf2OfCDFs comb $ bs <&> cdfForgerLgrView
  cdfForgerLeads            <- cdf2OfCDFs comb $ bs <&> cdfForgerLeads
  cdfForgerForges           <- cdf2OfCDFs comb $ bs <&> cdfForgerForges
  cdfForgerAdoptions        <- cdf2OfCDFs comb $ bs <&> cdfForgerAdoptions
  cdfForgerAnnouncements    <- cdf2OfCDFs comb $ bs <&> cdfForgerAnnouncements
  cdfForgerSends            <- cdf2OfCDFs comb $ bs <&> cdfForgerSends
  cdfPeerNotices            <- cdf2OfCDFs comb $ bs <&> cdfPeerNotices
  cdfPeerRequests           <- cdf2OfCDFs comb $ bs <&> cdfPeerRequests
  cdfPeerFetches            <- cdf2OfCDFs comb $ bs <&> cdfPeerFetches
  cdfPeerAdoptions          <- cdf2OfCDFs comb $ bs <&> cdfPeerAdoptions
  cdfPeerAnnouncements      <- cdf2OfCDFs comb $ bs <&> cdfPeerAnnouncements
  cdfPeerSends              <- cdf2OfCDFs comb $ bs <&> cdfPeerSends
  cdfForks                  <- cdf2OfCDFs comb $ bs <&> cdfForks
  cdfSizes                  <- cdf2OfCDFs comb $ bs <&> cdfSizes
  bpPropagation            <- sequence $ transpose (bs <&> bpPropagation) <&>
    \case
      [] -> Left CDFEmptyDataset
      xs@((d,_):ds) -> do
        unless (all (d ==) $ fmap fst ds) $
          Left $ CDFIncoherentSamplingCentiles [Centile . fst <$> xs]
        (d,) <$> cdf2OfCDFs comb (snd <$> xs)
  pure $ BlockProp
    { bpVersion             = bpVersion headline
    , bpDomainSlots         = dataDomainsMergeOuter $ bs <&> bpDomainSlots
    , bpDomainBlocks        = dataDomainsMergeOuter $ bs <&> bpDomainBlocks
    , ..
    }
 where
   comb :: forall a. Divisible a => Combine I a
   comb = stdCombine1 centiles

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
  , bfeStarted      :: !(Maybe a)
  , bfeBlkCtx       :: !(Maybe a)
  , bfeLgrState     :: !(Maybe a)
  , bfeLgrView      :: !(Maybe a)
  , bfeLeading      :: !(Maybe a)
  , bfeForged       :: !(Maybe a)
  , bfeAnnounced    :: !(Maybe a)
  , bfeSending      :: !(Maybe a)
  , bfeAdopted      :: !(Maybe a)
  , bfeChainDelta   :: !Int
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
  , boeAnnounced  :: !(Maybe a)
  , boeSending    :: !(Maybe a)
  , boeAdopted    :: !(Maybe a)
  , boeChainDelta :: !Int
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
  , (Announce,   mbeAnnounced)
  , (Send,       mbeSending)
  , (Adopt,      mbeAdopted)
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

mbeNoticed, mbeRequested, mbeAcquired, mbeAnnounced, mbeSending, mbeAdopted :: MachBlockEvents a -> Maybe a
mbeNoticed   = mapMbe (const Nothing)  boeNoticed   (const Nothing)
mbeRequested = mapMbe (const Nothing)  boeRequested (const Nothing)
mbeAcquired  = mapMbe bfeForged        boeFetched   (const Nothing)
mbeAnnounced = mapMbe bfeAnnounced     boeAnnounced (const Nothing)
mbeSending   = mapMbe bfeSending       boeSending   (const Nothing)
mbeAdopted   = mapMbe bfeAdopted       boeAdopted   (const Nothing)

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
  { mvHost     :: !Host
  , mvBlocks   :: !(MachBlockMap UTCTime)
  , mvStarted  :: !(Maybe UTCTime)
  , mvBlkCtx   :: !(Maybe UTCTime)
  , mvLgrState :: !(Maybe UTCTime)
  , mvLgrView  :: !(Maybe UTCTime)
  , mvLeading  :: !(Maybe UTCTime)
  }
  deriving (FromJSON, Generic, NFData, ToJSON)

machViewMaxBlock :: MachView -> MachBlockEvents UTCTime
machViewMaxBlock MachView{..} =
  Map.elems mvBlocks
  & \case
       [] -> MBE $ BPError { eHost=mvHost, eBlock=Hash "Genesis", eLO=Nothing, eDesc=BPENoBlocks }
       xs -> maximumBy ordBlockEv xs

beForgedAt :: BlockEvents -> UTCTime
beForgedAt BlockEvents{beForge=BlockForge{..}} =
  bfForged `afterSlot` bfSlotStart

buildMachViews :: Run -> [(JsonLogfile, [LogObject])] -> IO [(JsonLogfile, MachView)]
buildMachViews run = mapConcurrentlyPure (fst &&& blockEventMapsFromLogObjects run)

blockEventsAcceptance :: Genesis -> [ChainFilter] -> BlockEvents -> [(ChainFilter, Bool)]
blockEventsAcceptance genesis flts be = flts <&> (id &&& testBlockEvents genesis be)

rebuildChain :: Run -> [ChainFilter] -> [FilterName] -> [(JsonLogfile, MachView)] -> IO (DataDomain SlotNo, DataDomain BlockNo, [BlockEvents], [BlockEvents])
rebuildChain run@Run{genesis} flts fltNames xs@(fmap snd -> machViews) = do
  progress "tip" $ Q $ show $ bfeBlock tipBlock
  forM_ flts $
    progress "filter" . Q . show
  pure (domSlot, domBlock, chainRejecta, chain)
 where
   (blk0,  blkL)  = (head chain, last chain)
   mblkV =
     liftA2 (,) (find (all snd . beAcceptance)          chain)
                (find (all snd . beAcceptance) (reverse chain))
   domSlot = DataDomain
             (blk0  & beSlotNo)  (blkL  & beSlotNo)
             (mblkV <&> beSlotNo . fst)
             (mblkV <&> beSlotNo . snd)
             (beSlotNo blkL - beSlotNo blk0 & fromIntegral . unSlotNo)
             (mblkV &
              maybe 0 (fromIntegral . unSlotNo . uncurry (on (flip (-)) beSlotNo)))
   domBlock = DataDomain
              (blk0  & beBlockNo) (blkL  & beBlockNo)
              (mblkV <&> beBlockNo . fst)
              (mblkV <&> beBlockNo . snd)
              (length chain)
              (length acceptableChain)

   (acceptableChain, chainRejecta) = partition (all snd . beAcceptance) chain

   chain = computeChainBlockGaps $
           doRebuildChain (fmap deltifyEvents <$> eventMaps) tipHash

   eventMaps      = mvBlocks <$> machViews

   finalBlockEv   = maximumBy ordBlockEv $ machViewMaxBlock <$> machViews

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
   liftBlockEvents ForgerEvents{bfeHost=host, ..} os errs = blockEvents
    where
      blockEvents =
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
          , bfStarted    = bfeStarted   & handleMiss "Δt Started"
          , bfBlkCtx     = bfeBlkCtx
          , bfLgrState   = bfeLgrState
          , bfLgrView    = bfeLgrView
          , bfLeading    = bfeLeading   & handleMiss "Δt Leading"
          , bfForged     = bfeForged    & handleMiss "Δt Forged"
          -- NOTE (XXX, TODO, FIXME):
          --    1. we need to get to the bottom of this
          --    2. this happens sufficiently rarely (2500+ blocks in an affected run)
          --       that it has no impact on statistics, quite frankly
          , bfAnnounced  = bfeAnnounced
                           <|> (if True -- bfeBlockNo == 0 -- silliness
                                then Just 0.01 else Nothing)
                           & handleMiss "Δt Announced (forger)"
          , bfSending    = bfeSending
                           <|> (if True -- bfeBlockNo == 0 -- silliness
                                then Just 0.01 else Nothing)
                           & handleMiss "Δt Sending (forger)"
          , bfAdopted    = bfeAdopted
                           <|> (if True -- bfeBlockNo == 0 -- silliness
                                then Just 0.01 else Nothing)
                           & handleMiss "Δt Adopted (forger)"
          , bfChainDelta = bfeChainDelta
          }
        , beForks = unsafeCoerceCount $ countOfList otherBlocks
        , beObservations =
            catMaybes $
            os <&> \ObserverEvents{..}->
              BlockObservation
                <$> Just boeHost
                <*> Just bfeSlotStart
                <*> boeNoticed
                <*> boeRequested
                <*> boeFetched
                <*> Just boeAnnounced
                <*> Just boeSending
                <*> Just boeAdopted
                <*> Just boeChainDelta
                <*> Just boeErrorsCrit
                <*> Just boeErrorsSoft
        , bePropagation  = cdf adoptionCentiles adoptions
        , beOtherBlocks  = otherBlocks <&>
                           \(ForgerEvents{bfeBlock}, _) -> bfeBlock
        , beErrors =
            errs
            <> (otherBlocks <&> snd)
            <> bfeErrs
            <> concatMap boeErrorsCrit os
            <> concatMap boeErrorsSoft os
        , beAcceptance = blockEventsAcceptance genesis flts blockEvents
        }

      adoptions =
        (fmap (`sinceSlot` bfeSlotStart) . Map.lookup bfeBlock) `mapMaybe` adoptionMap

      otherBlocks = otherBlockHashes <&>
                    \blk ->
                      let forger = findForger blk in
                      (forger,
                       fail' (bfeHost forger) bfeBlock (BPEFork blk))
      otherBlockHashes = Map.lookup bfeBlockNo heightMap
                         & handleMiss "height map"
                         & Set.delete bfeBlock
                         & Set.toList

      findForger :: Hash -> ForgerEvents UTCTime
      findForger hash =
        maybe
          (error $ "Unknown host for block " <> show hash)
          (mapMbe id (error "Invariant failed") (error "Invariant failed"))
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

blockProp :: Run -> [BlockEvents] -> DataDomain SlotNo -> DataDomain BlockNo -> IO BlockPropOne
blockProp run@Run{genesis} fullChain domSlot domBlock = do
  pure $ BlockProp
    { bpDomainSlots          = domSlot
    , bpDomainBlocks         = domBlock
    , cdfForgerStarts        = forgerEventsCDF   (Just . bfStarted   . beForge)
    , cdfForgerBlkCtx        = forgerEventsCDF           (bfBlkCtx   . beForge)
    , cdfForgerLgrState      = forgerEventsCDF           (bfLgrState . beForge)
    , cdfForgerLgrView       = forgerEventsCDF           (bfLgrView  . beForge)
    , cdfForgerLeads         = forgerEventsCDF   (Just . bfLeading   . beForge)
    , cdfForgerForges        = forgerEventsCDF   (Just . bfForged    . beForge)
    , cdfForgerAnnouncements = forgerEventsCDF   (Just . bfAnnounced . beForge)
    , cdfForgerSends         = forgerEventsCDF   (Just . bfSending   . beForge)
    , cdfForgerAdoptions     = forgerEventsCDF   (Just . bfAdopted   . beForge)
    , cdfPeerNotices         = observerEventsCDF (Just . boNoticed)   "noticed"
    , cdfPeerRequests        = observerEventsCDF (Just . boRequested) "requested"
    , cdfPeerFetches         = observerEventsCDF (Just . boFetched)   "fetched"
    , cdfPeerAnnouncements   = observerEventsCDF boAnnounced          "announced"
    , cdfPeerSends           = observerEventsCDF boSending            "sending"
    , cdfPeerAdoptions       = observerEventsCDF boAdopted            "adopted"
    , bpPropagation          =
      [ (p', forgerEventsCDF (Just . unI . projectCDF' "bePropagation" p . bePropagation))
      | p@(Centile p') <- adoptionCentiles <> [Centile 1.0] ]
    , cdfForks               = forgerEventsCDF   (Just . unCount . beForks)
    , cdfSizes               = forgerEventsCDF   (Just . bfBlockSize . beForge)
    , bpVersion              = getVersion
    }
 where
   analysisChain = filter (all snd . beAcceptance) fullChain

   forgerEventsCDF   :: Divisible a => (BlockEvents -> Maybe a) -> CDF I a
   forgerEventsCDF   = flip (witherToDistrib (cdf stdCentiles)) analysisChain
   observerEventsCDF = mapChainToPeerBlockObservationCDF stdCentiles analysisChain

   mapChainToBlockEventCDF ::
     Divisible a
     => [Centile]
     -> [BlockEvents]
     -> (BlockEvents -> Maybe a)
     -> CDF I a
   mapChainToBlockEventCDF percs cbes proj =
     cdf percs $
       mapMaybe proj cbes

   mapChainToPeerBlockObservationCDF ::
        [Centile]
     -> [BlockEvents]
     -> (BlockObservation -> Maybe NominalDiffTime)
     -> String
     -> CDF I NominalDiffTime
   mapChainToPeerBlockObservationCDF percs cbes proj desc =
     cdf percs $
       concat $ cbes <&> blockObservations
    where
      blockObservations :: BlockEvents -> [NominalDiffTime]
      blockObservations be =
        proj `mapMaybe` filter isValidBlockObservation (beObservations be)

witherToDistrib ::
     ([b] -> CDF p b)
  -> (a -> Maybe b)
  -> [a]
  -> CDF p b
witherToDistrib distrify proj xs =
  distrify $ mapMaybe proj xs

-- | Given a single machine's log object stream, recover its block map.
blockEventMapsFromLogObjects :: Run -> (JsonLogfile, [LogObject]) -> MachView
blockEventMapsFromLogObjects run (f@(unJsonLogfile -> fp), []) =
  error $ mconcat ["0 LogObjects in ", fp]
blockEventMapsFromLogObjects run (f@(unJsonLogfile -> fp), xs@(x:_)) =
  foldl' (blockPropMachEventsStep run f) initial xs
 where
   initial =
     MachView
     { mvHost     = loHost x
     , mvBlocks   = mempty
     , mvStarted  = Nothing
     , mvBlkCtx   = Nothing
     , mvLgrState = Nothing
     , mvLgrView  = Nothing
     , mvLeading  = Nothing
     }

blockPropMachEventsStep :: Run -> JsonLogfile -> MachView -> LogObject -> MachView
blockPropMachEventsStep run@Run{genesis} (JsonLogfile fp) mv@MachView{..} lo = case lo of
  -- 0. Notice (observer only)
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = getBlock loBlock
    in if isJust mbe0 then mv else
      MOE
       (ObserverEvents
         loHost
         loBlock loBlockNo loSlotNo
         (slotStart genesis loSlotNo) -- t+0:  slot start
         (Just loAt)                  -- Noticed
         Nothing                      -- Requested
         Nothing                      -- Fetched
         Nothing                      -- Announced
         Nothing                      -- Sending
         Nothing 0                    -- Adopted & chain delta
         [] [])
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
        , bfeStarted      = mvStarted
        , bfeBlkCtx       = mvBlkCtx
        , bfeLgrState     = mvLgrState
        , bfeLgrView      = mvLgrView
        , bfeLeading      = mvLeading
        , bfeForged       = Just loAt
        , bfeAnnounced    = Nothing
        , bfeSending      = Nothing
        , bfeAdopted      = Nothing
        , bfeChainDelta   = 0
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
    mv { mvStarted = Just loAt }
  LogObject{loAt, loBody=LOBlockContext{}} ->
    mv { mvBlkCtx = Just loAt }
  LogObject{loAt, loBody=LOLedgerState{}} ->
    mv { mvLgrState = Just loAt }
  LogObject{loAt, loBody=LOLedgerView{}} ->
    mv { mvLgrView = Just loAt }
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
  { bfeStarted   = bfeStarted  <&> (`sinceSlot` bfeSlotStart)
  , bfeBlkCtx    = diffUTCTime <$> bfeBlkCtx    <*> bfeStarted
  , bfeLgrState  = diffUTCTime <$> bfeLgrState  <*> bfeBlkCtx
  , bfeLgrView   = diffUTCTime <$> bfeLgrView   <*> bfeLgrState
  , bfeLeading   = (diffUTCTime <$> bfeLeading   <*> bfeLgrView)
                   <|>
                   (diffUTCTime <$> bfeLeading   <*> bfeStarted)
  , bfeForged    = diffUTCTime <$> bfeForged    <*> bfeLeading
  , bfeAnnounced = diffUTCTime <$> bfeAnnounced <*> bfeForged
  , bfeSending   = diffUTCTime <$> bfeSending   <*> bfeForged
  , bfeAdopted   = diffUTCTime <$> bfeAdopted   <*> bfeForged
  } & \case
  v@(MFE x') -> MFE x' { bfeErrs = collectEventErrors v
                         [Forge, Adopt, Announce, Send] }
  _ -> error "Impossible"
deltifyEvents (MOE x@ObserverEvents{..}) =
  MOE x
  { boeNoticed   = boeNoticed <&> (`sinceSlot` boeSlotStart)
  , boeRequested = diffUTCTime <$> boeRequested <*> boeNoticed
  , boeFetched   = diffUTCTime <$> boeFetched   <*> boeRequested
  , boeAnnounced = diffUTCTime <$> boeAnnounced <*> boeFetched
  , boeSending   = diffUTCTime <$> boeSending   <*> boeFetched
  , boeAdopted   = diffUTCTime <$> boeAdopted   <*> boeFetched
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
