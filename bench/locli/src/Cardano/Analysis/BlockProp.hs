{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}

{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use head" -}

module Cardano.Analysis.BlockProp
  ( summariseMultiBlockProp
  , MachView
  , buildMachViews
  , rebuildChain
  , blockProp
  , BlockPropError(..)
  , renderBlockPropError
  , checkAllForgersKnown
  )
where

import Prelude                  (String, (!!), error, head, last, id, show, tail, read)
import Cardano.Prelude          hiding (head, show)

import Control.Arrow            ((***), (&&&))
import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Bifunctor
import Data.Function            (on)
import Data.List                (break, dropWhileEnd, intercalate, partition, span)
import Data.Map.Strict          (Map)
import Data.Map.Strict          qualified as Map
import Data.Maybe               (catMaybes, mapMaybe, isNothing)
import Data.Set                 (Set)
import Data.Set                 qualified as Set
import Data.Text                qualified as T
import Data.Text.Short          (toText)
import Data.Tuple               (swap)
import Data.Tuple.Extra         (both, fst3, snd3, thd3)
import Data.Vector              (Vector)
import Data.Vector              qualified as Vec

import Data.Time.Clock          (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

import Text.Printf              (printf)

import Cardano.Slotting.Slot    (EpochNo(..), SlotNo(..))
import Ouroboros.Network.Block  (BlockNo(..))

import Data.Accum
import Data.CDF

import Cardano.Render
import Cardano.Unlog.LogObject
import Cardano.Unlog.Resources
import Cardano.Util

import Cardano.Analysis.API


summariseMultiBlockProp :: [Centile] -> [BlockPropOne] -> Either CDFError MultiBlockProp
summariseMultiBlockProp _ [] = error "Asked to summarise empty list of BlockPropOne"
summariseMultiBlockProp centiles bs@(headline:_) = do
  cdfForgerStart            <- cdf2OfCDFs comb $ bs <&> cdfForgerStart
  cdfForgerBlkCtx           <- cdf2OfCDFs comb $ bs <&> cdfForgerBlkCtx
  cdfForgerLgrState         <- cdf2OfCDFs comb $ bs <&> cdfForgerLgrState
  cdfForgerLgrView          <- cdf2OfCDFs comb $ bs <&> cdfForgerLgrView
  cdfForgerLead             <- cdf2OfCDFs comb $ bs <&> cdfForgerLead
  cdfForgerTicked           <- cdf2OfCDFs comb $ bs <&> cdfForgerTicked
  cdfForgerMemSnap          <- cdf2OfCDFs comb $ bs <&> cdfForgerMemSnap
  cdfForgerForge            <- cdf2OfCDFs comb $ bs <&> cdfForgerForge
  cdfForgerAnnounce         <- cdf2OfCDFs comb $ bs <&> cdfForgerAnnounce
  cdfForgerSend             <- cdf2OfCDFs comb $ bs <&> cdfForgerSend
  cdfForgerAdoption         <- cdf2OfCDFs comb $ bs <&> cdfForgerAdoption
  cdfForgerAnnounceCum      <- cdf2OfCDFs comb $ bs <&> cdfForgerAnnounceCum
  cdfPeerNoticeFirst        <- cdf2OfCDFs comb $ bs <&> cdfPeerNoticeFirst
  cdfPeerFetchFirst         <- cdf2OfCDFs comb $ bs <&> cdfPeerFetchFirst
  cdfPeerRequest            <- cdf2OfCDFs comb $ bs <&> cdfPeerRequest
  cdfPeerFetch              <- cdf2OfCDFs comb $ bs <&> cdfPeerFetch
  cdfPeerAdoption           <- cdf2OfCDFs comb $ bs <&> cdfPeerAdoption
  cdfPeerAnnounce           <- cdf2OfCDFs comb $ bs <&> cdfPeerAnnounce
  cdfPeerSend               <- cdf2OfCDFs comb $ bs <&> cdfPeerSend
  cdfBlockBattle            <- cdf2OfCDFs comb $ bs <&> cdfBlockBattle
  cdfBlockSize              <- cdf2OfCDFs comb $ bs <&> cdfBlockSize
  cdfBlocksPerHost          <- cdf2OfCDFs comb $ bs <&> cdfBlocksPerHost
  cdfBlocksFilteredRatio    <- cdf2OfCDFs comb $ bs <&> cdfBlocksFilteredRatio
  cdfBlocksChainedRatio     <- cdf2OfCDFs comb $ bs <&> cdfBlocksChainedRatio
  bpPropagation             <- sequence $ transpose (bs <&> Map.toList . bpPropagation) <&>
    \case
      [] -> Left CDFEmptyDataset
      xs@((d,_):ds) -> do
        unless (all (d ==) $ fmap fst ds) $
          Left $ CDFIncoherentSamplingCentiles [Centile . read . T.unpack . T.drop 3 . fst <$> xs]
        (d,) <$> cdf2OfCDFs comb (snd <$> xs)
  pure $ BlockProp
    { bpVersion             = bpVersion headline
    , bpDomainSlots         = slotDomains
    , bpDomainBlocks        = blockDomains
    , bpDomainCDFSlots      = slotDomains  &
                                traverseDataDomain (cdf stdCentiles . fmap unI)
    , bpDomainCDFBlocks     = blockDomains &
                                traverseDataDomain (cdf stdCentiles . fmap unI)
    , bpPropagation         = Map.fromList bpPropagation
    , ..
    }
 where
   comb :: forall a. Divisible a => Combine I a
   comb = stdCombine1 centiles

   slotDomains :: [DataDomain I SlotNo]
   slotDomains = bs <&> bpDomainSlots
   blockDomains :: [DataDomain I BlockNo]
   blockDomains = bs <&> bpDomainBlocks

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
  , boeNoticed    :: !(SMaybe a)
  , boeRequested  :: !(SMaybe a)
  , boeFetched    :: !(SMaybe a)
  , boeAnnounced  :: !(SMaybe a)
  , boeSending    :: !(SMaybe a)
  , boeAdopted    :: !(SMaybe a)
  , boeChainDelta :: !Int
  , boeErrorsCrit :: [BPError]
  , boeErrorsSoft :: [BPError]
  }
  deriving (Generic, NFData, FromJSON, ToJSON, Show)

mbePhaseIndex :: Map Phase (MachBlockEvents a -> SMaybe a)
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

mbeGetProjection :: Phase -> (MachBlockEvents a -> SMaybe a)
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

mapMbeMayNE :: (ForgerEvents a -> SMaybe b) -> (ObserverEvents a -> SMaybe b)
       -> MachBlockEvents a -> SMaybe b
mapMbeMayNE f o = \case
  MFE x -> f x
  MOE x -> o x
  MBE{} -> SNothing

mbeForge :: MachBlockEvents a -> Maybe (ForgerEvents a)
mbeForge = mapMbe Just (const Nothing) (const Nothing)

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

mbeNoticed, mbeRequested, mbeAcquired, mbeAnnounced, mbeSending, mbeAdopted :: MachBlockEvents a -> SMaybe a
mbeNoticed   = mapMbe (const SNothing) boeNoticed   (const SNothing)
mbeRequested = mapMbe (const SNothing) boeRequested (const SNothing)
mbeAcquired  = mapMbe bfeForged        boeFetched   (const SNothing)
mbeAnnounced = mapMbe bfeAnnounced     boeAnnounced (const SNothing)
mbeSending   = mapMbe bfeSending       boeSending   (const SNothing)
mbeAdopted   = mapMbe bfeAdopted       boeAdopted   (const SNothing)

mbeBlockSize :: MachBlockEvents a -> SMaybe Int
mbeBlockSize = mapMbe bfeBlockSize (const SNothing) (const SNothing)

mbeHost :: MachBlockEvents a -> Host
mbeHost = mapMbe bfeHost boeHost eHost

mbeBlock :: MachBlockEvents a -> Hash
mbeBlock = mapMbe bfeBlock boeBlock eBlock

mbeBlockNo :: MachBlockEvents a -> BlockNo
mbeBlockNo = mapMbe bfeBlockNo boeBlockNo (const (-1))

-- | Machine's private view of all the blocks.
type MachHashBlockEvents a
  =  Map.Map Hash (MachBlockEvents a)

-- An accumulator for: tip-block-events & the set of all blocks events
data MachView
  = MachView
  { mvHost         :: !Host
  , mvHashBlocks   :: !(MachHashBlockEvents UTCTime)
  , mvStarted      :: !(SMaybe UTCTime)
  , mvBlkCtx       :: !(SMaybe UTCTime)
  , mvLgrState     :: !(SMaybe UTCTime)
  , mvLgrView      :: !(SMaybe UTCTime)
  , mvLeading      :: !(SMaybe UTCTime)
  , mvTicked       :: !(SMaybe UTCTime)
  , mvMemSnap      :: !(SMaybe UTCTime)
  }
  deriving (FromJSON, Generic, NFData, ToJSON)

mvForges :: MachView -> [ForgerEvents UTCTime]
mvForges = mapMaybe (mbeForge . snd) . Map.toList . mvHashBlocks

machViewMaxBlock :: MachView -> MachBlockEvents UTCTime
machViewMaxBlock MachView{..} =
  Map.elems mvHashBlocks
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

rebuildChain :: Run -> [ChainFilter] -> [FilterName] -> [(JsonLogfile, MachView)] -> Chain
rebuildChain run@Run{genesis} flts fltNames xs@(fmap snd -> machViews) =
  Chain
  { cDomSlots   = DataDomain
                  (Interval (blk0  & beSlotNo)  (blkL  & beSlotNo) <&> I)
                  (mFltDoms <&> fmap I . fst3)
                  (beSlotNo blkL - beSlotNo blk0 & I . fromIntegral . unSlotNo)
                  (mFltDoms <&> thd3 & maybe (I 0) I)
  , cDomBlocks  = DataDomain
                  (Interval (blk0  & beBlockNo) (blkL  & beBlockNo) <&> I)
                  (mFltDoms <&> fmap I . snd3)
                  (length cMainChain & I)
                  (length accepta & I)
  , cHostBlockStats = Map.fromList $ machViews <&> (mvHost &&& mvBlockStats)
  , ..
  }
 where
   cMainChain = computeChainBlockGaps $
                doRebuildChain (fmap deltifyEvents <$> eventMaps) tipHash
   (accepta, cRejecta) = partition (all snd . beAcceptance) cMainChain

   blkSets :: (Set Hash, Set Hash)
   blkSets@(acceptaBlocks, rejectaBlocks) =
     both (Set.fromList . fmap beBlock) (accepta, cRejecta)
   mvBlockStats :: MachView -> HostBlockStats
   mvBlockStats (fmap bfeBlock . mvForges -> fs) = HostBlockStats {..}
    where hbsUnchained = (countListAll                         fs & unsafeCoerceCount)
                       - hbsFiltered - hbsRejected
          hbsFiltered  = countList (`Set.member` acceptaBlocks) fs & unsafeCoerceCount
          hbsRejected  = countList (`Set.member` rejectaBlocks) fs & unsafeCoerceCount

   (blk0,  blkL)  = (head &&& last) cMainChain
   mFltDoms :: Maybe (Interval SlotNo, Interval BlockNo, Int)
   mFltDoms =
     liftA2 (,) (find (all snd . beAcceptance)          cMainChain)
                (find (all snd . beAcceptance) (reverse cMainChain))
     <&> \firstLastBlk ->
     (,,) (uncurry Interval $ both beSlotNo  firstLastBlk)
          (uncurry Interval $ both beBlockNo firstLastBlk)
          (fromIntegral . unSlotNo . uncurry (on (flip (-)) beSlotNo) $ firstLastBlk)

   eventMaps      = machViews <&> mvHashBlocks

   finalBlockEv   = maximumBy ordBlockEv $ machViewMaxBlock <$> machViews
   finalBlockNo   = mbeBlockNo finalBlockEv

   tipHash        = rewindChain eventMaps finalBlockNo 1 (mbeBlock finalBlockEv)
   tipBlock       = getBlockForge eventMaps finalBlockNo tipHash

   computeChainBlockGaps :: [BlockEvents] -> [BlockEvents]
   computeChainBlockGaps [] = error "computeChainBlockGaps on an empty chain"
   computeChainBlockGaps lst@(hd:_) =
     snd $ mapAccumL step (unSlotStart . bfSlotStart $ beForge hd) lst
    where
      step :: UTCTime -> BlockEvents -> (UTCTime, BlockEvents)
      step prevForge x@(beForgedAt -> at) =
        (at, x { beForge = (beForge x) { bfBlockGap = at `diffUTCTime` prevForge } })

   rewindChain :: [MachHashBlockEvents a] -> BlockNo -> Int -> Hash -> Hash
   rewindChain eventMaps nr0 count tip = go tip nr0 count
    where go tip nr = \case
            0 -> tip
            n -> go (bfeBlockPrev $ getBlockForge eventMaps nr tip)
                    (nr - 1) (n - 1)

   getBlockForge :: [MachHashBlockEvents a] -> BlockNo -> Hash -> ForgerEvents a
   getBlockForge xs (BlockNo nr) h =
     mapMaybe (Map.lookup h) xs
     & find mbeForgP
     & fromMaybe
        (error $ mconcat
         [ "Invariant failed: couldn't find a forge for hash ", show h
         , " BlockNo ", show nr
         , "\nErrors:\n", show (intercalate "\n" $ fmap show $ errorMbes $ mapMaybe (Map.lookup h) xs)
         ])
     & mapMbe id (error "Silly invariant failed.") (error "Silly invariant failed.")

   adoptionMap    :: [Map Hash UTCTime]
   adoptionMap    =  Map.mapMaybe (lazySMaybe . mbeAdopted) <$> eventMaps

   heightHostMap      :: (Map BlockNo (Set Hash), Map Host (Set Hash))
   heightHostMap@(heightMap, hostMap)
     = foldr (\MachView{..} (accHeight, accHost) ->
                 (,)
                 (Map.foldr
                   (\mbe -> Map.alter
                            (maybe (Just $ Set.singleton (mbeBlock mbe))
                                   (Just . Set.insert (mbeBlock mbe)))
                            (mbeBlockNo mbe))
                   accHeight mvHashBlocks)
                 (Map.insert
                   mvHost
                   (Map.elems mvHashBlocks
                    & Set.fromList . fmap bfeBlock . mapMaybe mbeForge)
                   accHost))
       (mempty, mempty) machViews

   doRebuildChain :: [MachHashBlockEvents NominalDiffTime] -> Hash -> [BlockEvents]
   doRebuildChain machBlockMaps chainTipHash = go (Just chainTipHash) []
    where go Nothing  acc = acc
          go (Just hash) acc =
            case partitionMbes $ mapMaybe (Map.lookup hash) machBlockMaps of
              ([], _, ers) -> error $ mconcat
                [ "No forger for hash ", show hash
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
          , bfTicked     = bfeTicked
          , bfMemSnap    = bfeMemSnap
          , bfForged     = bfeForged    & handleMiss "Δt Forged"
          -- NOTE (XXX, TODO, FIXME):
          --    1. we need to get to the bottom of this
          --    2. this happens sufficiently rarely (2500+ blocks in an affected run)
          --       that it has no impact on statistics, quite frankly
          , bfAnnounced  = bfeAnnounced
                           <|> (if True -- bfeBlockNo == 0 -- silliness
                                then SJust 0.01 else SNothing)
                           & handleMiss "Δt Announced (forger)"
          , bfAnnouncedCum = bfeAnnouncedCum
                           <|> (if True -- bfeBlockNo == 0 -- silliness
                                then SJust 0.01 else SNothing)
                           & handleMiss "Δt AnnouncedCum (forger)"
          , bfSending    = bfeSending
                           <|> (if True -- bfeBlockNo == 0 -- silliness
                                then SJust 0.01 else SNothing)
                           & handleMiss "Δt Sending (forger)"
          , bfAdopted    = bfeAdopted
                           <|> (if True -- bfeBlockNo == 0 -- silliness
                                then SJust 0.01 else SNothing)
                           & handleMiss "Δt Adopted (forger)"
          , bfChainDelta = bfeChainDelta
          }
        , beForks = unsafeCoerceCount $ countListAll otherBlocks
        , beObservations =
            catSMaybes $
            os <&> \ObserverEvents{..}->
              do
                let boObserver   = boeHost
                    boSlotStart  = bfeSlotStart
                    boAnnounced  = boeAnnounced
                    boSending    = boeSending
                    boAdopted    = boeAdopted
                    boChainDelta = boeChainDelta
                    boErrorsCrit = boeErrorsCrit
                    boErrorsSoft = boeErrorsSoft
                boNoticed   <- boeNoticed
                boRequested <- boeRequested
                boFetched   <- boeFetched
                pure BlockObservation{..}
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
                         & strictMaybe
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

      handleMiss :: String -> SMaybe a -> a
      handleMiss slotDesc = fromSMaybe $ error $ mconcat
       [ "While processing ", show bfeBlockNo, " hash ", show bfeBlock
       , " forged by ", show (unHost host)
       , " -- missing: ", slotDesc
       ]

data BlockPropError
  = BPEEntireChainFilteredOut
    { bpeChainLen   :: Int
    , bpeAcceptance :: [(BlockNo, [(ChainFilter, Bool)])]
    }

renderBlockPropError :: BlockPropError -> T.Text
renderBlockPropError = \case
  BPEEntireChainFilteredOut chainlen rejs -> mconcat $
    [ "blockProp | analysisChain:  all blocks filtered out of originally "
    , T.pack $ show chainlen, "\n\n"
    , "  Block pass/fail reasons:\n"
    ] ++
    fmap (("\n    " <>)
          . (\(no, rs) -> T.pack $
              show no <> ":  " <> show rs))
         rejs

blockProp :: Run -> Chain -> Either BlockPropError BlockPropOne
blockProp run@Run{genesis} Chain{..} = do
  (c :: [BlockEvents]) <-
    case filter (all snd . beAcceptance) cMainChain of
      [] -> Left $
        BPEEntireChainFilteredOut
          (length cMainChain)
          ((beBlockNo &&& beAcceptance) <$> cMainChain)
      xs -> pure xs

  pure $ BlockProp
    { bpDomainSlots        = cDomSlots
    , bpDomainBlocks       = cDomBlocks
    , bpDomainCDFSlots     = cDomSlots   -- At unit-arity..
    , bpDomainCDFBlocks    = cDomBlocks  -- .. it's just a replica.
    , cdfForgerStart       = forgerCDF c   (SJust . bfStarted   . beForge)
    , cdfForgerBlkCtx      = forgerCDF c           (bfBlkCtx    . beForge)
    , cdfForgerLgrState    = forgerCDF c           (bfLgrState  . beForge)
    , cdfForgerLgrView     = forgerCDF c           (bfLgrView   . beForge)
    , cdfForgerLead        = forgerCDF c   (SJust . bfLeading   . beForge)
    , cdfForgerTicked      = forgerCDF c           (bfTicked    . beForge)
    , cdfForgerMemSnap     = forgerCDF c           (bfMemSnap   . beForge)
    , cdfForgerForge       = forgerCDF c   (SJust . bfForged    . beForge)
    , cdfForgerAnnounce    = forgerCDF c   (SJust . bfAnnounced . beForge)
    , cdfForgerAnnounceCum = forgerCDF c   (SJust . bfAnnouncedCum . beForge)
    , cdfForgerSend        = forgerCDF c   (SJust . bfSending   . beForge)
    , cdfForgerAdoption    = forgerCDF c   (SJust . bfAdopted   . beForge)
    , cdfPeerNoticeFirst   = observerCDF c (SJust . boNoticed)    earliest "noticed"
    , cdfPeerFetchFirst    = observerCDF c (SJust . boFetchedCum) earliest "fetched"
    , cdfPeerRequest       = observerCDF c (SJust . boRequested) each "requested"
    , cdfPeerFetch         = observerCDF c (SJust . boFetched)   each "fetched"
    , cdfPeerAnnounce      = observerCDF c boAnnounced           each "announced"
    , cdfPeerSend          = observerCDF c boSending             each "sending"
    , cdfPeerAdoption      = observerCDF c boAdopted             each "adopted"
    , bpPropagation        = Map.fromList
      [ ( T.pack $ printf "cdf%.2f" p'
        , forgerCDF c (SJust . unI . projectCDF' "bePropagation" p . bePropagation))
      | p@(Centile p') <- adoptionCentiles <> [Centile 1.0] ]
    , cdfBlockBattle         = forgerCDF c (SJust . unCount . beForks)
    , cdfBlockSize           = forgerCDF c (SJust . bfBlockSize . beForge)
    , bpVersion              = getLocliVersion
    , cdfBlocksPerHost       = cdf stdCentiles (hostBlockStats
                                                <&> unCount . hbsTotal)
    , cdfBlocksFilteredRatio = cdf stdCentiles (hostBlockStats
                                                <&> uncurry ((/) `on`
                                                             fromIntegral . unCount)
                                                 . (hbsFiltered &&& hbsChained)
                                                & filter (not . isNaN))
    , cdfBlocksChainedRatio  = cdf stdCentiles (hostBlockStats
                                                <&> uncurry ((/) `on`
                                                             fromIntegral . unCount)
                                                 . (hbsChained &&& hbsTotal)
                                                & filter (not . isNaN))
    }
 where
   ne :: String -> [a] -> [a]
   ne desc = \case
     [] -> error desc
     xs -> xs

   hostBlockStats = Map.elems cHostBlockStats

   boFetchedCum :: BlockObservation -> NominalDiffTime
   boFetchedCum BlockObservation{..} = boNoticed + boRequested + boFetched

   forgerCDF :: Divisible a => [BlockEvents] -> (BlockEvents -> SMaybe a) -> CDF I a
   forgerCDF chain proj =
     cdfZ stdCentiles $ mapSMaybe proj chain

   each, earliest :: [NominalDiffTime] -> [NominalDiffTime]
   each = identity
   earliest [] = []
   earliest xs = [minimum xs]

   observerCDF :: [BlockEvents]
               -> (BlockObservation -> SMaybe NominalDiffTime)
               -> ([NominalDiffTime] -> [NominalDiffTime])
               -> String -> CDF I NominalDiffTime
   observerCDF chain proj subset _ =
     mapChainBlockEventsCDF stdCentiles chain (subset . blockObservations)
    where
      blockObservations :: BlockEvents -> [NominalDiffTime]
      blockObservations be =
        proj `mapSMaybe` filter isValidBlockObservation (beObservations be)

   mapChainBlockEventsCDF ::
     Divisible a
     => [Centile]
     -> [BlockEvents]
     -> (BlockEvents -> [a])
     -> CDF I a
   mapChainBlockEventsCDF percs cbes f =
     cdfZ percs $ concatMap f cbes

-- | Given a single machine's log object stream, recover its block map.
blockEventMapsFromLogObjects :: Run -> (JsonLogfile, [LogObject]) -> MachView
blockEventMapsFromLogObjects run (f@(unJsonLogfile -> fp), []) =
  error $ mconcat ["0 LogObjects in ", fp]
blockEventMapsFromLogObjects run (f@(unJsonLogfile -> fp), xs@(x:_)) =
  foldl' (blockPropMachEventsStep run f) initial xs
 where
   initial =
     MachView
     { mvHost         = loHost x
     , mvHashBlocks   = mempty
     , mvStarted      = SNothing
     , mvBlkCtx       = SNothing
     , mvLgrState     = SNothing
     , mvLgrView      = SNothing
     , mvLeading      = SNothing
     , mvTicked       = SNothing
     , mvMemSnap      = SNothing
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
         (SJust loAt)                 -- Noticed
         SNothing                     -- Requested
         SNothing                     -- Fetched
         SNothing                     -- Announced
         SNothing                     -- Sending
         SNothing 0                   -- Adopted & chain delta
         [] [])
      & doInsert loBlock
  -- 1. Request (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientRequested{loBlock,loLength}} ->
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Request)
    in if isSJust (mbeRequested mbe0) then mv else
      bimapMbe'
      (const . Left $ fail' loHost loBlock $ BPEUnexpectedForForger Request)
      (\x -> Right x { boeRequested=SJust loAt, boeChainDelta=loLength `max` boeChainDelta x })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Fetch (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Fetch)
    in if isSJust (mbeAcquired mbe0) then mv else
      bimapMbe'
      (const . Left $ fail' loHost loBlock (BPEUnexpectedForForger Fetch))
      (\x -> Right x { boeFetched=SJust loAt })
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
        , bfeBlockSize    = SNothing
        , bfeStarted      = mvStarted
        , bfeBlkCtx       = mvBlkCtx
        , bfeLgrState     = mvLgrState
        , bfeLgrView      = mvLgrView
        , bfeLeading      = mvLeading
        , bfeTicked       = mvTicked
        , bfeMemSnap      = mvMemSnap
        , bfeForged       = SJust loAt
        , bfeAnnounced    = SNothing
        , bfeAnnouncedCum = SNothing
        , bfeSending      = SNothing
        , bfeAdopted      = SNothing
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
      if isSJust (mbeAdopted mbe0) && isSJust (mbeBlockSize mbe0)
      then mv else
      mbe0
      & (if isSJust (mbeAdopted mbe0) then id else
         bimapMbe
         (\x -> x { bfeAdopted=SJust loAt, bfeChainDelta=loLength })
         (\x -> x { boeAdopted=SJust loAt, boeChainDelta=loLength `max` boeChainDelta x}))
      & (if isSJust (mbeBlockSize mbe0) || isSNothing loSize then id else
         bimapMbe
         (\x -> x { bfeBlockSize=loSize })
         id)
      & doInsert loBlock
  -- 4. Announce
  LogObject{loAt, loHost, loBody=LOChainSyncServerSendHeader{loBlock}} ->
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Announce)
    in if isSJust (mapMbeMayNE bfeAnnounced boeAnnounced mbe0) then mv else
      bimapMbe
      (\x -> x { bfeAnnounced=SJust loAt })
      (\x -> x { boeAnnounced=SJust loAt })
      mbe0
      & doInsert loBlock
  -- 5. Sending started
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSending{loBlock}} ->
    let mbe0 = getBlock loBlock
               & fromMaybe (fail loHost loBlock $ BPEUnexpectedAsFirst Send)
    in if isSJust (mbeSending mbe0) then mv else
      bimapMbe
      (\x -> x { bfeSending=SJust loAt })
      (\x -> x { boeSending=SJust loAt })
      mbe0
      & doInsert loBlock
  LogObject{loAt, loBody=LOTraceStartLeadershipCheck{}} ->
    mv { mvStarted = SJust loAt }
  LogObject{loAt, loBody=LOBlockContext{}} ->
    mv { mvBlkCtx = SJust loAt }
  LogObject{loAt, loBody=LOLedgerState{}} ->
    mv { mvLgrState = SJust loAt }
  LogObject{loAt, loBody=LOLedgerView{}} ->
    mv { mvLgrView = SJust loAt }
  LogObject{loAt, loBody=LOTraceLeadershipDecided _ leading} ->
    if not leading then mv
    else mv { mvLeading = SJust loAt }
  LogObject{loAt, loBody=LOTickedLedgerState {}} ->
    mv { mvTicked  = SJust loAt }
  LogObject{loAt, loBody=LOMempoolSnapshot{}} ->
    mv { mvMemSnap = SJust loAt }
  _ -> mv
 where
   fail' :: Host -> Hash -> BPErrorKind -> BPError
   fail' host hash desc = BPError host hash (Just lo) desc

   fail :: Host -> Hash -> BPErrorKind -> MachBlockEvents a
   fail host hash desc = MBE $ fail' host hash desc

   getBlock :: Hash -> Maybe (MachBlockEvents UTCTime)
   getBlock k = Map.lookup k mvHashBlocks

   doInsert :: Hash -> MachBlockEvents UTCTime -> MachView
   doInsert k x = mv { mvHashBlocks = Map.insert k x mvHashBlocks }

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
  , bfeTicked    = diffUTCTime <$> bfeTicked     <*> bfeLeading
  , bfeMemSnap   = diffUTCTime <$> bfeMemSnap    <*> bfeTicked
  , bfeForged    = (diffUTCTime <$> bfeForged    <*> bfeMemSnap)
                   <|>
                   (diffUTCTime <$> bfeForged    <*> bfeLeading)
  , bfeAnnounced = diffUTCTime <$> bfeAnnounced <*> bfeForged
  , bfeAnnouncedCum = bfeAnnounced <&> (`sinceSlot` bfeSlotStart)
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
      (,) True _        -> BPEMissingPhase phase
      (,) _ (SJust neg) -> BPENegativePhase phase neg
      _ -> error "Impossible."
  | phase <- phases
  , let proj = mbeGetProjection phase mbe
  , let miss = isSNothing proj
  , let neg  = ((< 0) <$> proj) == SJust True
  , miss || neg
  ]

-- | Expects a log object stream of or including block forges.
-- Returns the first log object + hash that we don't know a forge for.
checkAllForgersKnown :: [LogObject] -> Maybe (LogObject, Hash)
checkAllForgersKnown logobjs =
  getFirst $ First pass1 <> First pass2
  where
    (pass1, forged) = go1 (Set.singleton $ Hash "GenesisHash") logobjs
    pass2           = go2 forged logobjs

    go1 forged = \case
      [] -> (Nothing, forged)
      lo@LogObject{loBody}:los -> case loBody of
        LOBlockForged{..}
          | loPrev `Set.member` forged  -> go1 (loBlock `Set.insert` forged) los
          | otherwise                   -> (Just (lo, loPrev), forged)
        _                               -> go1 forged los

    go2 forged = \case
      [] -> Nothing
      lo@LogObject{loBody}:los -> case loBody of
        LOChainSyncClientSeenHeader{loBlock}
          | loBlock `Set.member` forged -> go2 forged los
          | otherwise                   -> Just (lo, loBlock)
        LOChainSyncServerSendHeader{loBlock}
          | loBlock `Set.member` forged -> go2 forged los
          | otherwise                   -> Just (lo, loBlock)
        _                               -> go2 forged los
