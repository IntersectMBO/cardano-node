{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.MachPerf (module Cardano.Analysis.MachPerf) where

import Prelude (head, last)
import Cardano.Prelude hiding (head)
import Cardano.Prelude qualified as CP

import Data.List                        ((!!))
import Data.Map.Strict qualified as Map
import Data.Text                        (pack, unpack)
import Data.Text.Short                  (toText)
import Data.Vector (Vector)
import Data.Vector qualified as Vec

import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock qualified as Time

import Data.CDF
import Cardano.Util
import Cardano.Analysis.API
import Cardano.Unlog.LogObject hiding (Text)
import Cardano.Unlog.Resources


-- * 1. Collect SlotStats & RunScalars:
--
collectSlotStats :: Run -> [(JsonLogfile, [LogObject])]
                 -> IO (Either Text [(JsonLogfile, (RunScalars, [SlotStats UTCTime]))])
collectSlotStats run = fmap sequence <$> mapConcurrentlyPure (timelineFromLogObjects run)


timelineFromLogObjects :: Run -> (JsonLogfile, [LogObject])
                       -> Either Text (JsonLogfile, (RunScalars, [SlotStats UTCTime]))
timelineFromLogObjects _ (JsonLogfile f, []) =
  Left $ "timelineFromLogObjects:  zero logobjects from " <> pack f
timelineFromLogObjects run@Run{genesis} (f, xs') =
  Right . (f,)
  $ foldl' (timelineStep run f) zeroTimelineAccum xs
  & (aRunScalars &&& reverse . aSlotStats)
 where
   xs = filter ((/= "DecodeError") . loKind) xs'

   firstRelevantLogObjectTime :: UTCTime
   firstRelevantLogObjectTime = loAt (head xs) `max` systemStart genesis
   firstLogObjectHost :: Host
   firstLogObjectHost = loHost (head xs)

   zeroTimelineAccum :: TimelineAccum
   zeroTimelineAccum =
     TimelineAccum
     { aResAccums     = mkResAccums
     , aResTimestamp  = firstRelevantLogObjectTime
     , aMempoolTxs    = 0
     , aBlockNo       = 0
     , aLastBlockSlot = 0                          -- Genesis counts
     , aSlotStats     = [zeroSlotStats]
     , aRunScalars    = zeroRunScalars
     , aTxsCollectedAt= mempty
     , aHost          = firstLogObjectHost
     }
   zeroRunScalars :: RunScalars
   zeroRunScalars  = RunScalars Nothing Nothing Nothing
   zeroSlotStats :: SlotStats UTCTime
   zeroSlotStats =
     SlotStats
     { slSlot = impliedSlot genesis firstRelevantLogObjectTime
     , slEpoch = 0
     , slEpochSlot = 0
     , slEpochSafeInt = 0
     , slStart = SlotStart firstRelevantLogObjectTime
     , slCountStarts   = 0
     , slCountBlkCtx   = 0
     , slCountLgrState = 0
     , slCountLgrView  = 0
     , slCountLeads = 0
     , slCountForges = 0
     , slStarted    = SNothing
     , slBlkCtx     = SNothing
     , slLgrState   = SNothing
     , slLgrView    = SNothing
     , slLeading    = SNothing
     , slForged     = SNothing
     , slSpanTxsMem = SNothing
     , slMempoolTxs = 0
     , slTxsCollected = 0
     , slTxsAccepted = 0
     , slTxsRejected = 0
     , slUtxoSize = 0
     , slDensity = 0
     , slResources = SNothing
     , slChainDBSnap = 0
     , slRejectedTx = 0
     , slBlockNo = 0
     , slBlockGap = 0
     }

timelineStep :: Run -> JsonLogfile -> TimelineAccum -> LogObject -> TimelineAccum
timelineStep Run{genesis} f a@TimelineAccum{aSlotStats=cur:_, ..} lo =
  let continue :: SlotNo -> UTCTime -> TimelineAccum
      continue slot loAt =
        if slot < slSlot cur then a
        else a
             & (if slot - slSlot cur <= 1
                then identity                   -- for the next slot, no gap to patch
                else patchSlotGap genesis slot) -- for a future slot, patch the gap just until
             & if slot == slSlot cur
               then identity                    -- for the current slot, nothing to add
               else addTimelineSlot genesis slot loAt
      mapExistingSlot :: SlotNo -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum -> TimelineAccum
      mapExistingSlot slot fSlot a'@TimelineAccum{aSlotStats=last:_} =
        (if slot < slSlot last -- for a slot gone by
         then forTANth  a' (fromIntegral . unSlotNo $ slSlot last - slot) fSlot
         else forTAHead a' fSlot)
      forExistingSlot :: SlotNo -> TimelineAccum -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
      forExistingSlot slot a' fSlot = mapExistingSlot slot fSlot a'
      forNonFutureSlot :: TimelineAccum -> SlotNo -> String -> Host -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
      forNonFutureSlot TimelineAccum{aSlotStats=cur:_} slot desc host x =
        if slot > slSlot cur
        then error $ mconcat
             [ desc, " for a future slot=", show slot
             , " cur=", show (slSlot cur)
             , " host=", unpack . toText $ unHost host
             , " file=", unJsonLogfile f
             ]
        else forExistingSlot slot a x
  in if loAt lo < systemStart genesis then a else
  case lo of
  -- First, events that can extend the timeline:
  --
  LogObject{loAt, loBody=LOResources rs} ->
    continue slot loAt
    & mapExistingSlot slot
     (\sl -> sl { slResources   = SJust $ extractResAccums accs })
    & \a' -> a' { aResAccums    = accs
                , aResTimestamp = loAt
                }
   where
     slot = impliedSlot genesis loAt
     accs = updateResAccums loAt rs aResAccums
  LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo density} ->
    continue slot loAt
    & mapExistingSlot slot
    (\sl@SlotStats{..} ->
        sl { slCountStarts = slCountStarts + 1
           , slStarted = SJust loAt
           , slUtxoSize = utxo
           , slDensity = density
           })
  -- Next, events that technically should extend the timeline,
  -- but we don't really care for their misattribution to incur the overhead:
  --
  LogObject{loBody=LOMempoolTxs txCount} ->
    (forTAHead a
      \s-> s { slMempoolTxs = txCount })
    { aMempoolTxs     = txCount }
  LogObject{loBody=LOMempoolRejectedTx} ->
    forTAHead a
      \s-> s { slRejectedTx = slRejectedTx cur + 1 }
  LogObject{loBody=LOLedgerTookSnapshot} ->
    forTAHead a
      \s-> s { slChainDBSnap = slChainDBSnap cur + 1 }
  LogObject{loBody=LOGeneratorSummary _noFails rssub rselap rsthr} ->
    a { aRunScalars =
        RunScalars
        { rsSubmitted     = Just rssub
        , rsElapsed       = Just rselap
        , rsThreadwiseTps = Just rsthr
        } }
  LogObject{loBody=LOTxsCollected coll, loTid, loAt} ->
    (forTAHead a
      \s-> s { slTxsCollected = slTxsCollected cur + max 0 (fromIntegral coll) })
    { aTxsCollectedAt =
      aTxsCollectedAt &
      (\case
          Just{} -> Just loAt
          --   error $ mconcat
          --   ["Duplicate LOTxsCollected for tid ", show tid, " at ", show loAt]
          Nothing -> Just loAt)
      `Map.alter` loTid
    }
  LogObject{loBody=LOTxsProcessed acc rej, loTid, loAt} ->
    (forTAHead a
      \s@SlotStats{..}-> s
      { slSpanTxsMem =
          case loTid `Map.lookup` aTxsCollectedAt of
            Nothing ->
              -- error $ mconcat
              -- ["LOTxsProcessed missing LOTxsCollected for tid", show tid, " at ", show loAt]
              SJust $
              1.0
              +
              fromSMaybe 0 slSpanTxsMem
            Just base ->
              SJust $
              (loAt `Time.diffUTCTime` base)
              +
              fromSMaybe 0 slSpanTxsMem
      , slTxsAccepted = slTxsAccepted + acc
      , slTxsRejected = slTxsRejected + max 0 (fromIntegral rej)
      })
    { aTxsCollectedAt = loTid `Map.delete` aTxsCollectedAt
    }
  -- Next, events that rely on their slotstats to pre-exist:
  --
  LogObject{loBody=LOBlockContext slot blockNo, loHost, loAt} ->
    (forNonFutureSlot a slot "BlockContext" loHost $
      \sl ->
       sl { slCountBlkCtx = slCountBlkCtx sl + 1
          , slBlkCtx      = SJust loAt
          , slBlockNo     = blockNo
          , slBlockGap    = if blockNo /= aBlockNo then 0 else slBlockGap cur
          })
    { aBlockNo        = blockNo
    , aLastBlockSlot  = a & lastBlockSlot blockNo
    }
  LogObject{loBody=LOLedgerState slot, loHost, loAt} ->
    forNonFutureSlot a slot "LedgerState" loHost $
      \sl@SlotStats{..} ->
       sl { slCountLgrState = slCountLgrState + 1
          , slLgrState      = SJust loAt
          }
  LogObject{loBody=LOLedgerView slot, loHost, loAt} ->
    forNonFutureSlot a slot "LedgerView" loHost $
      \sl@SlotStats{..} ->
       sl { slCountLgrView = slCountLgrView + 1
          , slLgrView      = SJust loAt
          }
  LogObject{loAt, loHost, loBody=LOTraceLeadershipDecided slot lead} ->
    forNonFutureSlot a slot "LeadDecided" loHost $
      \sl@SlotStats{..} ->
       sl { slCountLeads = slCountLeads + if lead then 1 else 0
          , slLeading    = SJust loAt
          }
  LogObject{loAt, loHost, loBody=LOBlockForged{loSlotNo}} ->
    if loSlotNo > slSlot cur
    then error $ mconcat
         [ "BlockForged for a future slot=", show loSlotNo
         , " cur=", show (slSlot cur)
         , " host=", unpack . toText $ unHost loHost
         ]
    else forExistingSlot loSlotNo a $
           onBlockForge loAt
   where
     onBlockForge :: UTCTime -> SlotStats UTCTime -> SlotStats UTCTime
     onBlockForge now sl@SlotStats{..} =
       sl { slCountForges = slCountForges + 1
          , slForged      = SJust now
          }
  _ -> a
timelineStep _ _ a _ = a
-- The "fold" state that accumulates as we process 'LogObject's into a stream
-- of 'SlotStats'.
data TimelineAccum
  = TimelineAccum
  { aResAccums     :: ResAccums
  , aResTimestamp  :: UTCTime
  , aMempoolTxs    :: Word64
  , aBlockNo       :: BlockNo
  , aLastBlockSlot :: SlotNo
  , aSlotStats     :: [SlotStats UTCTime]
  , aRunScalars    :: RunScalars
  , aTxsCollectedAt:: Map.Map TId UTCTime
  , aHost          :: Host
  }

forTAHead :: TimelineAccum -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
forTAHead xs@TimelineAccum{aSlotStats=s:ss} f = xs {aSlotStats=f s:ss}

forTANth :: TimelineAccum -> Int -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
forTANth xs@TimelineAccum{aSlotStats=ss, aHost} n f =
  xs { aSlotStats = mapNth f n ss }
 where
   mapNth :: (a -> a) -> Int -> [a] -> [a]
   mapNth f n xs =
     case splitAt n xs of
       (pre, x:post) -> pre <> (f x : post)
       _ -> error $ mconcat
            [ "mapNth: couldn't go ", show n, "-deep into the timeline, "
            , "host=", unpack . toText $ unHost aHost
            ]

lastBlockSlot :: BlockNo -> TimelineAccum -> SlotNo
lastBlockSlot new TimelineAccum{aSlotStats=SlotStats{..}:_,..} =
  if aBlockNo /= new -- A new block?
  then slSlot
  else aLastBlockSlot

patchSlotGap :: Genesis -> SlotNo -> TimelineAccum -> TimelineAccum
patchSlotGap genesis curSlot a@TimelineAccum{aSlotStats=last:_, ..} =
  a & if gapLen < 1000
      then go gapLen gapStartSlot
      else error $ mconcat
           [ "patchSlotGap: gap too large: ", show gapLen, ", "
           , "curSlot=", show curSlot, ", "
           , "gapStartSlot=", show gapStartSlot, ", "
           ]
 where
   gapStartSlot = slSlot last + 1
   gapLen = unSlotNo $ curSlot - gapStartSlot

   go :: Word64 -> SlotNo -> TimelineAccum -> TimelineAccum
   go 0      _         acc = acc
   go remainingGap patchSlot acc =
     go (remainingGap - 1) (patchSlot + 1) (acc & addGapSlot patchSlot)

   addGapSlot :: SlotNo -> TimelineAccum -> TimelineAccum
   addGapSlot slot acc =
    let (epoch, epochSlot) = genesis `unsafeParseSlot` slot in
      acc { aSlotStats = SlotStats
          { slSlot        = slot
          , slEpoch       = epoch
          , slEpochSlot   = epochSlot
          , slEpochSafeInt= slotEpochSafeInt genesis epochSlot
          , slStart       = slStart
            -- Updated as we see repeats:
          , slCountStarts = 0
          , slCountBlkCtx = 0
          , slCountLgrState = 0
          , slCountLgrView = 0
          , slCountLeads  = 0
          , slCountForges = 0
          , slStarted     = SNothing
          , slBlkCtx      = SNothing
          , slLgrState    = SNothing
          , slLgrView     = SNothing
          , slLeading     = SNothing
          , slForged      = SNothing
          , slSpanTxsMem  = SNothing
          , slTxsCollected= 0
          , slTxsAccepted = 0
          , slTxsRejected = 0
          , slMempoolTxs  = aMempoolTxs
          , slUtxoSize    = slUtxoSize last
          , slDensity     = slDensity last
          , slChainDBSnap = 0
          , slRejectedTx  = 0
          , slBlockNo     = aBlockNo
          , slBlockGap    = unSlotNo $ slot - aLastBlockSlot
          , slResources   = SJust $ zeroObsoleteValues
                                    <*> extractResAccums aResAccums}
          : aSlotStats acc
        }
    where slStart = slotStart genesis slot

addTimelineSlot :: Genesis -> SlotNo -> UTCTime -> TimelineAccum -> TimelineAccum
addTimelineSlot genesis slot time a@TimelineAccum{..} =
  let (epoch, epochSlot) = genesis `unsafeParseSlot` slot in
    a { aSlotStats = SlotStats
        { slSlot        = slot
        , slEpoch       = epoch
        , slEpochSlot   = epochSlot
        , slEpochSafeInt= slotEpochSafeInt genesis epochSlot
        , slStart       = slStart
          -- Updated as we see repeats:
        , slCountStarts = 0
        , slCountBlkCtx = 0
        , slCountLgrState = 0
        , slCountLgrView = 0
        , slCountLeads  = 0
        , slCountForges = 0
        , slStarted     = SJust time
        , slBlkCtx      = SNothing
        , slLgrState    = SNothing
        , slLgrView     = SNothing
        , slLeading     = SNothing
        , slForged      = SNothing
        , slSpanTxsMem  = SNothing
        , slTxsCollected= 0
        , slTxsAccepted = 0
        , slTxsRejected = 0
        , slMempoolTxs  = aMempoolTxs
        , slUtxoSize    = 0
        , slDensity     = 0
        , slChainDBSnap = 0
        , slRejectedTx  = 0
        , slBlockNo     = aBlockNo
        , slBlockGap    = unSlotNo $ slot - aLastBlockSlot
        , slResources   = SJust $ zeroObsoleteValues
                                  <*> extractResAccums aResAccums}
        : aSlotStats
      }
    where slStart = slotStart genesis slot

-- * 2. Filter SlotStats:
--
runSlotFilters ::
     NFData a =>
     Run
  -> [ChainFilter]
  -> [(JsonLogfile, [SlotStats a])]
  -> IO (DataDomain SlotNo, [(JsonLogfile, [SlotStats a])])
runSlotFilters Run{genesis} flts slots =
  mapConcurrentlyPure (fmap $ filterSlotStats flts) slots
    <&> \filtered ->
          (,) (domain filtered) filtered
 where
   domain :: [(JsonLogfile, [SlotStats a])] -> DataDomain SlotNo
   domain filtered = mkDataDomain
     ((CP.head samplePre  <&> slSlot) & fromMaybe 0)
     ((lastMay samplePre  <&> slSlot) & fromMaybe 0)
     ((CP.head samplePost <&> slSlot) & fromMaybe 0)
     ((lastMay samplePost <&> slSlot) & fromMaybe 0)
     (fromIntegral . unSlotNo)
    where
      samplePre  =    slots !! 0 & snd
      samplePost = filtered !! 0 & snd

   filterSlotStats :: [ChainFilter] -> [SlotStats a] -> [SlotStats a]
   filterSlotStats filters =
     filter (\x -> all (testSlotStats genesis x) slotFilters)
    where
      slotFilters :: [SlotCond]
      slotFilters = catSlotFilters filters

-- * 3. Post-process:
--
deltifySlotStats :: Genesis -> SlotStats UTCTime -> SlotStats NominalDiffTime
deltifySlotStats gsis s@SlotStats{..} =
  s
  { slStarted   = slStarted  <&> (`sinceSlot` slotStart gsis slSlot)
  , slBlkCtx    =  diffUTCTime <$> slBlkCtx    <*> slStarted
  , slLgrState  =  diffUTCTime <$> slLgrState  <*> slBlkCtx
  , slLgrView   =  diffUTCTime <$> slLgrView   <*> slLgrState
  , slLeading   = (diffUTCTime <$> slLeading   <*> slLgrView)
                  <|>
                  (diffUTCTime <$> slLeading   <*> slStarted)
  , slForged    =  diffUTCTime <$> slForged    <*> slLeading
  }

-- Field 6 "productiv"   "Produc" "tivity"  (IText
--       (\SlotStats{..}->
--           f 4 $ calcProd <$> (min 6 . -- workaround for ghc-8.10.2
--                               fromIntegral <$> rCentiMut slResources :: Maybe Double)
--           <*> (fromIntegral <$> rCentiCpu slResources))) "" ""

-- Field 6 "allocMut"     "Alloc/" "mutSec" (IText
-- (\SlotStats{..}->
--     d 5 $
--     (ceiling :: Double -> Int)
--     <$> ((/) <$> (fromIntegral . (100 *) <$> rAlloc slResources)
--           <*> (fromIntegral . max 1 . (1024 *) <$> rCentiMut slResources)))) "" ""

-- Field 10 0 "absSlotTime" "Absolute" "slot time" $ IText
-- (\SlotStats{..}->
--    T.pack $ " " `splitOn` show slStart !! 1)

data SlotStatsSummary
  = SlotStatsSummary
  { sssSpanLensCpu      :: [Int]
  , sssSpanLensCpuEpoch :: [Int]
  , sssSpanLensCpuRwd   :: [Int]
  }

slotStatsSummary :: forall a. Run -> [SlotStats a] -> SlotStatsSummary
slotStatsSummary Run{genesis=Genesis{epochLength}} slots =
  SlotStatsSummary{..}
 where
   sssSpanLensCpu      = spanLen <$> spansCpu
   sssSpanLensCpuRwd   = Vec.length <$> filter (spanContainsEpochSlot rewardCalcBeginSlot) spansCpu
   sssSpanLensCpuEpoch = Vec.length <$> spansCpuEpoch

   rewardCalcBeginSlot = 3 + floor @Double (fromIntegral epochLength * 0.4)

   spansCpu :: [Vector (SlotStats a)]
   spansCpu       = spans
                      ((/= SJust False) . fmap ((>=85) . rCentiCpu) . slResources)
                      (toList slots)

   spansCpuEpoch :: [Vector (SlotStats a)]
   spansCpuEpoch  = filter (spanContainsEpochSlot 3) spansCpu <&>
     \v-> let   tailEpoch =  slEpoch (Vec.last v)
          in if tailEpoch == slEpoch (Vec.head v) then v
             else Vec.dropWhile ((tailEpoch == ) . slEpoch) v

   spanLen :: Vector (SlotStats a) -> Int
   spanLen = fromIntegral . unSlotNo . uncurry (-) . (slSlot *** slSlot) . (Vec.last &&& Vec.head)

   spanContainsEpochSlot :: Word64 -> Vector (SlotStats a) -> Bool
   spanContainsEpochSlot s =
     uncurry (&&)
     . ((s >) . unEpochSlot . slEpochSlot . Vec.head &&&
        (s <) . unEpochSlot . slEpochSlot . Vec.last)

-- * 4. Summarise SlotStats & SlotStatsSummary into MachPerf:
--
slotStatsMachPerf :: Run -> (JsonLogfile, [SlotStats NominalDiffTime]) -> Either Text (JsonLogfile, MachPerfOne)
slotStatsMachPerf _ (JsonLogfile f, []) =
  Left $ "slotStatsMachPerf:  zero filtered slots from " <> pack f
slotStatsMachPerf run (f, slots) =
  Right . (f,) $ MachPerf
  { mpVersion            = getLocliVersion
  , mpDomainSlots        = mkDataDomainInj (slSlot $ head slots) (slSlot $ last slots)
                                           (fromIntegral . unSlotNo)
  --
  , cdfStarts            = dist (slCountStarts <$> slots)
  , cdfLeads             = dist (slCountLeads <$> slots)
  , cdfUtxo              = dist (slUtxoSize <$> slots)
  , cdfDensity           = dist (slDensity <$> slots)
  , cdfStarted           = dist (slStarted `mapSMaybe` slots)
  , cdfBlkCtx            = dist (slBlkCtx `mapSMaybe` slots)
  , cdfLgrState          = dist (slLgrState `mapSMaybe` slots)
  , cdfLgrView           = dist (slLgrView `mapSMaybe` slots)
  , cdfLeading           = dist (slLeading `mapSMaybe` slots)
  , cdfForged            = dist (filter (/= 0) $ slForged `mapSMaybe` slots)
  , cdfBlockGap          = dist (slBlockGap <$> slots)
  , cdfSpanLensCpu       = dist sssSpanLensCpu
  , cdfSpanLensCpuEpoch  = dist sssSpanLensCpuEpoch
  , cdfSpanLensCpuRwd    = dist sssSpanLensCpuRwd
  , mpResourceCDFs       = computeResCDF stdCentiles slResources slots
  }
 where
   dist :: Divisible a => [a] -> CDF I a
   dist = cdf stdCentiles

   SlotStatsSummary{..} = slotStatsSummary run slots

-- * 5. Multi-machine & multi-run summaries:
--
summariseMultiClusterPerf :: [Centile] -> [ClusterPerf] -> Either CDFError MultiClusterPerf
summariseMultiClusterPerf _ [] = error "Asked to summarise empty list of MachPerfOne"
summariseMultiClusterPerf centiles mps@(headline:_) = do
  cdfStarts            <- cdf2OfCDFs comb $ mps <&> cdfStarts
  cdfLeads             <- cdf2OfCDFs comb $ mps <&> cdfLeads
  cdfUtxo              <- cdf2OfCDFs comb $ mps <&> cdfUtxo
  cdfDensity           <- cdf2OfCDFs comb $ mps <&> cdfDensity
  cdfStarted           <- cdf2OfCDFs comb $ mps <&> cdfStarted
  cdfBlkCtx            <- cdf2OfCDFs comb $ mps <&> cdfBlkCtx
  cdfLgrState          <- cdf2OfCDFs comb $ mps <&> cdfLgrState
  cdfLgrView           <- cdf2OfCDFs comb $ mps <&> cdfLgrView
  cdfLeading           <- cdf2OfCDFs comb $ mps <&> cdfLeading
  cdfForged            <- cdf2OfCDFs comb $ mps <&> cdfForged
  cdfBlockGap          <- cdf2OfCDFs comb $ mps <&> cdfBlockGap
  cdfSpanLensCpu       <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpu
  cdfSpanLensCpuEpoch  <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuEpoch
  cdfSpanLensCpuRwd    <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuRwd
  mpResourceCDFs       <- sequence $ traverse identity (mps <&> mpResourceCDFs) <&>
    \case
      [] -> Left CDFEmptyDataset
      (xs :: [CDF (CDF I) Word64]) -> cdf2OfCDFs comb xs :: Either CDFError (CDF (CDF I) Word64)

  pure . MultiClusterPerf $ MachPerf
    { mpVersion          = mpVersion headline
    , mpDomainSlots      = dataDomainsMergeOuter $ mps <&> mpDomainSlots
    , ..
    }
 where
   comb :: forall a. Divisible a => Combine (CDF I) a
   comb = stdCombine2 centiles

summariseClusterPerf :: [Centile] -> [MachPerfOne] -> Either CDFError ClusterPerf
summariseClusterPerf _ [] = error "Asked to summarise empty list of MachPerfOne"
summariseClusterPerf centiles mps@(headline:_) = do
  cdfStarts            <- cdf2OfCDFs comb $ mps <&> cdfStarts
  cdfLeads             <- cdf2OfCDFs comb $ mps <&> cdfLeads
  cdfUtxo              <- cdf2OfCDFs comb $ mps <&> cdfUtxo
  cdfDensity           <- cdf2OfCDFs comb $ mps <&> cdfDensity
  cdfStarted           <- cdf2OfCDFs comb $ mps <&> cdfStarted
  cdfBlkCtx            <- cdf2OfCDFs comb $ mps <&> cdfBlkCtx
  cdfLgrState          <- cdf2OfCDFs comb $ mps <&> cdfLgrState
  cdfLgrView           <- cdf2OfCDFs comb $ mps <&> cdfLgrView
  cdfLeading           <- cdf2OfCDFs comb $ mps <&> cdfLeading
  cdfForged            <- cdf2OfCDFs comb $ mps <&> cdfForged
  cdfBlockGap          <- cdf2OfCDFs comb $ mps <&> cdfBlockGap
  cdfSpanLensCpu       <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpu
  cdfSpanLensCpuEpoch  <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuEpoch
  cdfSpanLensCpuRwd    <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuRwd
  mpResourceCDFs       <- sequence $ traverse identity (mps <&> mpResourceCDFs) <&>
    \case
      [] -> Left CDFEmptyDataset
      (xs :: [CDF I Word64]) -> cdf2OfCDFs comb xs :: Either CDFError (CDF (CDF I) Word64)

  pure MachPerf
    { mpVersion          = mpVersion headline
    , mpDomainSlots      = dataDomainsMergeOuter $ mps <&> mpDomainSlots
    , ..
    }
 where
   comb :: forall a. Divisible a => Combine I a
   comb = stdCombine1 centiles
