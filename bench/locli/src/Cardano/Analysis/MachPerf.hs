{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Evaluate" -}

module Cardano.Analysis.MachPerf (module Cardano.Analysis.MachPerf) where

import Cardano.Prelude hiding (head)
import Cardano.Prelude qualified as CP

import Data.List                        ((!!))
import Data.Map.Strict qualified as Map
import Data.Text                        (pack, unpack)
import Data.Text.Short                  (toText)
import Data.Vector (Vector)
import Data.Vector qualified as Vec

import Data.Time.Clock qualified as Time

import Data.CDF
import Cardano.Util
import Cardano.Analysis.API
import Cardano.Unlog.LogObject
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
   xs = filter (not . (`textRefEquals` "DecodeError") . loKind) xs'

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
     , aLogObjects    = []
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
     , slTicked     = SNothing
     , slMemSnap    = SNothing
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
     , slLogObjects  = []
     }

timelineStep :: Run -> JsonLogfile -> TimelineAccum -> LogObject -> TimelineAccum
timelineStep Run{genesis} f accum@TimelineAccum{aSlotStats=cur:_, ..} lo =
  -- 1. skip pre-historic events not subject to performance analysis;
  --    Potentially _collapsingly huge_, depending on what portion of logs you get.
  if loAt lo < systemStart genesis then accum else
  let continue :: SlotNo -> LogObject -> TimelineAccum -> TimelineAccum
      continue slot LogObject{loAt} acc =
        if slot < slSlot cur then acc
        else acc
             & taFlushSlotLOs
             & (if slot - slSlot cur <= 1
                then identity                   -- for the next slot, no gap to patch
                else patchSlotGap genesis slot) -- for a future slot, patch the gap just until
             & (if slot == slSlot cur
                then identity                    -- for the current slot, nothing to add
                else addTimelineSlot genesis slot loAt)
      mapExistingSlot :: SlotNo -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum -> TimelineAccum
      mapExistingSlot slot fSlot acc@TimelineAccum{aSlotStats=last:_} =
        if
          | slot < slSlot last -- for a slot gone by
            -> forTANth  acc (fromIntegral . unSlotNo $ slSlot last - slot) fSlot
          | slot == slSlot last
            -> forTAHead acc fSlot
          | otherwise -> error $ mconcat
            [ "mapExistingSlot called on a future slot ", show slot, ", "
            , "with the current slot being ", show $ slSlot last ]
      _mapSinceExistingSlot :: SlotNo -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum -> TimelineAccum
      _mapSinceExistingSlot slot fSlot acc@TimelineAccum{aSlotStats=last:_} =
        if
          | slot < slSlot last -- for a slot gone by
            -> forSinceTANth  acc (fromIntegral . unSlotNo $ slSlot last - slot) fSlot
          | slot == slSlot last
            -> forTAHead acc fSlot
          | otherwise -> error $ mconcat
            [ "mapSinceExistingSlot called on a future slot ", show slot, ", "
            , "with the current slot being ", show $ slSlot last ]
      forExistingSlot :: SlotNo -> TimelineAccum -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
      forExistingSlot slot acc fSlot = mapExistingSlot slot fSlot acc
      forNonFutureSlot :: TimelineAccum -> SlotNo -> String -> Host -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
      forNonFutureSlot acc@TimelineAccum{aSlotStats=cur:_} slot desc host x =
        if slot > slSlot cur
        then error $ mconcat
             [ desc, " for a future slot=", show slot
             , " cur=", show (slSlot cur)
             , " host=", unpack . toText $ unHost host
             , " file=", unJsonLogfile f
             ]
        else forExistingSlot slot acc x
  in
  taRecordLO lo $
  case lo of
  -- First, events that can extend the timeline:
  --  - note the mandatory use of 'continue', which performs the extension:
  LogObject{loAt, loBody=LOResources rs} ->
    continue slot lo accum
    & mapExistingSlot slot
     (\sl -> sl { slResources   = SJust $ extractResAccums accs })
    & \a' -> a' { aResAccums    = accs
                , aResTimestamp = loAt
                }
   where
     slot = impliedSlot genesis loAt
     accs = updateResAccums loAt rs aResAccums
  LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo density} ->
    continue slot lo accum
    & mapExistingSlot slot
    (\sl@SlotStats{..} ->
        sl { slCountStarts = slCountStarts + 1
           , slStarted = SJust loAt
           , slUtxoSize = utxo
           , slDensity = density
           })
  -- Next, events that technically should extend the timeline,
  -- but we don't really care for their misattribution to incur the overhead:
  --  - note the mandatory use of 'forTAHead' for SlotStats-modifying messages:
  LogObject{loBody=LOMempoolTxs txCount} ->
    (forTAHead accum
      \s-> s { slMempoolTxs = txCount })
    { aMempoolTxs     = txCount }
  LogObject{loBody=LOMempoolRejectedTx} ->
    forTAHead accum
      (\s-> s { slRejectedTx = slRejectedTx cur + 1 })
  LogObject{loBody=LOLedgerTookSnapshot} ->
    forTAHead accum
      (\s-> s { slChainDBSnap = slChainDBSnap cur + 1 })
  LogObject{loBody=LOTxsCollected coll, loTid, loAt} ->
    (forTAHead accum
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
    (forTAHead accum
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
  --  - again, note the use of forNonFutureSlot
  LogObject{loBody=LOBlockContext slot blockNo, loHost, loAt} ->
    (mapTAHead
      -- NOTE: we attribute the block number change only by the slot of arrival.
      (\sl -> sl { slBlockNo     = blockNo }) $
     forNonFutureSlot accum slot "BlockContext" loHost $
      -- NOTE: the rest of the properties get assigned in the past.
      \sl ->
       sl { slCountBlkCtx = slCountBlkCtx sl + 1
          , slBlkCtx      = SJust loAt
          , slBlockGap    = if blockNo /= aBlockNo then 0 else slBlockGap cur
          })
    { aBlockNo        = blockNo
    , aLastBlockSlot  = accum & lastBlockSlot blockNo
    }
  LogObject{loBody=LOLedgerState slot, loHost, loAt} ->
    forNonFutureSlot accum slot "LedgerState" loHost
      (\sl@SlotStats{..} ->
          sl { slCountLgrState = slCountLgrState + 1
             , slLgrState      = SJust loAt
             })
  LogObject{loBody=LOLedgerView slot, loHost, loAt} ->
    forNonFutureSlot accum slot "LedgerView" loHost
      (\sl@SlotStats{..} ->
          sl { slCountLgrView = slCountLgrView + 1
             , slLgrView      = SJust loAt
             })
  LogObject{loAt, loHost, loBody=LOTraceLeadershipDecided slot lead} ->
    forNonFutureSlot accum slot "LeadDecided" loHost
      (\sl@SlotStats{..} ->
          sl { slCountLeads = slCountLeads + if lead then 1 else 0
             , slLeading    = SJust loAt
             })
  LogObject{loAt, loHost, loBody=LOTickedLedgerState slot} ->
    forNonFutureSlot accum slot "LedgerTicked" loHost
      (\sl@SlotStats{} ->
          sl { slTicked     = SJust loAt
             })
  LogObject{loAt, loHost, loBody=LOMempoolSnapshot slot} ->
    forNonFutureSlot accum slot "MempoolSnapshotted" loHost
      (\sl@SlotStats{} ->
          sl { slMemSnap    = SJust loAt
             })
  LogObject{loAt, loHost, loBody=LOBlockForged{loSlotNo}} ->
    forNonFutureSlot accum loSlotNo "MempoolSnapshotted" loHost
      (\sl@SlotStats{..} ->
          sl { slCountForges = slCountForges + 1
             , slForged      = SJust loAt
             })
   where
  -- Aux/one-shot things.
  --
  LogObject{loBody=LOGeneratorSummary _noFails rssub rselap rsthr} ->
    accum { aRunScalars =
            RunScalars
              { rsSubmitted     = Just rssub
              , rsElapsed       = Just rselap
              , rsThreadwiseTps = Just rsthr
              } }
  _ -> accum
 where
   taRecordLO :: LogObject -> TimelineAccum -> TimelineAccum
   taRecordLO lo a@TimelineAccum{aLogObjects} = a { aLogObjects = lo:aLogObjects }

   taFlushSlotLOs :: TimelineAccum -> TimelineAccum
   taFlushSlotLOs ta@TimelineAccum{aSlotStats=cur:xs, ..} =
     ta { aSlotStats  = cur { slLogObjects = aLogObjects } : xs
        , aLogObjects = []
        }

   -- | Map `f` over the latest slot in the timeline accumulator.
   mapTAHead :: (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum -> TimelineAccum
   mapTAHead f xs@TimelineAccum{aSlotStats=s:ss} = xs {aSlotStats=f s:ss}

   -- | Map `f` over the latest slot in the timeline accumulator.
   forTAHead :: TimelineAccum -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
   forTAHead xs@TimelineAccum{aSlotStats=s:ss} f = xs {aSlotStats=f s:ss}

   -- | Map `f` over the n-th-from-the-top slot in the timeline accumulator.
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

   -- | Map `f` over n most recent slots in the timeline accumulator.
   forSinceTANth :: TimelineAccum -> Int -> (SlotStats UTCTime -> SlotStats UTCTime) -> TimelineAccum
   forSinceTANth xs@TimelineAccum{aSlotStats=ss, aHost} n f =
     xs { aSlotStats = mapSinceNth f n ss }
    where
      mapSinceNth :: (a -> a) -> Int -> [a] -> [a]
      mapSinceNth f n xs =
        case splitAt n xs of
          (pre, x:post) -> fmap f pre <> (f x : post)
          _ -> error $ mconcat
               [ "mapSinceNth: couldn't go ", show n, "-deep into the timeline, "
               , "host=", unpack . toText $ unHost aHost
               ]

--timelineStep _ _ a _ = a

-- | The "fold" state that accumulates as we process 'LogObject's into a stream
--   of 'SlotStats'.
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
  , aLogObjects    :: [LogObject]
  }

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
          , slTicked      = SNothing
          , slMemSnap     = SNothing
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
                                    <*> extractResAccums aResAccums
          , slLogObjects  = []
          }
          : aSlotStats acc
        }
    where slStart = slotStart genesis slot

addTimelineSlot :: Genesis -> SlotNo -> UTCTime -> TimelineAccum -> TimelineAccum
addTimelineSlot genesis slot _time a@TimelineAccum{..} =
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
        , slStarted     = SNothing
        , slBlkCtx      = SNothing
        , slLgrState    = SNothing
        , slLgrView     = SNothing
        , slLeading     = SNothing
        , slTicked      = SNothing
        , slMemSnap     = SNothing
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
                                  <*> extractResAccums aResAccums
        , slLogObjects  = []
        }
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
  -> IO (DataDomain I SlotNo, [(JsonLogfile, [SlotStats a])])
runSlotFilters Run{genesis} flts slots =
  mapConcurrentlyPure (fmap $ filterSlotStats flts) slots
    <&> \filtered ->
          (,) (domain filtered) filtered
 where
   domain :: [(JsonLogfile, [SlotStats a])] -> DataDomain I SlotNo
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
  , slTicked    =  diffUTCTime <$> slTicked    <*> slLeading
  , slMemSnap   =  diffUTCTime <$> slMemSnap   <*> slTicked
  , slForged    = (diffUTCTime <$> slForged    <*> slMemSnap)
                  <|>
                  (diffUTCTime <$> slForged    <*> slLeading)
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
  , mpDomainSlots        = domSlots
  , mpDomainCDFSlots     = domSlots -- At unit-arity it's just a replica.
  , cdfHostSlots         = dist [fromIntegral . unI $ ddFilteredCount domSlots]
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
  , cdfBlockGap          = dist (slBlockGap <$> slots)
  , cdfSpanLensCpu       = dist sssSpanLensCpu
  , cdfSpanLensCpuEpoch  = dist sssSpanLensCpuEpoch
  , cdfSpanLensCpuRwd    = dist sssSpanLensCpuRwd
  , mpResourceCDFs       = computeResCDF stdCentiles slResources slots
  , ..
  }
 where
   domSlots = mkDataDomainInj sFirst sLast (fromIntegral . unSlotNo)

   (,) sFirst sLast = (slSlot . head &&& slSlot . last) slots

   dist :: Divisible a => [a] -> CDF I a
   dist = cdfZ stdCentiles

   SlotStatsSummary{..} = slotStatsSummary run slots

-- * 5. Multi-machine & multi-run summaries:
--
summariseClusterPerf :: [Centile] -> [MachPerfOne] -> Either CDFError ClusterPerf
summariseClusterPerf _ [] = error "Asked to summarise empty list of MachPerfOne"
summariseClusterPerf centiles mps@(headline:_) = do
  cdfHostSlots         <- cdf2OfCDFs comb $ mps <&> cdfHostSlots
  cdfStarts            <- cdf2OfCDFs comb $ mps <&> cdfStarts
  cdfLeads             <- cdf2OfCDFs comb $ mps <&> cdfLeads
  cdfUtxo              <- cdf2OfCDFs comb $ mps <&> cdfUtxo
  cdfDensity           <- cdf2OfCDFs comb $ mps <&> cdfDensity
  cdfStarted           <- cdf2OfCDFs comb $ mps <&> cdfStarted
  cdfBlkCtx            <- cdf2OfCDFs comb $ mps <&> cdfBlkCtx
  cdfLgrState          <- cdf2OfCDFs comb $ mps <&> cdfLgrState
  cdfLgrView           <- cdf2OfCDFs comb $ mps <&> cdfLgrView
  cdfLeading           <- cdf2OfCDFs comb $ mps <&> cdfLeading
  cdfBlockGap          <- cdf2OfCDFs comb $ mps <&> cdfBlockGap
  cdfSpanLensCpu       <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpu
  cdfSpanLensCpuEpoch  <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuEpoch
  cdfSpanLensCpuRwd    <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuRwd
  mpResourceCDFs       <- sequence $ traverse identity (mps <&> mpResourceCDFs) <&>
    \case
      [] -> Left CDFEmptyDataset
      (xs :: [CDF I Word64]) -> cdf2OfCDFs comb xs :: Either CDFError (CDF (CDF I) Word64)

  pure MachPerf
    { mpVersion        = mpVersion headline
    , mpDomainSlots    = slotDomains
    , mpDomainCDFSlots = slotDomains & traverseDataDomain (cdf stdCentiles . fmap unI)
    , ..
    }
 where
   comb :: forall a. Divisible a => Combine I a
   comb = stdCombine1 centiles

   slotDomains :: [DataDomain I SlotNo]
   slotDomains = mps <&> mpDomainSlots

summariseMultiClusterPerf :: [Centile] -> [ClusterPerf] -> Either CDFError MultiClusterPerf
summariseMultiClusterPerf _ [] = error "Asked to summarise empty list of MachPerfOne"
summariseMultiClusterPerf centiles mps@(headline:_) = do
  cdfHostSlots         <- cdf2OfCDFs comb $ mps <&> cdfHostSlots
  cdfStarts            <- cdf2OfCDFs comb $ mps <&> cdfStarts
  cdfLeads             <- cdf2OfCDFs comb $ mps <&> cdfLeads
  cdfUtxo              <- cdf2OfCDFs comb $ mps <&> cdfUtxo
  cdfDensity           <- cdf2OfCDFs comb $ mps <&> cdfDensity
  cdfStarted           <- cdf2OfCDFs comb $ mps <&> cdfStarted
  cdfBlkCtx            <- cdf2OfCDFs comb $ mps <&> cdfBlkCtx
  cdfLgrState          <- cdf2OfCDFs comb $ mps <&> cdfLgrState
  cdfLgrView           <- cdf2OfCDFs comb $ mps <&> cdfLgrView
  cdfLeading           <- cdf2OfCDFs comb $ mps <&> cdfLeading
  cdfBlockGap          <- cdf2OfCDFs comb $ mps <&> cdfBlockGap
  cdfSpanLensCpu       <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpu
  cdfSpanLensCpuEpoch  <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuEpoch
  cdfSpanLensCpuRwd    <- cdf2OfCDFs comb $ mps <&> cdfSpanLensCpuRwd
  mpResourceCDFs       <- sequence $ traverse identity (mps <&> mpResourceCDFs) <&>
    \case
      [] -> Left CDFEmptyDataset
      (xs :: [CDF (CDF I) Word64]) -> cdf2OfCDFs comb xs :: Either CDFError (CDF (CDF I) Word64)

  pure . MultiClusterPerf $ MachPerf
    { mpVersion        = mpVersion headline
    , mpDomainSlots    = slotDomains
    , mpDomainCDFSlots =
      -- The simpler option, smashing the data from multiple runs into a single CDF:
        slotDomains & traverseDataDomain (cdf stdCentiles . fmap unI)
      -- Arguably, the proper option:
      -- mps <&> mpDomainCDFSlots
      --   & traverseDataDomain (cdf2OfCDFs comb)
    , ..
    }
 where
   slotDomains :: [DataDomain I SlotNo]
   slotDomains = concat $ mps <&> mpDomainSlots

   comb :: forall a. Divisible a => Combine (CDF I) a
   comb = stdCombine2 centiles
