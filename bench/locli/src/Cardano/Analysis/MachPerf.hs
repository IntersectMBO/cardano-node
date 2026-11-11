{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}

{- HLINT ignore "Use head" -}
{- HLINT ignore "Use maybe" -}
{- HLINT ignore "Use mapM" -}

module Cardano.Analysis.MachPerf (module Cardano.Analysis.MachPerf) where

import Cardano.Prelude hiding (head, toText)
import Cardano.Prelude qualified as CP

import Data.List                        ((!!))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text                        (pack, unpack)
import Data.Text.Short                  (toText)

import Data.Time.Clock qualified as Time

import Data.CDF
import Cardano.Util
import Cardano.Analysis.API
import Cardano.Unlog.LogObject
import Cardano.Unlog.Resources


-- * 1. Collect SlotStats & RunScalars:
--
collectSlotStats :: Run -> [(LogObjectSource, [LogObject])]
                 -> IO (Either Text [(LogObjectSource, (RunScalars, [SlotStats UTCTime]))])
collectSlotStats run = fmap sequence <$> mapConcurrentlyPure (timelineFromLogObjects run)


timelineFromLogObjects :: Run -> (LogObjectSource, [LogObject])
                       -> Either Text (LogObjectSource, (RunScalars, [SlotStats UTCTime]))
timelineFromLogObjects _ (f, []) =
  Left $ "timelineFromLogObjects:  zero logobjects from " <> pack (logObjectSourceFile f)
timelineFromLogObjects run@Run{genesis} (f, xs) =
  Right . (f,)
  $ foldl' (timelineStep run f) zeroTimelineAccum xs
  & (aRunScalars &&& reverse . aSlotStats)
 where
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
     , slEpoch = EpochNo 0
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
     , slSnapshot    = SNothing
     , slRejectedTx = 0
     , slBlockNo = 0
     , slBlockGap = 0
     , slLogObjects  = []
     }

timelineStep :: Run -> LogObjectSource -> TimelineAccum -> LogObject -> TimelineAccum
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
             , " file=", logObjectSourceFile f
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
           -- these metrics have moved to LOLedgerMetrics, but we still support values from former TraceStartLeadershipCheckPlus parses
           , slUtxoSize = if utxo > 0 then utxo else slUtxoSize
           , slDensity = if density > 0.0 then density else slDensity
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
  -- we only count snapshot completion events (those that have an enclosedTime in their trace)
  LogObject{loAt, loBody=LOLedgerTookSnapshot{loEnclosedTime=SJust duration}} ->
    forTAHead accum
      (\s-> s { slChainDBSnap = slChainDBSnap cur + 1
              , slSnapshot    = SJust (loAt, duration) })
  LogObject{loBody=LOLedgerMetrics _ utxo density} ->
    forTAHead accum
      (\s-> s { slUtxoSize = utxo, slDensity = density })
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
      , slTxsRejected = slTxsRejected + fromIntegral (max 0 rej)
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
          , slSnapshot    = SNothing
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
        , slSnapshot    = SNothing
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
  -> [(LogObjectSource, [SlotStats a])]
  -> IO (DataDomain I SlotNo, [(LogObjectSource, [SlotStats a])])
runSlotFilters Run{genesis} flts slots =
  mapConcurrentlyPure (fmap $ filterSlotStats flts) slots
    <&> \filtered ->
          (,) (domain filtered) filtered
 where
   domain :: [(LogObjectSource, [SlotStats a])] -> DataDomain I SlotNo
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
  , sssSpanLensCpuSnap  :: [Int]
  , sssSpanLensCpuOther :: [Int]
  }

-- | Whether the time interval overlaps any of the windows.
overlapsAny :: [(UTCTime, UTCTime)] -> (UTCTime, UTCTime) -> Bool
overlapsAny windows (start, end) = any (\(from, to) -> from <= end && start <= to) windows

-- | Over-85% CPU peaks: maximal runs of consecutive slots (in slot time: a gap left by the slot
--   filters ends a run) at >= 85% CPU; slots without a resource sample count as high.
--   Each peak is attributed to one cause, in this order:
--   1. the epoch transition: the peak ends at most 3 slots before the epoch's first block is
--      adopted (the ledger computes the transition with it); counted from the epoch's first slot;
--   2. a ledger snapshot: the peak overlaps a snapshot write window;
--   3. other.
--   Lengths are slot counts.
slotStatsSummary :: forall a. NominalDiffTime -> [(UTCTime, UTCTime)] -> [SlotStats a] -> SlotStatsSummary
slotStatsSummary slotLength snapshotWindows slots =
  SlotStatsSummary
  { sssSpanLensCpu      = length <$> spansCpu
  , sssSpanLensCpuEpoch = [ n | (EpochTransition n, _)  <- classified ]
  , sssSpanLensCpuSnap  = [ length sp | (SnapshotWrite, sp) <- classified ]
  , sssSpanLensCpuOther = [ length sp | (OtherCause,    sp) <- classified ]
  }
 where
   high :: SlotStats a -> Bool
   high = (/= SJust False) . fmap ((>=85) . rCentiCpu) . slResources

   spansCpu :: [NonEmpty (SlotStats a)]
   spansCpu = go slots
    where
      go xs = case dropWhile (not . high) xs of
        []      -> []
        x : xs' -> let (run, rest) = consecutive x xs' in (x :| run) : go rest
      consecutive prev (y : ys)
        | high y, slSlot y == slSlot prev + 1 = first (y :) (consecutive y ys)
      consecutive _ ys = ([], ys)

   -- Per epoch, the slot where its first block is adopted: the first change of the chain's block
   -- number within the epoch -- only for epochs whose first slot was analysed.
   firstBlockAdoptions :: Map.Map EpochNo (SlotStats a)
   firstBlockAdoptions = Map.fromListWith (\_ earlier -> earlier)
     [ (slEpoch cur, cur)
     | (prev, cur) <- zip slots (drop 1 slots)
     , slBlockNo cur /= slBlockNo prev
     , slEpoch cur `elem` epochsFromStart ]

   epochsFromStart :: [EpochNo]
   epochsFromStart = [ slEpoch sl | sl <- slots, unEpochSlot (slEpochSlot sl) == 0 ]

   classified :: [(Cause, NonEmpty (SlotStats a))]
   classified = spansCpu <&> \sp -> (,sp) $
     case [ n | let end = slSlot (NE.last sp)
              , adopted <- Map.elems firstBlockAdoptions
              , end <= slSlot adopted, slSlot adopted - end <= 3
              , let n = length $ NE.filter ((== slEpoch adopted) . slEpoch) sp
              , n > 0 ] of
       n : _ -> EpochTransition n
       []
         | overlapsAny snapshotWindows (spanTime sp) -> SnapshotWrite
         | otherwise                                 -> OtherCause

   spanTime :: NonEmpty (SlotStats a) -> (UTCTime, UTCTime)
   spanTime sp = ( unSlotStart (slStart (NE.head sp))
                 , slotLength `Time.addUTCTime` unSlotStart (slStart (NE.last sp)) )

data Cause = EpochTransition Int | SnapshotWrite | OtherCause

-- * 4. Summarise SlotStats & SlotStatsSummary into MachPerf:
--
slotStatsMachPerf :: Run -> (LogObjectSource, [SlotStats NominalDiffTime]) -> Either Text (LogObjectSource, MachPerfOne)
slotStatsMachPerf _ (f, []) =
  Left $ "slotStatsMachPerf:  zero filtered slots from " <> pack (logObjectSourceFile f)
slotStatsMachPerf run (f, slots) =
  Right (f, MachPerf
    { mpVersion            = getLocliVersion
    , mpDomainSlots        = domSlots
    , mpDomainCDFSlots     = domSlots -- At unit-arity it's just a replica.
    , cdfHostSlots         = dist [fromIntegral (length slots)]  -- slots analysed, not the domain's width
    --
    , cdfStarts            = dist (slCountStarts <$> slots)
    , cdfLeads             = dist (slCountLeads <$> slots)
    , cdfDensity           = dist (slDensity <$> slots)
    , cdfStarted           = dist (slStarted `mapSMaybe` slots)
    , cdfBlkCtx            = dist (slBlkCtx `mapSMaybe` slots)
    , cdfLgrState          = dist (slLgrState `mapSMaybe` slots)
    , cdfLgrView           = dist (slLgrView `mapSMaybe` slots)
    , cdfLeading           = dist (slLeading `mapSMaybe` slots)
    , cdfBlockGap          = dist (slBlockGap <$> slots)
    , cdfSpanLensCpu       = dist sssSpanLensCpu
    , cdfSpanLensCpuEpoch  = dist sssSpanLensCpuEpoch
    , cdfSpanLensCpuSnap   = dist sssSpanLensCpuSnap
    , cdfSpanLensCpuOther  = dist sssSpanLensCpuOther
    --
    , cdfSnapTaken         = dist [sum (slChainDBSnap <$> slots)]
    , cdfSnapDuration      = dist (snd <$> snapshots)
    , cdfLiveMajGcDuring   = dist [ rLive r | (sl, r) <- majorGcSlots, duringSnapshot sl ]
    , cdfLiveMajGcOutside  = dist [ rLive r | (sl, r) <- majorGcSlots, not (duringSnapshot sl) ]
    --
    , cdfTxsProcessed      = dist [txsProcessed]
    , cdfTxsAccepted       = dist [txsAccepted]
    , cdfTxsRejected       = dist [txsRejected]
    , cdfTxsRejectRatio    = dist [perProcessedTx (fromIntegral txsRejected)]
    , cdfMempoolTxs        = dist (slMempoolTxs <$> slots)
    , cdfCpuPerTx          = dist [cpuMsPerTx rCentiCpu]
    , cdfMutPerTx          = dist [cpuMsPerTx rCentiMut]
    , cdfGcPerTx           = dist [cpuMsPerTx rCentiGC]
    , mpResourceCDFs       = computeResCDF stdCentiles slResources slots
    , ..
    }
  )
 where
   Run{genesis = Genesis{slotLength}} = run

   -- Ledger snapshots: completion time and duration of each.
   snapshots :: [(UTCTime, NominalDiffTime)]
   snapshots = slSnapshot `mapSMaybe` slots

   -- A snapshot write window spans from its start until 5s after its completion,
   -- to also catch the major GC that typically follows the write.
   snapshotWindows :: [(UTCTime, UTCTime)]
   snapshotWindows = snapshots <&>
     \(done, duration) -> (negate duration `Time.addUTCTime` done, 5 `Time.addUTCTime` done)

   duringSnapshot :: SlotStats a -> Bool
   duringSnapshot SlotStats{slStart = SlotStart start} =
     overlapsAny snapshotWindows (start, slotLength `Time.addUTCTime` start)

   -- Slots whose resource sample saw at least one major GC; their live data is the one after it.
   majorGcSlots :: [(SlotStats NominalDiffTime, Resources Word64)]
   majorGcSlots = [ (sl, r) | sl@SlotStats{slResources = SJust r} <- slots, rGcsMajor r > 0 ]

   -- Transaction processing: run totals. Per event, the node's 'rejected' count is unreliable
   -- (computed against a different batch than 'accepted'); over the run, collected minus accepted is exact.
   txsProcessed, txsAccepted, txsRejected :: Word64
   txsProcessed = sum (slTxsCollected <$> slots)
   txsAccepted  = sum (slTxsAccepted  <$> slots)
   txsRejected  = txsProcessed - min txsProcessed txsAccepted

   perProcessedTx :: Double -> Double
   perProcessedTx x = if txsProcessed == 0 then 0 else x / fromIntegral txsProcessed

   -- CPU time over the analysed slots, in ms per processed tx: the mean per-slot CPU share
   -- (% of a core) times the analysed time, robust against slots without a resource sample.
   cpuMsPerTx :: (Resources Word64 -> Word64) -> Double
   cpuMsPerTx proj = perProcessedTx $ meanShare / 100 * analysedSeconds * 1000
    where
      shares = [ fromIntegral (proj r) | SJust r <- slResources <$> slots ] :: [Double]
      meanShare = if null shares then 0 else sum shares / fromIntegral (length shares)
      analysedSeconds = fromIntegral (length slots) * realToFrac slotLength
   domSlots = mkDataDomainInj sFirst sLast (fromIntegral . unSlotNo)

   (,) sFirst sLast = (slSlot . head &&& slSlot . last) slots

   dist :: Divisible a => [a] -> CDF I a
   dist = cdfZ stdCentiles

   SlotStatsSummary{..} = slotStatsSummary slotLength snapshotWindows slots

-- * 5. Multi-machine & multi-run summaries:
--
summariseClusterPerf :: [Centile] -> [MachPerfOne] -> Either CDFError ClusterPerf
summariseClusterPerf _ [] = error "Asked to summarise empty list of MachPerfOne"
summariseClusterPerf centiles mps@(headline:_) = do
  cdfHostSlots         <- cdf2OfCDFs comb $ mps <&> cdfHostSlots
  cdfStarts            <- cdf2OfCDFs comb $ mps <&> cdfStarts
  cdfLeads             <- cdf2OfCDFs comb $ mps <&> cdfLeads
  cdfDensity           <- cdf2OfCDFs comb $ mps <&> cdfDensity
  cdfStarted           <- cdf2OfCDFs comb $ mps <&> cdfStarted
  cdfBlkCtx            <- cdf2OfCDFs comb $ mps <&> cdfBlkCtx
  cdfLgrState          <- cdf2OfCDFs comb $ mps <&> cdfLgrState
  cdfLgrView           <- cdf2OfCDFs comb $ mps <&> cdfLgrView
  cdfLeading           <- cdf2OfCDFs comb $ mps <&> cdfLeading
  cdfBlockGap          <- cdf2OfCDFs comb $ mps <&> cdfBlockGap
  cdfSpanLensCpu       <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpu
  cdfSpanLensCpuEpoch  <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpuEpoch
  cdfSpanLensCpuSnap   <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpuSnap
  cdfSpanLensCpuOther  <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpuOther
  cdfSnapTaken         <- cdf2OfCDFs comb $ mps <&> cdfSnapTaken
  cdfSnapDuration      <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSnapDuration
  cdfLiveMajGcDuring   <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLiveMajGcDuring
  cdfLiveMajGcOutside  <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLiveMajGcOutside
  cdfTxsProcessed      <- cdf2OfCDFs comb $ mps <&> cdfTxsProcessed
  cdfTxsAccepted       <- cdf2OfCDFs comb $ mps <&> cdfTxsAccepted
  cdfTxsRejected       <- cdf2OfCDFs comb $ mps <&> cdfTxsRejected
  cdfTxsRejectRatio    <- cdf2OfCDFs comb $ mps <&> cdfTxsRejectRatio
  cdfMempoolTxs        <- cdf2OfCDFs comb $ mps <&> cdfMempoolTxs
  cdfCpuPerTx          <- cdf2OfCDFs comb $ mps <&> cdfCpuPerTx
  cdfMutPerTx          <- cdf2OfCDFs comb $ mps <&> cdfMutPerTx
  cdfGcPerTx           <- cdf2OfCDFs comb $ mps <&> cdfGcPerTx
  mpResourceCDFs       <- sequence $ traverse (identity . mpResourceCDFs) mps <&>
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
  cdfHostSlots         <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfHostSlots
  cdfStarts            <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfStarts
  cdfLeads             <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLeads
  cdfDensity           <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfDensity
  cdfStarted           <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfStarted
  cdfBlkCtx            <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfBlkCtx
  cdfLgrState          <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLgrState
  cdfLgrView           <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLgrView
  cdfLeading           <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLeading
  cdfBlockGap          <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfBlockGap
  cdfSpanLensCpu       <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpu
  cdfSpanLensCpuEpoch  <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpuEpoch
  cdfSpanLensCpuSnap   <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpuSnap
  cdfSpanLensCpuOther  <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSpanLensCpuOther
  cdfSnapTaken         <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSnapTaken
  cdfSnapDuration      <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfSnapDuration
  cdfLiveMajGcDuring   <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLiveMajGcDuring
  cdfLiveMajGcOutside  <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfLiveMajGcOutside
  cdfTxsProcessed      <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfTxsProcessed
  cdfTxsAccepted       <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfTxsAccepted
  cdfTxsRejected       <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfTxsRejected
  cdfTxsRejectRatio    <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfTxsRejectRatio
  cdfMempoolTxs        <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfMempoolTxs
  cdfCpuPerTx          <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfCpuPerTx
  cdfMutPerTx          <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfMutPerTx
  cdfGcPerTx           <- cdf2OfCDFs comb $ nonEmptyCDFs $ mps <&> cdfGcPerTx
  mpResourceCDFs       <- sequence $ traverse (identity . mpResourceCDFs) mps <&>
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
   slotDomains = concatMap mpDomainSlots mps

   comb :: forall a. Divisible a => Combine (CDF I) a
   comb = stdCombine2 centiles
