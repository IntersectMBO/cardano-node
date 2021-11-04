{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.MachTimeline (module Cardano.Analysis.MachTimeline) where

import Prelude (String, (!!), error, head, last)
import Cardano.Prelude hiding (head)

import Control.Arrow ((&&&), (***))
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Map.Strict qualified as Map

import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock qualified as Time

import Ouroboros.Network.Block (SlotNo(..))

import Data.Accum
import Data.Distribution

import Cardano.Analysis.API
import Cardano.Analysis.Chain
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Unlog.LogObject hiding (Text)
import Cardano.Unlog.Render
import Cardano.Unlog.Resources

instance RenderDistributions MachTimeline where
  rdFields _ =
    --  Width LeftPad
    [ Field 4 0 "missR"    "Miss"  "ratio"  $ DFloat   sMissDistrib
    , Field 5 0 "CheckΔ"   (d!!0)  "Check"  $ DDeltaT  sSpanCheckDistrib
    , Field 5 0 "LeadΔ"    (d!!1)  "Lead"   $ DDeltaT  sSpanLeadDistrib
    , Field 5 0 "ForgeΔ"   (d!!2)  "Forge"  $ DDeltaT  sSpanForgeDistrib
    , Field 4 0 "BlkGap"   "Block" "gap"    $ DWord64  sBlocklessDistrib
    , Field 5 0 "chDensity" "Dens" "ity"    $ DFloat   sDensityDistrib
    , Field 3 0 "CPU"      "CPU"   "%"      $ DWord64 (rCentiCpu . sResourceDistribs)
    , Field 3 0 "GC"       "GC"    "%"      $ DWord64 (rCentiGC . sResourceDistribs)
    , Field 3 0 "MUT"      "MUT"    "%"     $ DWord64 (fmap (min 999) . rCentiMut . sResourceDistribs)
    , Field 3 0 "GcMaj"    "GC "   "Maj"    $ DWord64 (rGcsMajor . sResourceDistribs)
    , Field 3 0 "GcMin"    "flt "  "Min"    $ DWord64 (rGcsMinor . sResourceDistribs)
    , Field 5 0 "RSS"      (m!!0)  "RSS"    $ DWord64 (rRSS . sResourceDistribs)
    , Field 5 0 "Heap"     (m!!1)  "Heap"   $ DWord64 (rHeap . sResourceDistribs)
    , Field 5 0 "Live"     (m!!2)  "Live"   $ DWord64 (rLive . sResourceDistribs)
    , Field 5 0 "Allocd"   "Alloc" "MB"     $ DWord64 (rAlloc . sResourceDistribs)
    , Field 5 0 "CPU85%LensAll"  (c!!0) "All"   $ DInt     sSpanLensCPU85Distrib
    , Field 5 0 "CPU85%LensEBnd" (c!!1) "EBnd"  $ DInt     sSpanLensCPU85EBndDistrib
    ]
   where
     d = nChunksEachOf  3 6 "---- Δt ----"
     m = nChunksEachOf  3 6 "Memory usage, MB"
     c = nChunksEachOf  2 6 "CPU85% spans"

slotStatsMachTimeline :: Run -> [SlotStats] -> MachTimeline
slotStatsMachTimeline _ slots =
  MachTimeline
  { sMaxChecks            = maxChecks
  , sSlotMisses           = misses
  , sSpanLensCPU85        = spanLensCPU85
  , sSpanLensCPU85EBnd    = sSpanLensCPU85EBnd
  , sSpanLensCPU85Rwd     = sSpanLensCPU85Rwd
  , sSlotRange            = (,) (slSlot $ head slots)
                                (slSlot $ last slots)
  , sVersion              = getVersion
  --
  , sMissDistrib          = dist missRatios
  , sLeadsDistrib         = dist (slCountLeads <$> slots)
  , sUtxoDistrib          = dist (slUtxoSize <$> slots)
  , sDensityDistrib       = dist (slDensity <$> slots)
  , sSpanCheckDistrib     = dist (slSpanCheck <$> slots)
  , sSpanLeadDistrib      = dist (slSpanLead <$> slots)
  , sSpanForgeDistrib     = dist (filter (/= 0) $ slSpanForge <$> slots)
  , sBlocklessDistrib     = dist (slBlockless <$> slots)
  , sSpanLensCPU85Distrib = dist spanLensCPU85
  , sSpanLensCPU85EBndDistrib = dist sSpanLensCPU85EBnd
  , sSpanLensCPU85RwdDistrib  = dist sSpanLensCPU85Rwd
  , sResourceDistribs         = computeResDistrib stdPercentiles resDistProjs slots
  }
 where
   dist :: (Real a, ToRealFrac a Float) => [a] -> Distribution Float a
   dist = computeDistribution stdPercentiles
   sSpanLensCPU85EBnd = Vec.length <$>
                        filter (spanContainsEpochSlot 3) spansCPU85
   sSpanLensCPU85Rwd  = Vec.length <$>
                        filter (spanContainsEpochSlot 803) spansCPU85

   checkCounts      = slCountChecks <$> slots
   maxChecks        = if length checkCounts == 0
                      then 0 else maximum checkCounts
   misses           = (maxChecks -) <$> checkCounts
   missRatios       = missRatio <$> misses
   spansCPU85 :: [Vector SlotStats]
   spansCPU85       = spans
                        ((/= Just False) . fmap (>=85) . rCentiCpu . slResources)
                        (toList slots)
   spanLensCPU85    = spanLen <$> spansCPU85
   spanContainsEpochSlot :: Word64 -> Vector SlotStats -> Bool
   spanContainsEpochSlot s =
     uncurry (&&)
     . ((s >) . unEpochSlot . slEpochSlot . Vec.head &&&
        (s <) . unEpochSlot . slEpochSlot . Vec.last)
   spanLen :: Vector SlotStats -> Int
   spanLen = fromIntegral . unSlotNo . uncurry (-) . (slSlot *** slSlot) . (Vec.last &&& Vec.head)
   resDistProjs     =
     Resources
     { rCentiCpu    = rCentiCpu   . slResources
     , rCentiGC     = rCentiGC    . slResources
     , rCentiMut    = rCentiMut   . slResources
     , rGcsMajor    = rGcsMajor   . slResources
     , rGcsMinor    = rGcsMinor   . slResources
     , rRSS         = rRSS        . slResources
     , rHeap        = rHeap       . slResources
     , rLive        = rLive       . slResources
     , rAlloc       = rAlloc      . slResources
     , rCentiBlkIO  = rCentiBlkIO . slResources
     , rThreads     = rThreads    . slResources
     }

   missRatio :: Word64 -> Float
   missRatio = (/ fromIntegral maxChecks) . fromIntegral

-- The "fold" state that accumulates as we process 'LogObject's into a stream
-- of 'SlotStats'.
data TimelineAccum
  = TimelineAccum
  { aResAccums     :: ResAccums
  , aResTimestamp  :: UTCTime
  , aMempoolTxs    :: Word64
  , aBlockNo       :: Word64
  , aLastBlockSlot :: SlotNo
  , aSlotStats     :: [SlotStats]
  , aRunScalars    :: RunScalars
  , aTxsCollectedAt:: Map.Map TId UTCTime
  }

forTAHead :: TimelineAccum -> (SlotStats -> SlotStats) -> TimelineAccum
forTAHead xs@TimelineAccum{aSlotStats=s:ss} f = xs {aSlotStats=f s:ss}

forTANth :: TimelineAccum -> Int -> (SlotStats -> SlotStats) -> TimelineAccum
forTANth xs@TimelineAccum{aSlotStats=ss} n f =
  xs { aSlotStats = mapNth f n ss }
 where
   mapNth :: (a -> a) -> Int -> [a] -> [a]
   mapNth f n xs =
     case splitAt n xs of
       (pre, x:post) -> pre <> (f x : post)
       _ -> error $ "mapNth: couldn't go " <> show n <> "-deep into the timeline"

data RunScalars
  = RunScalars
  { rsElapsed       :: Maybe NominalDiffTime
  , rsSubmitted     :: Maybe Word64
  , rsThreadwiseTps :: Maybe (Vector Float)
  }

timelineFromLogObjects :: Run -> [LogObject] -> (RunScalars, [SlotStats])
timelineFromLogObjects run =
  (aRunScalars &&& reverse . aSlotStats)
  . foldl (timelineStep run) zeroTimelineAccum
 where
   zeroTimelineAccum :: TimelineAccum
   zeroTimelineAccum =
     TimelineAccum
     { aResAccums     = mkResAccums
     , aResTimestamp  = zeroUTCTime
     , aMempoolTxs    = 0
     , aBlockNo       = 0
     , aLastBlockSlot = 0
     , aSlotStats     = [zeroSlotStats]
     , aRunScalars    = zeroRunScalars
     , aTxsCollectedAt= mempty
     }
   zeroRunScalars :: RunScalars
   zeroRunScalars = RunScalars Nothing Nothing Nothing
   zeroSlotStats :: SlotStats
   zeroSlotStats =
     SlotStats
     { slSlot = 0
     , slEpoch = 0
     , slEpochSlot = 0
     , slEpochSafeInt = 0
     , slStart = SlotStart zeroUTCTime
     , slCountChecks = 0
     , slCountLeads = 0
     , slCountForges = 0
     , slEarliest = zeroUTCTime
     , slSpanCheck = realToFrac (0 :: Int)
     , slSpanLead = realToFrac (0 :: Int)
     , slSpanForge = realToFrac (0 :: Int)
     , slMempoolTxs = 0
     , slTxsMemSpan = Nothing
     , slTxsCollected = 0
     , slTxsAccepted = 0
     , slTxsRejected = 0
     , slUtxoSize = 0
     , slDensity = 0
     , slResources = pure Nothing
     , slChainDBSnap = 0
     , slRejectedTx = 0
     , slBlockNo = 0
     , slBlockless = 0
     }

timelineStep :: Run -> TimelineAccum -> LogObject -> TimelineAccum
timelineStep run@Run{genesis} a@TimelineAccum{aSlotStats=cur:_, ..} = \case
  LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo density} ->
    if      slot == slSlot cur     -- L-shipCheck for the current slot.
    then forTAHead a
         (registerLeadCheck loAt)
    else if slot - slSlot cur == 1 -- L-shipCheck for the next slot.
    then forTAHead (addTimelineSlot run slot loAt 0 utxo density a)
         (registerLeadCheck loAt)
    else if slot < slSlot cur      -- L-shipCheck for a slot we've gone by already.
    then forTANth a (fromIntegral . unSlotNo $ slSlot cur - slot)
                  (registerLeadCheck loAt)
         -- L-shipCheck for a further-than-immediate future slot
    else let gap = unSlotNo $ slot - slSlot cur - 1
             gapStartSlot = slSlot cur + 1 in
         patchSlotCheckGap gap gapStartSlot a
         & addTimelineSlot run slot loAt 1 utxo density
   where
     registerLeadCheck :: UTCTime -> SlotStats -> SlotStats
     registerLeadCheck now sl@SlotStats{..} =
       sl { slCountChecks = slCountChecks + 1
          , slSpanCheck = now `sinceSlot` slStart -- XXX: used to "max 0" this
          }

     patchSlotCheckGap :: Word64 -> SlotNo -> TimelineAccum -> TimelineAccum
     patchSlotCheckGap 0 _ a' = a'
     patchSlotCheckGap gapLen slot a'@TimelineAccum{aSlotStats=cur':_} =
       patchSlotCheckGap (gapLen - 1) (slot + 1) $
        addTimelineSlot run slot
          (unSlotStart $ genesis `slotStart` slot)
          0 (slUtxoSize cur') (slDensity cur') a'
  LogObject{loAt, loBody=LOTraceLeadershipDecided slot yesNo} ->
    if slot /= slSlot cur
    then error $ "LeadDecided for noncurrent slot=" <> show slot <> " cur=" <> show (slSlot cur)
    else forTAHead a (onLeadershipCertainty loAt yesNo)
   where
     onLeadershipCertainty :: UTCTime -> Bool -> SlotStats -> SlotStats
     onLeadershipCertainty now lead sl@SlotStats{..} =
       sl { slCountLeads = slCountLeads + if lead then 1 else 0
          , slSpanLead   = checkToCertainty
          }
      where
        checkAbsTime = slSpanCheck `Time.addUTCTime` unSlotStart slStart
        checkToCertainty = now `Time.diffUTCTime` checkAbsTime
  LogObject{loAt, loBody=LOBlockForged _ _ _ slot} ->
    if slot /= slSlot cur
    then error $ "BlockForged for noncurrent slot=" <> show slot <> " cur=" <> show (slSlot cur)
    else forTAHead a (onBlockForge loAt)
   where
     onBlockForge :: UTCTime -> SlotStats -> SlotStats
     onBlockForge now sl@SlotStats{..} =
       sl { slCountForges = slCountForges + 1
          , slSpanForge   = certaintyToForge
          }
      where
        certaintyAbsTime = slSpanLead `Time.addUTCTime` (slSpanCheck `Time.addUTCTime` unSlotStart slStart)
        certaintyToForge = now `Time.diffUTCTime` certaintyAbsTime
  LogObject{loAt, loBody=LOResources rs} ->
    -- Update resource stats accumulators & record values current slot.
    (forTAHead a
      \s-> s { slResources = Just <$> extractResAccums accs })
    { aResAccums = accs, aResTimestamp = loAt }
   where accs = updateResAccums loAt rs aResAccums
  LogObject{loBody=LOMempoolTxs txCount} ->
    (forTAHead a
      \s-> s { slMempoolTxs = txCount })
    { aMempoolTxs     = txCount }
  LogObject{loBody=LOBlockContext blockNo} ->
    (forTAHead a
      \s-> s { slBlockNo = blockNo
             , slBlockless = if newBlock then 0 else slBlockless cur
             })
    { aBlockNo        = blockNo
    , aLastBlockSlot  = if newBlock
                        then slSlot cur
                        else aLastBlockSlot
    }
   where
     newBlock = aBlockNo /= blockNo
  LogObject{loBody=LOLedgerTookSnapshot} ->
    forTAHead a
      \s-> s { slChainDBSnap = slChainDBSnap cur + 1 }
  LogObject{loBody=LOMempoolRejectedTx} ->
    forTAHead a
      \s-> s { slRejectedTx = slRejectedTx cur + 1 }
  LogObject{loBody=LOGeneratorSummary _noFails sent elapsed threadwiseTps} ->
    a { aRunScalars = aRunScalars
        { rsThreadwiseTps = Just threadwiseTps
        , rsElapsed       = Just elapsed
        , rsSubmitted     = Just sent
        }
      }
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
      { slTxsMemSpan =
          case loTid `Map.lookup` aTxsCollectedAt of
            Nothing ->
              -- error $ mconcat
              -- ["LOTxsProcessed missing LOTxsCollected for tid", show tid, " at ", show loAt]
              Just $
              1.0
              +
              fromMaybe 0 slTxsMemSpan
            Just base ->
              Just $
              (loAt `Time.diffUTCTime` base)
              +
              fromMaybe 0 slTxsMemSpan
      , slTxsAccepted = slTxsAccepted + acc
      , slTxsRejected = slTxsRejected + max 0 (fromIntegral rej)
      })
    { aTxsCollectedAt = loTid `Map.delete` aTxsCollectedAt
    }
  _ -> a
timelineStep _ a = const a

addTimelineSlot ::
     Run
  -> SlotNo -> UTCTime -> Word64 -> Word64 -> Float
  -> TimelineAccum -> TimelineAccum
addTimelineSlot Run{genesis} slot time checks utxo density a@TimelineAccum{..} =
  let (epoch, epochSlot) = genesis `unsafeParseSlot` slot in
    a { aSlotStats = SlotStats
        { slSlot        = slot
        , slEpoch       = epoch
        , slEpochSlot   = epochSlot
        , slEpochSafeInt= slotEpochSafeInt genesis epochSlot
        , slStart       = slStart
        , slEarliest    = time
          -- Updated as we see repeats:
        , slCountChecks = checks
        , slCountLeads  = 0
        , slCountForges = 0
        , slSpanCheck   = time `sinceSlot` slStart
        , slSpanLead    = 0
        , slSpanForge   = 0
        , slTxsMemSpan  = Nothing
        , slTxsCollected= 0
        , slTxsAccepted = 0
        , slTxsRejected = 0
        , slMempoolTxs  = aMempoolTxs
        , slUtxoSize    = utxo
        , slDensity     = density
        , slChainDBSnap = 0
        , slRejectedTx  = 0
        , slBlockNo     = aBlockNo
        , slBlockless   = unSlotNo $ slot - aLastBlockSlot
        , slResources   = maybeDiscard
                          <$> discardObsoleteValues
                          <*> extractResAccums aResAccums}
        : aSlotStats
      }
    where maybeDiscard :: (Word64 -> Maybe Word64) -> Word64 -> Maybe Word64
          maybeDiscard f = f

          slStart = slotStart genesis slot

data DerivedSlot
  = DerivedSlot
  { dsSlot      :: SlotNo
  , dsBlockless :: Word64
  }

derivedSlotsHeader :: String
derivedSlotsHeader =
  "Slot,Blockless span"

renderDerivedSlot :: DerivedSlot -> String
renderDerivedSlot DerivedSlot{..} =
  mconcat
  [ show (unSlotNo dsSlot), ",", show dsBlockless
  ]

computeDerivedVectors :: [SlotStats] -> ([DerivedSlot], [DerivedSlot])
computeDerivedVectors ss =
  (\(_,_,d0,d1) -> (d0, d1)) $
  foldr step (0, 0, [], []) ss
 where
   step ::
        SlotStats
     -> (Word64, Word64, [DerivedSlot], [DerivedSlot])
     -> (Word64, Word64, [DerivedSlot], [DerivedSlot])
   step SlotStats{..} (lastBlockless, spanBLSC, accD0, accD1) =
     if lastBlockless < slBlockless
     then ( slBlockless
          , slBlockless
          , DerivedSlot
            { dsSlot = slSlot
            , dsBlockless = slBlockless
            }:accD0
          , DerivedSlot
            { dsSlot = slSlot
            , dsBlockless = slBlockless
            }:accD1
          )
     else ( slBlockless
          , spanBLSC
          , DerivedSlot
            { dsSlot = slSlot
            , dsBlockless = spanBLSC
            }:accD0
          , accD1
          )
