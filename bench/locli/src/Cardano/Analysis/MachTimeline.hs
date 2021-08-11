{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.MachTimeline (module Cardano.Analysis.MachTimeline) where

import           Prelude (String, (!!), error)
import           Cardano.Prelude

import           Control.Arrow ((&&&), (***))
import qualified Data.Aeson as AE
import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map

import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Time

import           Ouroboros.Network.Block (SlotNo(..))

import           Data.Accum
import           Data.Distribution

import           Cardano.Analysis.Profile
import           Cardano.Unlog.LogObject hiding (Text)
import           Cardano.Unlog.Render
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats

-- | The top-level representation of the machine timeline analysis results.
data MachTimeline
  = MachTimeline
    { sMaxChecks         :: !Word64
    , sSlotMisses        :: ![Word64]
    , sSpanLensCPU85     :: ![Int]
    , sSpanLensCPU85EBnd :: ![Int]
    , sSpanLensCPU85Rwd  :: ![Int]
    -- distributions
    , sMissDistrib       :: !(Distribution Float Float)
    , sLeadsDistrib      :: !(Distribution Float Word64)
    , sUtxoDistrib       :: !(Distribution Float Word64)
    , sDensityDistrib    :: !(Distribution Float Float)
    , sSpanCheckDistrib  :: !(Distribution Float NominalDiffTime)
    , sSpanLeadDistrib   :: !(Distribution Float NominalDiffTime)
    , sBlocklessDistrib  :: !(Distribution Float Word64)
    , sSpanLensCPU85Distrib
                         :: !(Distribution Float Int)
    , sSpanLensCPU85EBndDistrib :: !(Distribution Float Int)
    , sSpanLensCPU85RwdDistrib  :: !(Distribution Float Int)
    , sResourceDistribs  :: !(Resources (Distribution Float Word64))
    }
  deriving Show

instance RenderDistributions MachTimeline where
  rdFields _ =
    --  Width LeftPad
    [ Field 4 0 "missR"    "Miss"  "ratio"  $ DFloat   sMissDistrib
    , Field 6 0 "CheckΔ"   ""      "ChkΔt"  $ DDeltaT  sSpanCheckDistrib
    , Field 6 0 "LeadΔ"    ""      "LeadΔt" $ DDeltaT  sSpanLeadDistrib
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
     m = nChunksEachOf  3 6 "Memory usage, MB"
     c = nChunksEachOf  2 6 "CPU85% spans"

instance ToJSON MachTimeline where
  toJSON MachTimeline{..} = AE.Array $ Vec.fromList
    [ AE.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85EBnd"
        , "xs" .= toJSON sSpanLensCPU85EBnd]
    , AE.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85Rwd"
        , "xs" .= toJSON sSpanLensCPU85Rwd]
    , AE.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85"
        , "xs" .= toJSON sSpanLensCPU85]
    , AE.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85Sorted"
        , "xs" .= toJSON (sort sSpanLensCPU85)]
    , extendObject "kind" "spancheck" $ toJSON sSpanCheckDistrib
    , extendObject "kind" "spanlead"  $ toJSON sSpanLeadDistrib
    , extendObject "kind" "cpu"       $ toJSON (rCentiCpu sResourceDistribs)
    , extendObject "kind" "gc"        $ toJSON (rCentiGC  sResourceDistribs)
    , extendObject "kind" "density"   $ toJSON sDensityDistrib
    , extendObject "kind" "utxo"      $ toJSON sUtxoDistrib
    , extendObject "kind" "leads"     $ toJSON sLeadsDistrib
    , extendObject "kind" "misses"    $ toJSON sMissDistrib
    , extendObject "kind" "blockless" $ toJSON sBlocklessDistrib
    , extendObject "kind" "rss"       $ toJSON (rRSS      sResourceDistribs)
    , extendObject "kind" "heap"      $ toJSON (rHeap     sResourceDistribs)
    , extendObject "kind" "live"      $ toJSON (rLive     sResourceDistribs)
    , extendObject "kind" "spanLensCPU85Distrib"  $
                                        toJSON sSpanLensCPU85Distrib
    , extendObject "kind" "spanLensCPU85EBndDistrib"  $
                                        toJSON sSpanLensCPU85EBndDistrib
    , extendObject "kind" "spanLensCPU85RwdDistrib"  $
                                        toJSON sSpanLensCPU85RwdDistrib
    ]

slotStatsMachTimeline :: ChainInfo -> [SlotStats] -> MachTimeline
slotStatsMachTimeline CInfo{} slots =
  MachTimeline
  { sMaxChecks        = maxChecks
  , sSlotMisses       = misses
  , sSpanLensCPU85    = spanLensCPU85
  , sSpanLensCPU85EBnd = sSpanLensCPU85EBnd
  , sSpanLensCPU85Rwd  = sSpanLensCPU85Rwd
  --
  , sMissDistrib      = computeDistribution stdPercentiles missRatios
  , sLeadsDistrib     =
      computeDistribution stdPercentiles (slCountLeads <$> slots)
  , sUtxoDistrib      =
      computeDistribution stdPercentiles (slUtxoSize <$> slots)
  , sDensityDistrib   =
      computeDistribution stdPercentiles (slDensity <$> slots)
  , sSpanCheckDistrib =
      computeDistribution stdPercentiles (slSpanCheck <$> slots)
  , sSpanLeadDistrib =
      computeDistribution stdPercentiles (slSpanLead <$> slots)
  , sBlocklessDistrib =
      computeDistribution stdPercentiles (slBlockless <$> slots)
  , sSpanLensCPU85Distrib
                      = computeDistribution stdPercentiles spanLensCPU85
  , sResourceDistribs =
      computeResDistrib stdPercentiles resDistProjs slots
  , sSpanLensCPU85EBndDistrib = computeDistribution stdPercentiles sSpanLensCPU85EBnd
  , sSpanLensCPU85RwdDistrib  = computeDistribution stdPercentiles sSpanLensCPU85Rwd
  }
 where
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
     . ((s >) . slEpochSlot . Vec.head &&&
        (s <) . slEpochSlot . Vec.last)
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

data RunScalars
  = RunScalars
  { rsElapsed       :: Maybe NominalDiffTime
  , rsSubmitted     :: Maybe Word64
  , rsThreadwiseTps :: Maybe (Vector Float)
  }

timelineFromLogObjects :: ChainInfo -> [LogObject] -> (RunScalars, [SlotStats])
timelineFromLogObjects ci =
  (aRunScalars &&& reverse . aSlotStats)
  . foldl (timelineStep ci) zeroTimelineAccum
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

timelineStep :: ChainInfo -> TimelineAccum -> LogObject -> TimelineAccum
timelineStep ci a@TimelineAccum{aSlotStats=cur:rSLs, ..} = \case
  lo@LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot _ _} ->
    if slSlot cur > slot
    -- Slot log entry for a slot we've supposedly done processing.
    then a { aSlotStats = cur
             { slOrderViol = slOrderViol cur + 1
             } : case (slSlot cur - slot, rSLs) of
                   -- Limited back-patching:
                   (1, p1:rest)       ->       onLeadershipCheck loAt p1:rest
                   (2, p1:p2:rest)    ->    p1:onLeadershipCheck loAt p2:rest
                   (3, p1:p2:p3:rest) -> p1:p2:onLeadershipCheck loAt p3:rest
                   _ -> rSLs -- Give up.
           }
    else if slSlot cur == slot
    then a { aSlotStats = onLeadershipCheck loAt cur : rSLs
           }
    else if slot - slSlot cur > 1
    then let gap = unSlotNo $ slot - slSlot cur - 1
             gapStartSlot = slSlot cur + 1 in
         updateOnNewSlot lo $ -- We have a slot check gap to patch:
         patchSlotCheckGap gap gapStartSlot a
    else updateOnNewSlot lo a
  LogObject{loAt, loBody=LOTraceNodeIsLeader _} ->
    a { aSlotStats = onLeadershipCertainty loAt True cur : rSLs
      }
  LogObject{loAt, loBody=LOTraceNodeNotLeader _} ->
    a { aSlotStats = onLeadershipCertainty loAt False cur : rSLs
      }
  LogObject{loAt, loBody=LOResources rs} ->
    -- Update resource stats accumulators & record values current slot.
    a { aResAccums = accs
      , aResTimestamp = loAt
      , aSlotStats = cur { slResources = Just <$> extractResAccums accs
                     } : rSLs
      }
   where accs = updateResAccums loAt rs aResAccums
  LogObject{loBody=LOMempoolTxs txCount} ->
    a { aMempoolTxs     = txCount
      , aSlotStats      = cur { slMempoolTxs = txCount
                          } : rSLs
      }
  LogObject{loBody=LOBlockContext blockNo} ->
    let newBlock = aBlockNo /= blockNo in
    a { aBlockNo        = blockNo
      , aLastBlockSlot  = if newBlock
                          then slSlot cur
                          else aLastBlockSlot
      , aSlotStats      = cur { slBlockNo = blockNo
                              , slBlockless = if newBlock
                                              then 0
                                              else slBlockless cur
                              } : rSLs
      }
  LogObject{loBody=LOLedgerTookSnapshot} ->
    a { aSlotStats      = cur { slChainDBSnap = slChainDBSnap cur + 1
                              } : rSLs
      }
  LogObject{loBody=LOMempoolRejectedTx} ->
    a { aSlotStats      = cur { slRejectedTx = slRejectedTx cur + 1
                              } : rSLs
      }
  LogObject{loBody=LOGeneratorSummary _noFails sent elapsed threadwiseTps} ->
    a { aRunScalars       =
        aRunScalars
        { rsThreadwiseTps = Just threadwiseTps
        , rsElapsed       = Just elapsed
        , rsSubmitted     = Just sent
        }
      }
  LogObject{loBody=LOTxsCollected coll, loTid, loAt} ->
    a { aTxsCollectedAt =
        aTxsCollectedAt &
        (\case
            Just{} -> Just loAt
            --   error $ mconcat
            --   ["Duplicate LOTxsCollected for tid ", show tid, " at ", show loAt]
            Nothing -> Just loAt)
        `Map.alter` loTid
      , aSlotStats      =
        cur
        { slTxsCollected = slTxsCollected cur + max 0 (fromIntegral coll)
        } : rSLs
      }
  LogObject{loBody=LOTxsProcessed acc rej, loTid, loAt} ->
    a { aTxsCollectedAt = loTid `Map.delete` aTxsCollectedAt
      , aSlotStats      =
        cur
        { slTxsMemSpan =
          case loTid `Map.lookup` aTxsCollectedAt of
            Nothing ->
              -- error $ mconcat
              -- ["LOTxsProcessed missing LOTxsCollected for tid", show tid, " at ", show loAt]
              Just $
              1.0
              +
              fromMaybe 0 (slTxsMemSpan cur)
            Just base ->
              Just $
              (loAt `Time.diffUTCTime` base)
              +
              fromMaybe 0 (slTxsMemSpan cur)
        , slTxsAccepted = slTxsAccepted cur + acc
        , slTxsRejected = slTxsRejected cur + max 0 (fromIntegral rej)
        } : rSLs
      }
  _ -> a
 where
   updateOnNewSlot :: LogObject -> TimelineAccum -> TimelineAccum
   updateOnNewSlot LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo density} a' =
     extendTimelineAccum ci slot loAt 1 utxo density a'
   updateOnNewSlot _ _ =
     error "Internal invariant violated: updateSlot called for a non-LOTraceStartLeadershipCheck LogObject."

   onLeadershipCheck :: UTCTime -> SlotStats -> SlotStats
   onLeadershipCheck now sl@SlotStats{..} =
     sl { slCountChecks = slCountChecks + 1
        , slSpanCheck = max 0 $ now `sinceSlot` slStart
        }

   onLeadershipCertainty :: UTCTime -> Bool -> SlotStats -> SlotStats
   onLeadershipCertainty now lead sl@SlotStats{..} =
     sl { slCountLeads = slCountLeads + if lead then 1 else 0
        , slSpanLead  = max 0 $ now `Time.diffUTCTime` (slSpanCheck `Time.addUTCTime` unSlotStart slStart)
        }

   patchSlotCheckGap :: Word64 -> SlotNo -> TimelineAccum -> TimelineAccum
   patchSlotCheckGap 0 _ a' = a'
   patchSlotCheckGap n slot a'@TimelineAccum{aSlotStats=cur':_} =
     patchSlotCheckGap (n - 1) (slot + 1) $
     extendTimelineAccum ci slot (unSlotStart $ slotStart ci slot) 0 (slUtxoSize cur') (slDensity cur') a'
   patchSlotCheckGap _ _ _ =
     error "Internal invariant violated: patchSlotCheckGap called with empty TimelineAccum chain."
timelineStep _ a = const a

extendTimelineAccum ::
     ChainInfo
  -> SlotNo -> UTCTime -> Word64 -> Word64 -> Float
  -> TimelineAccum -> TimelineAccum
extendTimelineAccum ci@CInfo{..} slot time checks utxo density a@TimelineAccum{..} =
  let (epoch, epochSlot) = unSlotNo slot `divMod` epoch_length gsis in
    a { aSlotStats = SlotStats
        { slSlot        = slot
        , slEpoch       = epoch
        , slEpochSlot   = epochSlot
        , slStart       = slStart
        , slEarliest    = time
        , slOrderViol   = 0
          -- Updated as we see repeats:
        , slCountChecks = checks
        , slCountLeads  = 0
        , slSpanCheck   = max 0 $ time `sinceSlot` slStart
        , slSpanLead    = 0
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
                          <*> extractResAccums aResAccums
        } : aSlotStats
      }
    where maybeDiscard :: (Word64 -> Maybe Word64) -> Word64 -> Maybe Word64
          maybeDiscard f = f

          slStart = slotStart ci slot

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
