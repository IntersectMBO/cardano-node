{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
module Cardano.Unlog.Timeline (module Cardano.Unlog.Timeline) where

import           Prelude (String, error)
import           Cardano.Prelude

import           Control.Arrow ((&&&))
import           Data.Vector (Vector)
import qualified Data.Map.Strict as Map

import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Time

import           Ouroboros.Network.Block (SlotNo(..))

import           Data.Accum
import           Cardano.Analysis.Profile
import           Cardano.Unlog.LogObject
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats


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
  LogObject{loBody=LOTxsCollected tid coll, loAt} ->
    a { aTxsCollectedAt =
        aTxsCollectedAt &
        (\case
            Just{} -> Just loAt
            --   error $ mconcat
            --   ["Duplicate LOTxsCollected for tid ", show tid, " at ", show loAt]
            Nothing -> Just loAt)
        `Map.alter` tid
      , aSlotStats      =
        cur
        { slTxsCollected = slTxsCollected cur + max 0 (fromIntegral coll)
        } : rSLs
      }
  LogObject{loBody=LOTxsProcessed tid acc rej, loAt} ->
    a { aTxsCollectedAt = tid `Map.delete` aTxsCollectedAt
      , aSlotStats      =
        cur
        { slTxsMemSpan =
          case tid `Map.lookup` aTxsCollectedAt of
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
        , slSpanCheck = max 0 $ now `Time.diffUTCTime` slStart
        }

   onLeadershipCertainty :: UTCTime -> Bool -> SlotStats -> SlotStats
   onLeadershipCertainty now lead sl@SlotStats{..} =
     sl { slCountLeads = slCountLeads + if lead then 1 else 0
        , slSpanLead  = max 0 $ now `Time.diffUTCTime` (slSpanCheck `Time.addUTCTime` slStart)
        }

   patchSlotCheckGap :: Word64 -> SlotNo -> TimelineAccum -> TimelineAccum
   patchSlotCheckGap 0 _ a' = a'
   patchSlotCheckGap n slot a'@TimelineAccum{aSlotStats=cur':_} =
     patchSlotCheckGap (n - 1) (slot + 1) $
     extendTimelineAccum ci slot (slotStart ci slot) 0 (slUtxoSize cur') (slDensity cur') a'
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
        , slStart       = slotStart ci slot
        , slEarliest    = time
        , slOrderViol   = 0
          -- Updated as we see repeats:
        , slCountChecks = checks
        , slCountLeads  = 0
        , slSpanCheck   = max 0 $ time `Time.diffUTCTime` slotStart ci slot
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
