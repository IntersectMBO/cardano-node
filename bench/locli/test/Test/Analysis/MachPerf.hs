{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | The per-host metrics 'slotStatsMachPerf' derives from a host's slots: ledger snapshots
--   (incl. the split of major GCs by snapshot write windows), and transaction processing.
module Test.Analysis.MachPerf where

import           Cardano.Prelude

import           Data.Time.Calendar (fromGregorian)

import           Cardano.Analysis.API
import           Cardano.Analysis.MachPerf (slotStatsMachPerf, summariseClusterPerf)
import           Cardano.Unlog.Resources (Resources (..), extractResAccums, mkResAccums, updateResAccums)
import           Cardano.Util

import           Hedgehog


t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

-- | 1s slots from t0; slotStatsMachPerf reads only the genesis ('Run' is strict, hence the rest).
run :: Run
run = Run
  { genesisSpec      = GenesisSpec { delegators = 0, utxo = 0, dreps = 0 }
  , generatorProfile = GeneratorProfile
    { add_tx_size = 0, inputs_per_tx = 0, outputs_per_tx = 0, tps = 0, tx_count = 0
    , plutusMode = Nothing, plutusAutoMode = Nothing, plutus = Nothing }
  , metadata         = Metadata
    { tag = "", batch = "", ident = "", node_ghc_version = "", profile = "", era = ""
    , manifest = Manifest mempty, profile_content = mempty }
  , genesis          = Genesis
    { activeSlotsCoeff  = 0.05
    , protocolParams    = PParams { maxTxSize = 16384, maxBlockBodySize = 90112 }
    , networkMagic      = 42
    , epochLength       = 1000
    , systemStart       = t0
    , slotsPerKESPeriod = 129600
    , slotLength        = 1
    , maxKESEvolutions  = 62
    , securityParam     = 2160
    }
  }

-- | A slot at 50% process CPU (GC 10%, mutator 40%), without a major GC.
slot :: Word64 -> SlotStats NominalDiffTime
slot n = SlotStats
  { slSlot          = SlotNo n
  , slEpoch         = EpochNo 0
  , slEpochSlot     = EpochSlot n
  , slEpochSafeInt  = EpochSafeInt 0
  , slStart         = SlotStart (fromIntegral n `addUTCTime` t0)
  , slCountStarts   = 1
  , slCountBlkCtx   = 0
  , slCountLgrState = 0
  , slCountLgrView  = 0
  , slCountLeads    = 0
  , slCountForges   = 0
  , slChainDBSnap   = 0
  , slSnapshot      = SNothing
  , slRejectedTx    = 0
  , slBlockNo       = 0
  , slBlockGap      = 0
  , slStarted       = SNothing
  , slBlkCtx        = SNothing
  , slLgrState      = SNothing
  , slLgrView       = SNothing
  , slTicked        = SNothing
  , slMemSnap       = SNothing
  , slLeading       = SNothing
  , slForged        = SNothing
  , slMempoolTxs    = 0
  , slSpanTxsMem    = SNothing
  , slTxsCollected  = 0
  , slTxsAccepted   = 0
  , slTxsRejected   = 0
  , slUtxoSize      = 0
  , slDensity       = 0
  , slResources     = SJust $ (pure 0 :: Resources Word64) { rCentiCpu = 50, rCentiGC = 10, rCentiMut = 40 }
  , slLogObjects    = []
  }

withMajorGc :: Word64 -> SlotStats NominalDiffTime -> SlotStats NominalDiffTime
withMajorGc live sl = sl { slResources = slResources sl <&> \r -> r { rGcsMajor = 1, rLive = live } }

-- | Slots 0..14. A snapshot completes at 5.5s, taking 2s: its write window is 3.5s .. 10.5s
--   (until 5s after completion). Major GCs: slot 1 (before), 4 (writing), 9 (after completion,
--   within 5s), 12 (after the window). Slots 3 and 11 lack a resource sample. 15 txs processed, 12 accepted.
slots :: [SlotStats NominalDiffTime]
slots = [0 .. 14] <&> \n -> case n of
  1  -> withMajorGc 100 (slot n)
  2  -> (slot n) { slTxsCollected = 10, slTxsAccepted = 7 }
  3  -> (slot n) { slResources = SNothing }
  4  -> withMajorGc 200 (slot n)
  5  -> (slot n) { slChainDBSnap = 1, slSnapshot = SJust (5.5 `addUTCTime` t0, 2) }
  6  -> (slot n) { slTxsCollected = 5, slTxsAccepted = 5 }
  9  -> withMajorGc 300 (slot n)
  11 -> (slot n) { slResources = SNothing }
  12 -> withMajorGc 500 (slot n)
  _  -> slot n

-- | Size and average of a per-host CDF.
sizeAvg :: CDF I a -> (Int, Double)
sizeAvg c = (cdfSize c, unI (cdfAverage c))

prop_machPerf_snapshots_and_txs = withTests 1 $ property $ do
  mp <- evalEither $ snd <$> slotStatsMachPerf run (LogObjectSourceOther "test", slots)
  sizeAvg (cdfHostSlots mp)        === (1, 15)  -- slots analysed, not last - first
  -- Ledger snapshots
  sizeAvg (cdfSnapTaken mp)        === (1, 1)
  sizeAvg (cdfSnapDuration mp)     === (1, 2)
  sizeAvg (cdfLiveMajGcDuring mp)  === (2, 250)   -- slots 4 and 9
  sizeAvg (cdfLiveMajGcOutside mp) === (2, 300)   -- slots 1 and 12
  -- Transaction processing: run totals
  sizeAvg (cdfTxsProcessed mp)     === (1, 15)
  sizeAvg (cdfTxsAccepted mp)      === (1, 12)
  sizeAvg (cdfTxsRejected mp)      === (1, 3)
  sizeAvg (cdfTxsRejectRatio mp)   === (1, 0.2)
  -- CPU over the analysed 15s, per processed tx: 50% (the mean of the 13 samples) of 15s = 7.5s, over 15 txs
  sizeAvg (cdfCpuPerTx mp)         === (1, 500)
  sizeAvg (cdfGcPerTx mp)          === (1, 100)
  sizeAvg (cdfMutPerTx mp)         === (1, 400)

-- | Slots 990..1040 (epoch length 1000), slot 1030 filtered out. The epoch's first block is
--   adopted in slot 1004. Peaks at 95% CPU: 993-994 (other); 999-1002 (ends 2 slots before the
--   first block: the epoch transition, 3 slots of it in the new epoch); 1019-1022 (during a
--   snapshot completing at 1020.5 after 2s: window 1018.5 .. 1025.5); 1029 and 1031 (split by
--   the filtered slot: two peaks); 1035 (other).
peakSlots :: [SlotStats NominalDiffTime]
peakSlots =
  [ (slot n) { slEpoch     = EpochNo (n `div` 1000)
             , slEpochSlot = EpochSlot (n `mod` 1000)
             , slBlockNo   = if n < 1004 then 5 else 6
             , slSnapshot  = if n == 1020 then SJust (1020.5 `addUTCTime` t0, 2) else SNothing
             , slResources = slResources (slot n) <&> \r -> r { rCentiCpu = if peak n then 95 else 50 } }
  | n <- [990 .. 1040], n /= 1030 ]
 where
   peak n = n `elem` ([993, 994] <> [999 .. 1002] <> [1019 .. 1022] <> [1029, 1031, 1035])

prop_machPerf_cpu_peaks_by_cause = withTests 1 $ property $ do
  mp <- evalEither $ snd <$> slotStatsMachPerf run (LogObjectSourceOther "test", peakSlots)
  let sizeSum c = (cdfSize c, round (unI (cdfAverage c) * fromIntegral (cdfSize c)) :: Int)
  sizeSum (cdfSpanLensCpu      mp) === (6, 13)  -- 2 + 4 + 4 + 1 + 1 + 1 slots
  sizeSum (cdfSpanLensCpuEpoch mp) === (1, 3)   -- from the epoch's first slot
  sizeSum (cdfSpanLensCpuSnap  mp) === (1, 4)
  sizeSum (cdfSpanLensCpuOther mp) === (4, 5)   -- 2 + 1 + 1 + 1

-- | Without processed txs, the ratios read 0 (not NaN); more accepted than processed
--   (counted from different events) yields 0 rejected, not a Word64 underflow.
prop_machPerf_tx_edge_cases = withTests 1 $ property $ do
  let host xs = evalEither $ snd <$> slotStatsMachPerf run (LogObjectSourceOther "test", xs)
  idle <- host $ slot <$> [0 .. 14]
  sizeAvg (cdfTxsProcessed idle)   === (1, 0)
  sizeAvg (cdfTxsRejectRatio idle) === (1, 0)
  sizeAvg (cdfCpuPerTx idle)       === (1, 0)
  over <- host $ [0 .. 14] <&> \n -> if n == 2 then (slot n) { slTxsCollected = 3, slTxsAccepted = 5 } else slot n
  sizeAvg (cdfTxsRejected over)    === (1, 0)
  sizeAvg (cdfTxsRejectRatio over) === (1, 0)

-- | Combining hosts: a host without samples for a metric (here: no major GC outside snapshot
--   writes) doesn't pull the cluster average towards 0 ('nonEmptyCDFs').
prop_clusterPerf_skips_empty_hosts = withTests 1 $ property $ do
  let host xs = evalEither $ snd <$> slotStatsMachPerf run (LogObjectSourceOther "test", xs)
      noGcOutside = slots <&> \sl -> if slSlot sl `elem` [1, 12] then slot (unSlotNo $ slSlot sl) else sl
  a  <- host slots
  b  <- host noGcOutside
  cp <- evalEither $ summariseClusterPerf stdCentiles [a, b]
  let sizeAvg2 c = (cdfSize c, cdfAverageVal c)
  sizeAvg2 (cdfLiveMajGcOutside cp) === (2, 300)   -- host a only
  sizeAvg2 (cdfLiveMajGcDuring cp)  === (4, 250)   -- both hosts
  sizeAvg2 (cdfTxsProcessed cp)     === (2, 15)

-- | Resource samples: the first one only sets the baseline. A counter's first value is its
--   total since process start (e.g. a whole node startup), not one sampling interval's
--   increase: counters and CPU shares read 0, gauges their value; later samples, differences.
prop_resources_first_sample_is_baseline = withTests 1 $ property $ do
  let sample cpu gcs alloc rss = (pure 0 :: Resources Word64)
        { rCentiCpu = cpu, rGcsMinor = gcs, rAlloc = alloc, rRSS = rss }
      mib = 1048576
      sample1 = updateResAccums t0                  (sample 50000 6000 (200000 * mib) (7000 * mib)) mkResAccums
      sample2 = updateResAccums (1 `addUTCTime` t0) (sample 50050 6004 (200100 * mib) (7010 * mib)) sample1
      view r = (rCentiCpu r, rGcsMinor r, rAlloc r, rRSS r)
  view (extractResAccums sample1) === (0,  0, 0,   7000)
  view (extractResAccums sample2) === (50, 4, 100, 7010)

tests :: IO Bool
tests =
  checkSequential $$discover
