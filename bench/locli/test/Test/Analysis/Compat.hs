{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Analysis results of older runs must stay readable, so they can be compared
--   with new ones.
module Test.Analysis.Compat where

import           Cardano.Prelude

import           Data.Aeson (eitherDecodeFileStrict, eitherDecode, encode)
import           Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap

import           Cardano.Analysis.API
import           Cardano.Analysis.BlockProp (summariseMultiBlockProp)
import           Cardano.Analysis.MachPerf (summariseMultiClusterPerf)
import           Cardano.Analysis.Summary (summariseMultiSummary)
import           Cardano.Util

import           Hedgehog


-- | Real cluster performance results of a locli 3.0 analysis.
clusterPerfFixture :: FilePath
clusterPerfFixture = "test/data/analysis/clusterperf.json"

-- | The keys 'MachPerf' gained in locli 3.0. Fixed history, not to be derived: dropping
--   them from a 3.0 result yields a result as written by locli 2.x. When adding a CDF
--   in a later version, add a list like this one for that version.
addedIn3_0 :: [Key]
addedIn3_0 =
  [ "cdfSnapTaken", "cdfSnapDuration", "cdfLiveMajGcDuring", "cdfLiveMajGcOutside"
  , "cdfTxsProcessed", "cdfTxsAccepted", "cdfTxsRejected", "cdfTxsRejectRatio", "cdfMempoolTxs"
  , "cdfCpuPerTx", "cdfMutPerTx", "cdfGcPerTx"
  , "cdfSpanLensCpuSnap", "cdfSpanLensCpuOther"
  ]

decodeClusterPerf :: KeyMap.KeyMap Value -> Either [Char] ClusterPerf
decodeClusterPerf = eitherDecode . encode . Object

withoutKeys :: [Key] -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
withoutKeys ks o = foldr KeyMap.delete o ks

prop_clusterPerf_2x_readable = withTests 1 $ property $ do
  o :: KeyMap.KeyMap Value <- evalEither =<< evalIO (eitherDecodeFileStrict clusterPerfFixture)
  filter (not . (`KeyMap.member` o)) addedIn3_0 === []

  full <- evalEither $ decodeClusterPerf o
  old  <- evalEither $ decodeClusterPerf (withoutKeys addedIn3_0 o)

  -- The new CDFs are empty, ...
  let sizes = [ KeyMap.lookup k (asObject old) >>= cdfSizeOf | k <- addedIn3_0 ]
  sizes === (Just (Number 0) <$ addedIn3_0)
  -- ... everything else is read as before.
  withoutKeys addedIn3_0 (asObject old) === withoutKeys addedIn3_0 (asObject full)
 where
   asObject :: ClusterPerf -> KeyMap.KeyMap Value
   asObject x = case toJSON x of
     Object o -> o
     _        -> KeyMap.empty

   cdfSizeOf :: Value -> Maybe Value
   cdfSizeOf = \case
     Object c -> KeyMap.lookup "cdfSize" c
     _        -> Nothing

-- | Combining runs (as for variance reports) also works when an older run lacks CDFs:
--   they come from the runs that have them, and are empty if none has them.
prop_multiClusterPerf_2x_combinable = withTests 1 $ property $ do
  o :: KeyMap.KeyMap Value <- evalEither =<< evalIO (eitherDecodeFileStrict clusterPerfFixture)
  full <- evalEither $ decodeClusterPerf o
  old  <- evalEither $ decodeClusterPerf (withoutKeys addedIn3_0 o)
  let combine = fmap unMultiClusterPerf . summariseMultiClusterPerf (nEquicentiles 7)
  mixed  <- evalEither $ combine [full, old]
  only2x <- evalEither $ combine [old, old]
  cdfAverageVal (cdfMempoolTxs mixed) === cdfAverageVal (cdfMempoolTxs full)
  cdfSize (cdfMempoolTxs only2x) === 0
  -- a metric empty in the 3.0 run too (no CPU peaks in the fixture run): still combines, empty
  cdfSize (cdfSpanLensCpuOther full)  === 0
  cdfSize (cdfSpanLensCpuOther mixed) === 0

-- | Real summary of a locli 3.0 analysis.
summaryFixture :: FilePath
summaryFixture = "test/data/analysis/summary.json"

-- | The keys 'Summary' gained in locli 3.0 (fixed history, as 'addedIn3_0').
summaryAddedIn3_0 :: [Key]
summaryAddedIn3_0 = [ "cdfUtxoFinal" ]

decodeSummary :: KeyMap.KeyMap Value -> Either [Char] SummaryOne
decodeSummary = eitherDecode . encode . Object

-- | A summary as written by locli 2.x is read like the cluster performance results:
--   the new CDFs empty, the rest unchanged; and the multi-run summary takes them from
--   the runs that have them.
prop_summary_2x_readable = withTests 1 $ property $ do
  o :: KeyMap.KeyMap Value <- evalEither =<< evalIO (eitherDecodeFileStrict summaryFixture)
  filter (not . (`KeyMap.member` o)) summaryAddedIn3_0 === []

  full <- evalEither $ decodeSummary o
  old  <- evalEither $ decodeSummary (withoutKeys summaryAddedIn3_0 o)
  cdfSize (cdfUtxoFinal old) === 0
  withoutKeys summaryAddedIn3_0 (asObject old) === withoutKeys summaryAddedIn3_0 (asObject full)

  mixed <- evalEither . first (show @_ @Text) $ summariseMultiSummary (sumAnalysisTime full) (nEquicentiles 7) [full, old]
  cdfAverageVal (cdfUtxoFinal mixed) === cdfAverageVal (cdfUtxoFinal full)
 where
   asObject :: SummaryOne -> KeyMap.KeyMap Value
   asObject x = case toJSON x of
     Object o -> o
     _        -> KeyMap.empty

-- | Real block propagation results of a locli 3.0 analysis.
blockPropFixture :: FilePath
blockPropFixture = "test/data/analysis/blockprop.json"

-- | The keys 'BlockProp' gained in locli 3.0 (fixed history, as 'addedIn3_0'); they replace
--   2.x's "cdfBlockBattle", which is ignored when read.
blockPropAddedIn3_0 :: [Key]
blockPropAddedIn3_0 = [ "cdfSlotBattle", "cdfHeightBattle" ]

decodeBlockProp :: KeyMap.KeyMap Value -> Either [Char] BlockPropOne
decodeBlockProp = eitherDecode . encode . Object

-- | Block propagation results as written by locli 2.x read like the cluster performance results,
--   and combine with 3.0 ones.
prop_blockProp_2x_readable = withTests 1 $ property $ do
  o :: KeyMap.KeyMap Value <- evalEither =<< evalIO (eitherDecodeFileStrict blockPropFixture)
  filter (not . (`KeyMap.member` o)) blockPropAddedIn3_0 === []
  battle <- maybe failure pure $ KeyMap.lookup "cdfSlotBattle" o
  let o2x = KeyMap.insert "cdfBlockBattle" battle (withoutKeys blockPropAddedIn3_0 o)

  full <- evalEither $ decodeBlockProp o
  old  <- evalEither $ decodeBlockProp o2x
  (cdfSize (cdfSlotBattle old), cdfSize (cdfHeightBattle old)) === (0, 0)
  withoutKeys blockPropAddedIn3_0 (asObject old) === withoutKeys blockPropAddedIn3_0 (asObject full)

  mixed <- evalEither . first (show @_ @Text) $ summariseMultiBlockProp (nEquicentiles 7) [full, old]
  cdfAverageVal (cdfSlotBattle mixed) === cdfAverageVal (cdfSlotBattle full)
 where
   asObject :: BlockPropOne -> KeyMap.KeyMap Value
   asObject x = case toJSON x of
     Object o -> o
     _        -> KeyMap.empty

-- | The forger's phases after forging may be missing (no downstream peers, or adopted only
--   after a chain switch): they encode as null and read back as missing, while a `chain.json`
--   written before (numbers) reads them as present.
prop_blockForge_optional_phases = withTests 1 $ property $ do
  let bf = BlockForge
        { bfForger = Host "node-0", bfSlotStart = SlotStart zeroUTCTime, bfBlockGap = 1, bfBlockSize = 100
        , bfStarted = 0.1, bfBlkCtx = SNothing, bfLgrState = SNothing, bfLgrView = SNothing, bfLeading = 0.2
        , bfTicked = SNothing, bfMemSnap = SNothing, bfForged = 0.3
        , bfAnnounced = SNothing, bfAnnouncedCum = SNothing, bfSending = SNothing, bfAdopted = SNothing
        , bfChainDelta = 1 }
      phases b = [bfAnnounced b, bfAnnouncedCum b, bfSending b, bfAdopted b]
  back <- evalEither (eitherDecode (encode bf))
  phases back === replicate 4 SNothing
  o <- case toJSON bf of
    Object o -> pure o
    _        -> failure
  older <- evalEither . eitherDecode . encode . Object $
    foldr (\k -> KeyMap.insert k (Number 0.01)) o ["bfAnnounced", "bfAnnouncedCum", "bfSending", "bfAdopted"]
  phases older === replicate 4 (SJust 0.01)

-- | Leniency is limited to the CDFs: other missing keys still fail.
prop_clusterPerf_requires_version = withTests 1 $ property $ do
  o :: KeyMap.KeyMap Value <- evalEither =<< evalIO (eitherDecodeFileStrict clusterPerfFixture)
  assert . isLeft $ decodeClusterPerf (KeyMap.delete "mpVersion" o)

tests :: IO Bool
tests =
  checkSequential $$discover
