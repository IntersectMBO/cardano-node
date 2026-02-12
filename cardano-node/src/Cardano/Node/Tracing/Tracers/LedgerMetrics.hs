{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Node.Tracing.Tracers.LedgerMetrics
  ( LedgerMetrics (..)
  , traceLedgerMetrics
  , startLedgerMetricsTracer
  ) where

import           Cardano.Ledger.BaseTypes (SlotNo (..), StrictMaybe (..))
import           Cardano.Logging hiding (traceWith)
import           Cardano.Node.Queries (LedgerQueries (..), NodeKernelData (..), mapNodeKernelDataIO,
                   nkQueryChain, nkQueryLedger)
import           Cardano.Node.Tracing.Tracers.ChainDB (fragmentChainDensity)
import           Ouroboros.Consensus.BlockchainTime.API
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Abstract (IsLedger)
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Node (NodeKernel (getBlockchainTime, getChainDB))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB (getStatistics)
import qualified Ouroboros.Consensus.Storage.LedgerDB.Forker as LedgerDB (ledgerTableSize)
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad.Class.MonadAsync (link)
import           Control.Monad.STM (atomically, retry)
import           "contra-tracer" Control.Tracer (Tracer, traceWith)
import           Data.Aeson (Value (Number, String), toJSON, (.=))
import           Data.Text as Text
import           GHC.Conc (labelThread, myThreadId)


startLedgerMetricsTracer
  :: forall blk
   . IsLedger (LedgerState blk)
  => LedgerQueries blk
  => AF.HasHeader (Header blk)
  => AF.HasHeader blk
  => Tracer IO LedgerMetrics
  -> Int                         -- ^ Every Nth slot
  -> NodeKernelData blk
  -> IO ()
startLedgerMetricsTracer _ 0 _ = pure ()  -- Disabled if 0
startLedgerMetricsTracer tr everyNThSlot nodeKernelData =
  async ledgerMetricsThread >>= link
  where
    ledgerMetricsThread :: IO ()
    ledgerMetricsThread = do
      myThreadId >>= flip labelThread "Ledger Metrics"
      go 1 CurrentSlotUnknown
      where
        go :: Int -> CurrentSlot -> IO ()
        go !countdown !prevSlot = do
          !query <- waitForDifferentSlot prevSlot
          threadDelay $ 700 * 1000                      -- 700ms; see Note [Interval]
          case query of
            SJust current@(CurrentSlot slot')
              | countdown == 1  -> traceLedgerMetrics nodeKernelData slot' tr >> go everyNThSlot current
              | otherwise       -> go (countdown - 1) current
            _ -> go countdown prevSlot

        waitForDifferentSlot :: CurrentSlot -> IO (StrictMaybe CurrentSlot)
        waitForDifferentSlot prev =
          mapNodeKernelDataIO (\nk -> atomically $ do
            mSlot <- getCurrentSlot (getBlockchainTime nk)
            case mSlot of
              current@(CurrentSlot s') | isDifferentSlotNo s' -> return current
              _ -> retry
            ) nodeKernelData
          where
            isDifferentSlotNo = case prev of
              CurrentSlot s       -> (/= s)
              CurrentSlotUnknown  -> const True

data LedgerMetrics =
  LedgerMetrics {
        tsSlotNo       :: SlotNo
      , tsUtxoSize     :: Int
      , tsDelegMapSize :: Int
      , tsChainDensity :: Double
{- see Note [GovMetrics]
      , tsDRepCount    :: Int
      , tsDRepMapSize  :: Int
-}
    }

traceLedgerMetrics ::
  (  IsLedger (LedgerState blk)
  ,  LedgerQueries blk
  , AF.HasHeader blk
  ,  AF.HasHeader (Header blk))
  => NodeKernelData blk
  -> SlotNo
  -> Tracer IO LedgerMetrics
  -> IO ()
traceLedgerMetrics nodeKern slotNo tracer = do
  query <- mapNodeKernelDataIO
              (\nk ->
                (,,) -- (,,,,)
                  <$> ChainDB.getStatistics (getChainDB nk)
                  <*> nkQueryLedger (ledgerDelegMapSize . ledgerState) nk
                  <*> nkQueryChain fragmentChainDensity nk
{- see Note [GovMetrics]
                  <*> nkQueryLedger (ledgerDRepCount . ledgerState) nk
                  <*> nkQueryLedger (ledgerDRepMapSize . ledgerState) nk
-}
              )
              nodeKern
  case query of
    SNothing -> pure ()
    SJust (ledgerStatistics, delegMapSize, {- drepCount, drepMapSize, -} chainDensity) ->
        let msg = LedgerMetrics
                    slotNo
                    (LedgerDB.ledgerTableSize ledgerStatistics)
                    delegMapSize
{- see Note [GovMetrics]
                    drepCount
                    drepMapSize
-}
                    (fromRational chainDensity)
        in traceWith tracer msg

--------------------------------------------------------------------------------
-- LedgerMetrics
--------------------------------------------------------------------------------

instance LogFormatting LedgerMetrics where
  forMachine _dtal LedgerMetrics {..} =
        mconcat [ "kind" .= String "LedgerMetrics"
                , "slot" .= toJSON (unSlotNo tsSlotNo)
                , "utxoSize" .= Number (fromIntegral tsUtxoSize)
                , "delegMapSize" .= Number (fromIntegral tsDelegMapSize)
                , "chainDensity" .= Number (fromRational (toRational tsChainDensity))
{- see Note [GovMetrics]
                , "drepCount" .= Number (fromIntegral tsDRepCount)
                , "drepMapSize" .= Number (fromIntegral tsDRepMapSize)
-}
                ]
  forHuman LedgerMetrics {..} =
    "Ledger metrics "
      <> " utxoSize "     <> showT tsUtxoSize
      <> " delegMapSize " <> showT tsDelegMapSize
      <> " chainDensity " <> showT tsChainDensity
{- see Note [GovMetrics]
      <> " drepCount"     <> showT tsDRepCount
      <> " drepMapSize"   <> showT tsDRepMapSize
-}
  asMetrics LedgerMetrics {..} =
    [ IntM "utxoSize"     (fromIntegral tsUtxoSize)
    , IntM "delegMapSize" (fromIntegral tsDelegMapSize)
{- see Note [GovMetrics]
    , IntM "drepCount"    (fromIntegral tsDRepCount)
    , IntM "drepMapSize"  (fromIntegral tsDRepMapSize)
-}
    ]

instance MetaTrace LedgerMetrics where
  namespaceFor LedgerMetrics {} = Namespace [] ["LedgerMetrics"]
  severityFor (Namespace _ ["LedgerMetrics"]) _ = Just Info
  severityFor _ _ = Nothing

  metricsDocFor (Namespace _ ["LedgerMetrics"]) =
      [ ("utxoSize",      "UTxO set size")
      , ("delegMapSize",  "Delegation map size")
{- see Note [GovMetrics]
      , ("drepCount",     "Number of DReps")
      , ("drepMapSize",   "Number of DRep delegations")
-}
      ]
  metricsDocFor _ = []

  documentFor (Namespace _ ["LedgerMetrics"]) = Just $ Text.unlines
      [ "Periodic trace emitted every Nth slot, approximately 700 milliseconds after slot start."
      , "It queries the current ledger and chain to report metrics such as UTxO set and delegation map sizes."
{- see Note [GovMetrics]
      , "Additionally, it provides DRep-related metrics to help monitor governance dynamics."
-}
      ]
  documentFor _ = Nothing

  allNamespaces = [Namespace [] ["LedgerMetrics"]]


-- Note [GovMetrics]
-- ---------------------------
-- Governenance / DRep-related fields are currently excluded.
-- There hasn't been a decision as to their usefulness as metrics (vs. the existing route, CLI queries);
-- furthermore, their performance impact (if any) hasn't been assessed in benchmarks.
-- If ever needed, they can be trivially enabled by uncommenting all places referencing this note.


-- Note [Interval]
-- ---------------------------
-- NB. The hardcoded delay interval of 700ms is meant to represent "0.7 * slot duration". If running with other
-- durations than 1 second, this has to be accounted for. Especially when running with durations < 700ms, this
-- tracer will produce erroneous values and should be disabled by setting `TraceOptionLedgerMetricsFrequency = 0` in the config.
--
-- The interval has been picked as the best from different candidates, based on benchmarks that compared performance impact on block production and diffusion.
-- The previous state of affairs (which would correspond to a delay of 0ms) has shown to possess the least beneficial impact on performance.
