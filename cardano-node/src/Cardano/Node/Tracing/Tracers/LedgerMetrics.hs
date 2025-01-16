{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
import           Ouroboros.Consensus.Node (NodeKernel (getBlockchainTime))
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad.Class.MonadAsync (link)
import           Control.Monad.STM (atomically, retry)
import           "contra-tracer" Control.Tracer (Tracer, traceWith)
import           Data.Aeson (Value (Number, String), toJSON, (.=))

startLedgerMetricsTracer
  :: forall blk
   . IsLedger (LedgerState blk)
  => LedgerQueries blk
  => AF.HasHeader (Header blk)
  => AF.HasHeader blk
  => Tracer IO LedgerMetrics
  -> Int
  -> NodeKernelData blk
  -> IO ()
startLedgerMetricsTracer tr _delayMilliseconds nodeKernelData = do
    as <- async ledgerMetricsThread
    link as
  where
    ledgerMetricsThread :: IO ()
    ledgerMetricsThread = go (-1) where
      go slot = do
        !query <- waitForDifferentSlot slot
        threadDelay $ 700 * 1000
        case query of
          SJust slot' -> traceLedgerMetrics nodeKernelData slot' tr >> go slot'
          SNothing    -> go slot

    waitForDifferentSlot :: SlotNo -> IO (StrictMaybe SlotNo)
    waitForDifferentSlot s = do
      mapNodeKernelDataIO (\nk -> atomically $ do
          mSlot <- getCurrentSlot (getBlockchainTime nk)
          case mSlot of
            CurrentSlot s' | s' /= s  -> return s'
            _                         -> retry
        ) nodeKernelData

data LedgerMetrics =
  LedgerMetrics {
        tsSlotNo       :: SlotNo
      , tsUtxoSize     :: Int
      , tsDelegMapSize :: Int
      , tsChainDensity :: Double
    }

traceLedgerMetrics ::
  (  IsLedger (LedgerState blk)
  ,  LedgerQueries blk
#if __GLASGOW_HASKELL__ >= 906
  , AF.HasHeader blk
#endif
  ,  AF.HasHeader (Header blk))
  => NodeKernelData blk
  -> SlotNo
  -> Tracer IO LedgerMetrics
  -> IO ()
traceLedgerMetrics nodeKern slotNo tracer = do
  query <- mapNodeKernelDataIO
              (\nk ->
                (,,)
                  <$> nkQueryLedger (ledgerUtxoSize . ledgerState) nk
                  <*> nkQueryLedger (ledgerDelegMapSize . ledgerState) nk
                  <*> nkQueryChain fragmentChainDensity nk)
              nodeKern
  case query of
    SNothing -> pure ()
    SJust (utxoSize, delegMapSize, chainDensity) ->
        let msg = LedgerMetrics
                    slotNo
                    utxoSize
                    delegMapSize
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
                ]
  forHuman LedgerMetrics {..} =
      "Ledger metrics "
      <> " utxoSize "     <> showT tsUtxoSize
      <> " delegMapSize " <> showT tsDelegMapSize
      <> " chainDensity " <> showT tsChainDensity
  asMetrics LedgerMetrics {..} =
    [IntM "utxoSize"     (fromIntegral tsUtxoSize),
     IntM "delegMapSize" (fromIntegral tsDelegMapSize)]


instance MetaTrace LedgerMetrics where
  namespaceFor LedgerMetrics {} = Namespace [] ["LedgerMetrics"]
  severityFor (Namespace _ ["LedgerMetrics"]) _ = Just Info
  severityFor _ _ = Nothing

  metricsDocFor (Namespace _ ["LedgerMetrics"]) =
      [ ("utxoSize", "UTxO set size")
      , ("delegMapSize", "Delegation map size")
      ]
  metricsDocFor _ = []

  documentFor (Namespace _ ["LedgerMetrics"]) = Just $ mconcat
    [ "" -- TODO YUP
    ]

  documentFor _ = Nothing

  allNamespaces = [Namespace [] ["LedgerMetrics"]]






