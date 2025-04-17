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
import           Data.Text as Text
import           GHC.Conc (labelThread, myThreadId, unsafeIOToSTM)

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
startLedgerMetricsTracer tr everyNThSlot nodeKernelData = do
    as <- async ledgerMetricsThread
    link as
  where
    ledgerMetricsThread :: IO ()
    ledgerMetricsThread = do
      myThreadId >>= flip labelThread "Peer Tracer"
      go 1 SNothing
      where
        go :: Int -> StrictMaybe SlotNo -> IO ()
        go !i !prevSlot = do
          !query <- waitForDifferentSlot prevSlot
          threadDelay $ 700 * 1000
          case query of
            SJust slot'
              | i `mod` everyNThSlot == 0 -> do
                  traceLedgerMetrics nodeKernelData slot' tr
                  go (i + 1) (SJust slot')
              | otherwise -> go (i + 1) (SJust slot')
            SNothing -> go i prevSlot

        waitForDifferentSlot :: StrictMaybe SlotNo -> IO (StrictMaybe SlotNo)
        waitForDifferentSlot prev = do
          mapNodeKernelDataIO (\nk -> atomically $ do
            mSlot <- getCurrentSlot (getBlockchainTime nk)
            case mSlot of
              CurrentSlot s' | SJust s' /= prev -> return s'
              _ -> do
                    unsafeIOToSTM ( threadDelay $ 1 * 1000)
                    retry
            ) nodeKernelData

data LedgerMetrics =
  LedgerMetrics {
        tsSlotNo       :: SlotNo
      , tsUtxoSize     :: Int
      , tsDelegMapSize :: Int
      , tsDRepCount    :: Int
      , tsDRepMapSize  :: Int
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
                (,,,,)
                  <$> nkQueryLedger (ledgerUtxoSize . ledgerState) nk
                  <*> nkQueryLedger (ledgerDelegMapSize . ledgerState) nk
                  <*> nkQueryLedger (ledgerDRepCount . ledgerState) nk
                  <*> nkQueryLedger (ledgerDRepMapSize . ledgerState) nk
                  <*> nkQueryChain fragmentChainDensity nk)
              nodeKern
  case query of
    SNothing -> pure ()
    SJust (utxoSize, delegMapSize, drepCount, drepMapSize, chainDensity) ->
        let msg = LedgerMetrics
                    slotNo
                    utxoSize
                    delegMapSize
                    drepCount
                    drepMapSize
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
                , "drepCount" .= Number (fromIntegral tsDRepCount)
                , "drepMapSize" .= Number (fromIntegral tsDRepMapSize)
                , "chainDensity" .= Number (fromRational (toRational tsChainDensity))
                ]
  forHuman LedgerMetrics {..} =
      "Ledger metrics "
      <> " utxoSize "     <> showT tsUtxoSize
      <> " delegMapSize " <> showT tsDelegMapSize
      <> " drepCount"     <> showT tsDRepCount
      <> " drepMapSize"   <> showT tsDRepMapSize
      <> " chainDensity " <> showT tsChainDensity
  asMetrics LedgerMetrics {..} =
    [ IntM "utxoSize"     (fromIntegral tsUtxoSize)
    , IntM "delegMapSize" (fromIntegral tsDelegMapSize)
    , IntM "drepCount"    (fromIntegral tsDRepCount)
    , IntM "drepMapSize"  (fromIntegral tsDRepMapSize)]


instance MetaTrace LedgerMetrics where
  namespaceFor LedgerMetrics {} = Namespace [] ["LedgerMetrics"]
  severityFor (Namespace _ ["LedgerMetrics"]) _ = Just Info
  severityFor _ _ = Nothing

  metricsDocFor (Namespace _ ["LedgerMetrics"]) =
      [ ("utxoSize",      "Size of the current UTxO set (number of entries)")
      , ("delegMapSize",  "Size of the delegation map (number of delegators)")
      , ("drepCount",     "Number of active DReps (Delegated Representatives)")
      , ("drepMapSize",   "Size of the DRep map (number of stake keys mapped to DReps)")
      ]
  metricsDocFor _ = []

  documentFor (Namespace _ ["LedgerMetrics"]) = Just $ Text.unlines
      [ "Periodic trace emitted every Nth slot, approximately 700 milliseconds after slot start."
      , "It queries the current ledger state to report metrics such as UTxO size, delegation map size,"
      , "and DRep participation. This trace helps monitor ledger growth and governance dynamics over time."
      ]


  documentFor _ = Nothing

  allNamespaces = [Namespace [] ["LedgerMetrics"]]






