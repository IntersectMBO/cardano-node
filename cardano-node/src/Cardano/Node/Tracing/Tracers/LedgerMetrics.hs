{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-redundant-constraints
                -Wno-error=partial-type-signatures
                -Wno-error=unused-imports
                -Wno-error=unused-top-binds
  #-}

module Cardano.Node.Tracing.Tracers.LedgerMetrics
  ( LedgerMetrics (..)
  , traceLedgerMetrics
  , startLedgerMetricsTracer
  ) where

import           Cardano.Ledger.BaseTypes (SlotNo (..))
import           Cardano.Logging hiding (traceWith)
import           Cardano.Node.Queries (LedgerQueries (..), NodeKernelData (..), mapNodeKernelDataIO,
                   nkQueryChain, nkQueryLedger)
import           Cardano.Node.Tracing.Tracers.ChainDB (fragmentChainDensity)
import           Ouroboros.Consensus.BlockchainTime.API (BlockchainTime (..), CurrentSlot (..), getCurrentSlot)
import           Ouroboros.Consensus.HardFork.Combinator (Header (..))
import           Ouroboros.Consensus.Ledger.Abstract (IsLedger, LedgerState)
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Node (NodeKernel (getBlockchainTime))
import           Ouroboros.Consensus.Util.IOLike (IOLike (..))
import           Ouroboros.Consensus.Util.STM (Watcher (..), withWatcher)
import qualified Ouroboros.Network.AnchoredFragment as AF (HasHeader (..))
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Control.Concurrent (threadDelay)
import           "io-classes" Control.Concurrent.Class.MonadSTM.TMVar (newEmptyTMVar, takeTMVar, writeTMVar)
import           "io-classes" Control.Monad.Class.MonadAsync (MonadAsync (..), link)
import           "io-classes" Control.Monad.Class.MonadSTM (MonadSTM (..), STM, atomically, retry)
import           Control.Monad (when)
import           "contra-tracer" Control.Tracer (Tracer, traceWith)
import           Data.Aeson (Value (Number, String), toJSON, (.=))
import           Data.IORef (readIORef)
import           Data.Kind (Type)
import           "cardano-strict-containers" Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import           Data.Text as Text (unlines)
import           GHC.Conc (labelThread, myThreadId)

(!++) :: forall (blk :: Type) (monad :: Type -> Type) (nodeKernel :: Type) . ()
  => nodeKernel ~ NodeKernel monad RemoteAddress LocalConnectionId blk
  => IOLike monad
  => MonadSTM monad
  => nodeKernel -> StrictMaybe SlotNo -> monad SlotNo
nodeKernel !++ slotNo = do
  tmVar <- atomically do newEmptyTMVar
  let wNotify :: SlotNo -> monad () = atomically . writeTMVar tmVar
  withWatcher threadLabel Watcher {..} $ atomically do takeTMVar tmVar
  where
    threadLabel :: String
    threadLabel = "startLedgerMetricsTracer awaiting new SlotNo thread"
    wFingerprint :: SlotNo -> SlotNo
    wFingerprint = id
    wInitial :: Maybe SlotNo
    wInitial = strictMaybeToMaybe slotNo
    wReader :: STM monad SlotNo
    wReader = getCurrentSlot (getBlockchainTime nodeKernel) >>= \case
      CurrentSlot newSlotNo | SJust newSlotNo /= slotNo -> pure newSlotNo
      _ -> retry

startLedgerMetricsTracer
  :: forall blk nodeKernel
   . IsLedger (LedgerState blk)
  => nodeKernel ~ NodeKernel IO RemoteAddress LocalConnectionId blk
  => LedgerQueries blk
  => AF.HasHeader (Header blk)
  => AF.HasHeader blk
  => Tracer IO LedgerMetrics
      -> Int                         -- ^ Every Nth slot
      -> NodeKernelData blk
      -> IO ()
startLedgerMetricsTracer _ 0 _ = pure ()  -- Disabled if 0
startLedgerMetricsTracer tr n nkd = work `withAsync` link where
  readNK :: IO nodeKernel
  readNK = readIORef (unNodeKernelData nkd) >>= \case
    SJust nodeKernel -> pure nodeKernel
    SNothing -> threadDelay 10_000 >> readNK
  work :: IO ()
  work = readNK >>= \nodeKernel -> do
    myThreadId >>= flip labelThread "Peer Tracer"
    let
      go :: Int -> StrictMaybe SlotNo -> IO ()
      infixl 3 `go`
      go !i !prevSlot = do
        !slot' <- nodeKernel !++ prevSlot
        threadDelay $ 700 * 1000
        when (i `mod` n == 0) do
          traceLedgerMetrics nkd slot' tr
        i + 1 `go` SJust slot'
    go 1 SNothing

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






