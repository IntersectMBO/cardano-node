{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing.Tracers.StartLeadershipCheck
  ( TraceStartLeadershipCheckPlus (..)
  , ForgeTracerType
  , forgeTracerTransform
  ) where


import           Cardano.Logging
import qualified "trace-dispatcher" Control.Tracer as T

import           Control.Concurrent.STM (atomically)
import           Data.IORef (readIORef)
import           Data.Word (Word64)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), blockNo, unBlockNo)
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Ouroboros.Consensus.Block (SlotNo (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Abstract (IsLedger, EmptyMK)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import           Ouroboros.Consensus.Node (NodeKernel (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

import           Cardano.Node.Queries (LedgerQueries (..), NodeKernelData (..))
import           Cardano.Slotting.Slot (fromWithOrigin)

import           Cardano.Ledger.BaseTypes (StrictMaybe (..))


type ForgeTracerType blk = Either (TraceForgeEvent blk)
                                  TraceStartLeadershipCheckPlus

data TraceStartLeadershipCheckPlus =
  TraceStartLeadershipCheckPlus {
        tsSlotNo       :: SlotNo
      , tsUtxoSize     :: Int
      , tsDelegMapSize :: Int
      , tsChainDensity :: Double
    }

forgeTracerTransform ::
  (  IsLedger (LedgerState blk)
  ,  LedgerQueries blk
  ,  AF.HasHeader (Header blk))
  => NodeKernelData blk
  -> Trace IO (ForgeTracerType blk)
  -> IO (Trace IO (ForgeTracerType blk))
forgeTracerTransform nodeKern (Trace tr) = pure $ Trace $ T.arrow $ T.emit $
    \case
      (lc, Right (Left slc@(TraceStartLeadershipCheck slotNo))) -> do
        query <- mapNodeKernelDataIO
                    (\nk ->
                       (,,)
                         <$> nkQueryLedger (ledgerUtxoSize . ledgerState) nk
                         <*> nkQueryLedger (ledgerDelegMapSize . ledgerState) nk
                         <*> nkQueryChain fragmentChainDensity nk)
                    nodeKern
        case query of
          SNothing -> T.traceWith tr (lc, Right (Left slc))
          SJust (utxoSize, delegMapSize, chainDensity) ->
                let msg = TraceStartLeadershipCheckPlus
                            slotNo
                            utxoSize
                            delegMapSize
                            (fromRational chainDensity)
                in T.traceWith tr (lc, Right (Right msg))
      (lc, Right a) ->
          T.traceWith tr (lc, Right a)
      (lc, Left control) ->
          T.traceWith tr (lc, Left control)

nkQueryLedger ::
     IsLedger (LedgerState blk)
  => (ExtLedgerState blk EmptyMK -> a)
  -> NodeKernel IO RemoteAddress LocalConnectionId blk
  -> IO a
nkQueryLedger f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentLedger getChainDB)

fragmentChainDensity ::
  AF.HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk) -> Rational
fragmentChainDensity frag = calcDensity blockD slotD
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b

nkQueryChain ::
     (AF.AnchoredFragment (Header blk) -> a)
  -> NodeKernel IO RemoteAddress LocalConnectionId blk
  -> IO a
nkQueryChain f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentChain getChainDB)


mapNodeKernelDataIO ::
  (NodeKernel IO RemoteAddress LocalConnectionId blk -> IO a)
  -> NodeKernelData blk
  -> IO (StrictMaybe a)
mapNodeKernelDataIO f (NodeKernelData ref) =
  readIORef ref >>= traverse f
