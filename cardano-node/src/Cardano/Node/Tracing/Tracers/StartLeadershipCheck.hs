{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Tracing.Tracers.StartLeadershipCheck
  ( TraceStartLeadershipCheckPlus (..)
  , ForgeTracerType
  , forgeTracerTransform
  ) where


import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import           Cardano.Logging
import           Cardano.Node.Queries (LedgerQueries (..), NodeKernelData (..))
import           Cardano.Slotting.Slot (fromWithOrigin)
import           Ouroboros.Consensus.Block (SlotNo (..), blockNo, BlockNo (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node (NodeKernel (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.API as LedgerDB

import           Control.Concurrent.STM (atomically)
import           Data.IORef (readIORef)
import           Data.Word (Word64)
import qualified Ouroboros.Network.AnchoredFragment as AF


type ForgeTracerType blk = Either (TraceForgeEvent blk)
                                  TraceStartLeadershipCheckPlus

data TraceStartLeadershipCheckPlus =
  TraceStartLeadershipCheckPlus {
        tsSlotNo       :: SlotNo
      , tsUtxoSize     :: Int
      , tsDelegMapSize :: Int
      , tsDRepCount    :: Int
      , tsDRepMapSize  :: Int
      , tsChainDensity :: Double
    }

forgeTracerTransform ::
  (  LedgerSupportsProtocol blk
  ,  LedgerQueries blk
  )
  => NodeKernelData blk
  -> Trace IO (ForgeTracerType blk)
  -> IO (Trace IO (ForgeTracerType blk))
forgeTracerTransform (NodeKernelData ref) (Trace tr) =
  let secondM f (x, y) = do -- avoiding new dep on extra pkg
        y' <- f y
        pure (x, y')
  in contramapM (Trace tr) $ secondM
      \case
          Right (Left slc@(TraceStartLeadershipCheck tsSlotNo)) -> do
            query <- readIORef ref >>= traverse
                        \NodeKernel{getChainDB} -> do
                               ledger <- fmap ledgerState . atomically $
                                           ChainDB.getCurrentLedger getChainDB
                               chain  <- atomically $ ChainDB.getCurrentChain getChainDB
                               utxoSize <- fmap (maybe 0 LedgerDB.ledgerTableSize) (ChainDB.getStatistics getChainDB)
                               pure TraceStartLeadershipCheckPlus {
                                   tsSlotNo
                                 , tsUtxoSize     = utxoSize
                                 , tsDelegMapSize = ledgerDelegMapSize   ledger
                                 , tsDRepCount    = ledgerDRepCount      ledger
                                 , tsDRepMapSize  = ledgerDRepMapSize    ledger
                                 , tsChainDensity = fragmentChainDensity chain }
            pure . Right $ case query of
              SNothing    -> Left slc
              SJust tslcp -> Right tslcp
          Right a ->
              pure $ Right a
          Left control ->
              pure $ Left control

fragmentChainDensity ::
#if __GLASGOW_HASKELL__ >= 906
  (AF.HasHeader blk, AF.HasHeader (Header blk))
#else
  AF.HasHeader (Header blk)
#endif
  => AF.AnchoredFragment (Header blk) -> Double
fragmentChainDensity frag = fromRational $ calcDensity blockD slotD
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
