{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Tracing.Tracers.LedgerQuery
  ( TraceLedgerQuery (..)
  , traceLedgerQuery
  ) where


import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import           Cardano.Logging
import           Cardano.Node.Queries (LedgerQueries (..), NodeKernelData (..), mapNodeKernelDataIO,
                   nkQueryChain, nkQueryLedger)
import           Cardano.Node.Tracing.Tracers.ChainDB (fragmentChainDensity)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Abstract (IsLedger)
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Data.Aeson (Value (Number, String), (.=))


data TraceLedgerQuery =
  TraceLedgerQuery {
     --   tsSlotNo       :: SlotNo
        tsUtxoSize     :: Int
      , tsDelegMapSize :: Int
      , tsChainDensity :: Double
    }

traceLedgerQuery ::
  (  IsLedger (LedgerState blk)
  ,  LedgerQueries blk
#if __GLASGOW_HASKELL__ >= 906
  , AF.HasHeader blk
#endif
  ,  AF.HasHeader (Header blk))
  => NodeKernelData blk
  -> Trace IO TraceLedgerQuery
  -> IO ()
traceLedgerQuery nodeKern tracer = do
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
        let msg = TraceLedgerQuery
                    utxoSize
                    delegMapSize
                    (fromRational chainDensity)
        in traceWith tracer msg

--------------------------------------------------------------------------------
-- TraceStartLeadershipCheck
--------------------------------------------------------------------------------

instance LogFormatting TraceLedgerQuery where
  forMachine _dtal TraceLedgerQuery {..} =
        mconcat [ "kind" .= String "TraceLedgerQuery"
                -- , "slot" .= toJSON (unSlotNo tsSlotNo)
                , "utxoSize" .= Number (fromIntegral tsUtxoSize)
                , "delegMapSize" .= Number (fromIntegral tsDelegMapSize)
                , "chainDensity" .= Number (fromRational (toRational tsChainDensity))
                ]
  forHuman TraceLedgerQuery {..} =
      "Querying ledger " -- <> showT (unSlotNo tsSlotNo)
      <> " utxoSize "     <> showT tsUtxoSize
      <> " delegMapSize " <> showT tsDelegMapSize
      <> " chainDensity " <> showT tsChainDensity
  asMetrics TraceLedgerQuery {..} =
    [IntM "utxoSize"     (fromIntegral tsUtxoSize),
     IntM "delegMapSize" (fromIntegral tsDelegMapSize)]





