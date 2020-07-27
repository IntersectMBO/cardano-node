{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Config.LedgerQueries
  ( LedgerQueries(..)
  )
where

import           Prelude                        ( Int
                                                , error
                                                , (.)
                                                )

import qualified Data.Map.Strict               as Map

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Unary

import qualified Cardano.Chain.Block           as Byron
import qualified Cardano.Chain.UTxO            as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Block
                                               as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger
                                               as Byron

import qualified Ouroboros.Consensus.Shelley.Ledger
                                               as Shelley
import qualified Shelley.Spec.Ledger.LedgerState
                                               as Shelley
import qualified Shelley.Spec.Ledger.UTxO      as Shelley

import qualified Ouroboros.Consensus.Cardano   as Cardano
import qualified Ouroboros.Consensus.Cardano.Block
                                               as Cardano

import qualified Ouroboros.Consensus.Mock.Ledger
                                               as Mock


class LedgerQueries blk where
  ledgerUtxoSize :: LedgerState blk -> Int

instance LedgerQueries Byron.ByronBlock where
  ledgerUtxoSize =
    Map.size . Byron.unUTxO . Byron.cvsUtxo . Byron.byronLedgerState

instance LedgerQueries (Shelley.ShelleyBlock c) where
  ledgerUtxoSize =
    (\(Shelley.UTxO xs) -> Map.size xs)
      . Shelley._utxo
      . Shelley._utxoState
      . Shelley.esLState
      . Shelley.nesEs
      . Shelley.shelleyState

instance (LedgerQueries x, NoHardForks x)
      => LedgerQueries (HardForkBlock '[x]) where
  ledgerUtxoSize = ledgerUtxoSize . project

instance LedgerQueries (Cardano.CardanoBlock c) where
  ledgerUtxoSize = \case
    Cardano.LedgerStateByron ledgerByron -> ledgerUtxoSize ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerUtxoSize ledgerShelley
    _ -> error "ledgerUtxoSize:  unhandled CardanoBlock case"

instance LedgerQueries (Mock.SimpleBlock a b) where
  ledgerUtxoSize _ = error "ledgerUtxoSize:  not implemented for SimpleBlock"
