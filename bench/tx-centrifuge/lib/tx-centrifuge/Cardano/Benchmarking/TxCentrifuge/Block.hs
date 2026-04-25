{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------

-- | Translation layer between @cardano-api@ and @ouroboros-consensus@ types.
--
-- The benchmarking tool uses @cardano-api@ types internally (@Api.TxId@,
-- @Api.Tx@) because they are era-agnostic and simple to work with. The
-- Ouroboros mini-protocols (TxSubmission2, ChainSync, BlockFetch) speak
-- @ouroboros-consensus@ types (@GenTx@, @GenTxId@, @CardanoBlock@), which are
-- era-indexed sum types with one constructor per era. This module is the
-- single translation point between the two:
--
-- * Outbound (submitting): @Api.Tx@ → @GenTx@ via 'toGenTx'.
-- * Inbound (node replies): @GenTxId@ → @Api.TxId@ via 'fromGenTxId'.
-- * Inbound (chain-following): @CardanoBlock@ → @[Api.TxId]@ via 'extractTxIds',
--   which also reaches into @cardano-ledger@ to unwrap raw block bodies
--   (ideally @cardano-api@ would provide this, but it currently does not).
--
-- Centralising all boundary crossings here keeps the rest of the codebase
-- unaware of the consensus type machinery.
module Cardano.Benchmarking.TxCentrifuge.Block
  ( -- * Block type.
    CardanoBlock
    -- * Confirmed transactions.
  , ConfirmedTx (..)
    -- * Transaction extraction.
  , extractTxIds
  , extractFromShelleyBlock
    -- * Protocol boundary.
  , toGenTx
  , fromGenTxId
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Foldable (toList)
import Data.Functor.Const (Const (..))
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
--------------------
-- cardano-ledger --
--------------------
import Cardano.Ledger.Block qualified as LedgerBlock
import Cardano.Ledger.Core qualified as Core
---------------------------------
-- ouroboros-consensus:cardano --
---------------------------------
import Ouroboros.Consensus.Cardano.Block qualified as Cardano
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Mempool qualified as Mempool
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Block qualified as Block
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as SupportsMempool

--------------------------------------------------------------------------------
-- Block type.
--------------------------------------------------------------------------------

-- | The Cardano block type used throughout the benchmarking tool.
type CardanoBlock = Cardano.CardanoBlock Eras.StandardCrypto

--------------------------------------------------------------------------------
-- Confirmed transactions.
--------------------------------------------------------------------------------

-- | Information of a confirmed transaction, according to a configured
-- confirmation depth.
data ConfirmedTx = ConfirmedTx
  { -- | The confirmed transaction ID.
    confirmedTxId      :: !Api.TxId
    -- | Block number where this transaction was included.
  , confirmedTxBlockNo :: !Block.BlockNo
    -- | Slot number where this transaction was included.
  , confirmedTxSlotNo  :: !Block.SlotNo
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Transaction ID extraction.
--------------------------------------------------------------------------------

-- | Extract all transaction IDs from a Cardano block.
extractTxIds :: CardanoBlock -> [Api.TxId]
extractTxIds = \case
  -- Byron era: skip (different tx format and not relevant for benchmarking).
  Cardano.BlockByron _ -> []
  -- Shelley-based eras: extract TxIds.
  Cardano.BlockShelley  blk -> extractFromShelleyBlock blk
  Cardano.BlockAllegra  blk -> extractFromShelleyBlock blk
  Cardano.BlockMary     blk -> extractFromShelleyBlock blk
  Cardano.BlockAlonzo   blk -> extractFromShelleyBlock blk
  Cardano.BlockBabbage  blk -> extractFromShelleyBlock blk
  Cardano.BlockConway   blk -> extractFromShelleyBlock blk
  Cardano.BlockDijkstra blk -> extractFromShelleyBlock blk

-- | Extract transaction IDs from a Shelley-based block.
extractFromShelleyBlock
  :: Core.EraBlockBody ledgerEra
  => Shelley.ShelleyBlock proto ledgerEra
  -> [Api.TxId]
extractFromShelleyBlock shelleyBlock =
  case Shelley.shelleyBlockRaw shelleyBlock of
    LedgerBlock.Block _ body ->
      let txSeq = getConst (Core.txSeqBlockBodyL Const body)
      in map toTxId (toList txSeq)
  where
    toTxId tx = Api.fromShelleyTxId (Core.txIdTx tx)

--------------------------------------------------------------------------------
-- Protocol boundary.
--------------------------------------------------------------------------------

-- | Convert a cardano-api transaction to the consensus 'Mempool.GenTx' type.
-- This is the single point where we cross from cardano-api types to
-- ouroboros-consensus types.
toGenTx :: Api.Tx Api.ConwayEra -> SupportsMempool.GenTx CardanoBlock
toGenTx tx = Api.toConsensusGenTx $ Api.TxInMode Api.shelleyBasedEra tx

-- | Convert a consensus 'Mempool.GenTxId' to a cardano-api 'Api.TxId'.
--
-- All Shelley-based eras use the same 'Mempool.ShelleyTxId' wrapper, so a
-- single 'Api.fromShelleyTxId' covers every post-Byron era.
fromGenTxId :: SupportsMempool.GenTxId CardanoBlock -> Api.TxId
fromGenTxId (Cardano.GenTxIdByron _) =
  error "fromGenTxId: Byron transactions not supported"
fromGenTxId (Cardano.GenTxIdShelley  (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdAllegra  (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdMary     (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdAlonzo   (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdBabbage  (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdConway   (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdDijkstra (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i

