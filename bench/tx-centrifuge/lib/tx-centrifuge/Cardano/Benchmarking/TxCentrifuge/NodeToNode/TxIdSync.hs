{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------

-- | Transaction confirmation tracking via ChainSync and BlockFetch.
--
-- This module provides clients for the ChainSync and BlockFetch mini-protocols
-- that work together to:
-- 1. Follow the chain tip via ChainSync (receives headers, detects rollbacks).
-- 2. Fetch block bodies via BlockFetch.
-- 3. Extract transactions from blocks.
-- 4. Broadcast when transactions reach the configured confirmation depth.
--
-- == Header Queue (@stateHeaders@)
--
-- The header queue is written to by ChainSync and read by BlockFetch.
-- Both protocols can remove headers from it:
-- * __ChainSync__ filters out headers past the rollback point on
--   @MsgRollBackward@.
-- * __BlockFetch__ claims the head on @MsgBlock@ (block received) or filters
--   it out on @MsgNoBlocks@ (block no longer available on the node).
--
-- == Confirmation Depth
--
-- The @confirmationDepth@ parameter controls when transactions are considered
-- confirmed:
-- * @0@: Immediate, broadcast as soon as a block is seen (no reorg
--   protection).
-- * @N@: Wait for N additional blocks on top before broadcasting.
--
-- == Rollback Handling
--
-- When ChainSync announces a rollback:
-- * All blocks after the rollback point are discarded from the pending queue.
-- * Transactions in discarded blocks are never broadcast.
-- * Transactions already broadcast (buried deeper than rollback) are unaffected.
--
-- == Usage
--
-- Create the sync state, pass the clients to a NodeToNode connection, and
-- subscribe to the broadcast channel to receive confirmed transactions:
--
-- @
-- -- 1. Create shared state with a confirmation depth of 6 blocks.
-- state <- emptyState Config { confirmationDepth = 6 }
--
-- -- 2. Subscribe to the broadcast channel (before connecting, so no
-- --    confirmations are missed).
-- sub <- atomically $ dupTChan (stateBroadcast state)
--
-- -- 3. Connect to a node, running both clients over the same mux.
-- N2N.connect ioManager codecConfig networkMagic tracers addrInfo
--   N2N.emptyClients
--     { N2N.clientChainSync  = Just $ chainSyncClient state
--     , N2N.clientBlockFetch = Just $ blockFetchClient state
--     }
--
-- -- 4. Read confirmed transactions as they arrive.
-- confirmed <- atomically $ readTChan sub
-- putStrLn $ "Confirmed: " ++ show (confirmedTxId confirmed)
-- @
module Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync
  ( -- * Configuration
    Config (..)
    -- * State
  , State, emptyState
    -- * Subscription.
  , ConfirmedTx (..)
  , stateBroadcast
    -- * Client types
  , BlockFetchClient
  , ChainSyncClient
    -- * Protocol Clients
  , chainSyncClient
  , blockFetchClient
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Functor.Const (Const (..))
import Numeric.Natural (Natural)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
--------------------
-- cardano-ledger --
--------------------
import Cardano.Ledger.Block qualified as LedgerBlock
import Cardano.Ledger.Core qualified as Core
----------------
-- containers --
----------------
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
-------------------------
-- ouroboros-consensus --
-------------------------
import Ouroboros.Consensus.Block qualified as Block
import Ouroboros.Consensus.Cardano.Block qualified as Cardano
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
-----------------------
-- ouroboros-network --
-----------------------
import Ouroboros.Network.Block qualified as Net
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BF
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BFType
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM

--------------------------------------------------------------------------------
-- Types.
--------------------------------------------------------------------------------

type CardanoBlock = Cardano.CardanoBlock Eras.StandardCrypto

-- | BlockFetch client for retrieving full block bodies.
type BlockFetchClient =
  BF.BlockFetchClient
    CardanoBlock
    (Net.Point CardanoBlock)
    IO
    ()

-- | ChainSync client for following the chain tip (receives headers).
type ChainSyncClient =
  CS.ChainSyncClient
    (Block.Header CardanoBlock)
    (Net.Point CardanoBlock)
    (Net.Tip CardanoBlock)
    IO
    ()

--------------------------------------------------------------------------------
-- Internal State.
--------------------------------------------------------------------------------

-- | Configuration for transaction synchronization.
data Config = Config
  { -- | Number of blocks to wait before confirming.
    -- 0 = immediate (no reorg protection), N = wait for N blocks on top.
    confirmationDepth :: !Natural
  }

-- | Information of a confirmed transaction, according to the 'Config'.
data ConfirmedTx = ConfirmedTx
  { -- | The confirmed transaction ID.
    confirmedTxId      :: !Api.TxId
    -- | Block number where this transaction was included.
  , confirmedTxBlockNo :: !Block.BlockNo
    -- | Slot number where this transaction was included.
  , confirmedTxSlotNo  :: !Block.SlotNo
  }
  deriving (Show, Eq)

-- | Shared state between ChainSync and BlockFetch clients.
data State = State
  { -- | Configuration.
    stateConfig        :: !Config
    -- | Current chain tip as reported by the node.
  , stateCurrentTip    :: !(STM.TVar (Net.Tip CardanoBlock))
    -- | Headers received from ChainSync, waiting for BlockFetch.
    -- Removed by both ChainSync (rollback filter) and BlockFetch (claim on
    -- block received, filter on block unavailable).
    -- Uses @TVar (Seq …)@ instead of @TBQueue@ because both removal patterns
    -- need filtered access, which TBQueue does not support.
  , stateHeaders       :: !(STM.TVar (Seq (Block.Header CardanoBlock)))
    -- | Blocks fetched but not yet confirmed (ordered by block number).
  , statePendingBlocks :: !(STM.TVar (Seq CardanoBlock))
    -- | Broadcast channel for confirmed transactions.
    -- Write-only end; subscribers obtain a read-end via 'subscribe'.
  , stateBroadcast     :: !(STM.TChan ConfirmedTx)
  }

-- | Create initial sync state.
emptyState :: Config -> IO State
emptyState config = do
  currentTip    <- STM.newTVarIO Net.TipGenesis
  headerQueue   <- STM.newTVarIO Seq.empty
  pendingBlocks <- STM.newTVarIO Seq.empty
  broadcast     <- STM.newBroadcastTChanIO
  pure State
    { stateConfig        = config
    , stateCurrentTip    = currentTip
    , stateHeaders       = headerQueue
    , statePendingBlocks = pendingBlocks
    , stateBroadcast     = broadcast
    }

--------------------------------------------------------------------------------
-- ChainSync Client.
--------------------------------------------------------------------------------

-- | ChainSync client that follows the chain and adds headers to a queue.
--
-- On @MsgRollForward@:  queues the header for BlockFetch.
-- On @MsgRollBackward@: discards pending blocks after the rollback point.
chainSyncClient :: State -> ChainSyncClient
chainSyncClient state = CS.ChainSyncClient $ pure clientStIdle
  where
    -- Request the next update from the server.
    clientStIdle = CS.SendMsgRequestNext
      (pure ())    -- Action when server says "await".
      clientStNext -- Handler for the roll-forward / roll-backward response.
    -- Handle the server's roll-forward or roll-backward response.
    clientStNext = CS.ClientStNext
      { -- Advance the tip and queue the new header for BlockFetch to use.
        CS.recvMsgRollForward = \header tip -> CS.ChainSyncClient $ do
          STM.atomically $ do
            ---------- STM START ----------
            STM.writeTVar   (stateCurrentTip state) tip
            STM.modifyTVar' (stateHeaders    state)
              -- Append the new header at the end!
              (\q -> q <> Seq.singleton header)
            ---------- STM ENDED ----------
          -- Continue following the chain.
          pure clientStIdle
      , -- Discard pending blocks and queued headers past the rollback point.
        CS.recvMsgRollBackward = \rollbackPoint tip -> CS.ChainSyncClient $ do
          let keepBlock  block  = case rollbackPoint of
                Net.BlockPoint newSlot _ -> Block.blockSlot block  <= newSlot
                Net.GenesisPoint         -> False
              keepHeader header = case rollbackPoint of
                Net.BlockPoint newSlot _ -> Block.blockSlot header <= newSlot
                Net.GenesisPoint         -> False
          STM.atomically $ do
            ---------- STM START ----------
            STM.writeTVar   (stateCurrentTip    state) tip
            STM.modifyTVar' (stateHeaders       state) (Seq.filter keepHeader)
            STM.modifyTVar' (statePendingBlocks state) (Seq.filter keepBlock)
            ---------- STM ENDED ----------
          pure clientStIdle
      }

--------------------------------------------------------------------------------
-- BlockFetch Client.
--------------------------------------------------------------------------------

-- | BlockFetch client that fetches blocks and processes transactions.
--
-- Continuously peeks at headers from the queue (without consuming them),
-- fetches their blocks, and processes transactions. Headers stay in
-- @stateHeaders@ until the block body arrives or the node reports the block is
-- unavailable, so that concurrent rollbacks (via ChainSync) can still filter
-- them out. This closes the in-flight gap that would otherwise allow a
-- rolled-back block to be silently inserted into @statePendingBlocks@.
blockFetchClient :: State -> BlockFetchClient
blockFetchClient state = BF.BlockFetchClient $ do
    -- Peek at the next header without consuming it.
    -- The header stays in stateHeaders so rollbacks can still filter it out.
    header <- STM.atomically $ do
      ---------- STM START ----------
      headersSeq <- STM.readTVar (stateHeaders state)
      if Seq.null headersSeq
        then STM.retry
        else pure (Seq.index headersSeq 0)
        ---------- STM ENDED ----------
    -- We ask for only one block, using a [point..point] range.
    let !point = Net.BlockPoint
                   (Block.blockSlot header)
                   (Block.blockHash header)
    -- The actual request.
    pure $ BF.SendMsgRequestRange
      (BFType.ChainRange point point)
      (BF.BlockFetchResponse
        { -- MsgStartBatch: the node has the block and will send it next via
          -- MsgBlock, followed by MsgBatchDone.
          BF.handleStartBatch = pure BF.BlockFetchReceiver
            -- MsgStartBatch → MsgBlock → MsgBatchDone.
            { BF.handleBlock = \block -> do
                STM.atomically $ do
                  ---------- STM START ----------
                  -- False if a ChainSync rollback already removed this header.
                  notRolledBack <- claimHeader state point
                  if notRolledBack
                    then processBlock state block
                    else pure ()
                  ---------- STM ENDED ----------
                -- Single-block range: no further blocks expected.
                pure BF.BlockFetchReceiver
                  { BF.handleBlock     = \_ ->
                      error "blockFetchClient: unexpected second block."
                  , BF.handleBatchDone = pure ()
                  }
            , BF.handleBatchDone = pure ()
            }
          -- MsgNoBlocks: the node no longer has the requested block (e.g. it
          -- was pruned or belongs to a fork that the node has since rolled
          -- back).
        , BF.handleNoBlocks = STM.atomically $
            ---------- STM START ----------
            -- Filter the peeked header out of stateHeaders (the other removal
            -- path is ChainSync's rollback filter; see module header).
            STM.modifyTVar'
              (stateHeaders state)
              (Seq.filter
                (\h ->
                  let headerPoint = Net.BlockPoint
                                      (Block.blockSlot h)
                                      (Block.blockHash h)
                  in  headerPoint /= point
                )
              )
            ---------- STM ENDED ----------
        }
      )
      -- Recursion. The continuation. Start the peek all over again.
      (blockFetchClient state)

--------------------------------------------------------------------------------
-- Block Processing.
--------------------------------------------------------------------------------

-- | Claim the previously peeked header from the head of @stateHeaders@.
--
-- Returns @True@ if the header was found and removed (slot and hash match the
-- head of the queue). Returns @False@ if the header is no longer present either
-- because a rollback already filtered it out, or because a previous call
-- already claimed it.
claimHeader :: State -> Net.Point CardanoBlock -> STM.STM Bool
claimHeader state point = do
  headersSeq <- STM.readTVar (stateHeaders state)
  if Seq.null headersSeq
    then do
      -- ChainSync rolled back the header.
      pure False
    else do
      -- Get the first header in the sequence (like a queue).
      let header      = Seq.index headersSeq 0
          headerPoint = Net.BlockPoint
                          (Block.blockSlot header)
                          (Block.blockHash header)
      -- As new headers are added at the end of the sequence, we check that the
      -- first one is still the one we peeked.
      if headerPoint == point
        then do
          -- We can remove it and let BlockFetch process it.
          STM.writeTVar (stateHeaders state) (Seq.drop 1 headersSeq)
          pure True
        else do
          -- ChainSync rolled back the header.
          pure False

-- | Add a block to @statePendingBlocks@ and broadcast any transactions that
-- have reached the configured confirmation depth.
--
-- Must be called inside an @atomically@ block together with 'claimHeader'
-- so that the header consumption and block insertion are a single atomic step.
processBlock :: State -> CardanoBlock -> STM.STM ()
processBlock state newBlock = do
  let depth = fromIntegral (confirmationDepth (stateConfig state)) :: Block.BlockNo
  tip     <- STM.readTVar (stateCurrentTip state)
  pending <- STM.readTVar (statePendingBlocks state)
  let isConfirmed block = case tip of
        Net.TipGenesis       -> False
        Net.Tip _ _ tipBlock -> tipBlock >= Block.blockNo block + depth
      (confirmed, remaining) =
        Seq.spanl isConfirmed (pending <> Seq.singleton newBlock)
  STM.writeTVar (statePendingBlocks state) remaining
  -- Broadcast each confirmed transaction.
  forM_ (toList confirmed) $ \block ->
    forM_ (extractTxIds block) $ \txId ->
      STM.writeTChan (stateBroadcast state) ConfirmedTx
        { confirmedTxId      = txId
        , confirmedTxBlockNo = Block.blockNo block
        , confirmedTxSlotNo  = Block.blockSlot block
        }

--------------------------------------------------------------------------------
-- Transaction ID Extraction.
--------------------------------------------------------------------------------

-- | Extract all transaction IDs from a Cardano block.
extractTxIds :: CardanoBlock -> [Api.TxId]
extractTxIds = \case
  -- Byron era: skip (different transaction format, not relevant for benchmarking)
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
