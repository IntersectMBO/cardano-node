{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | Transaction confirmation tracking via local ChainSync (NodeToClient).
--
-- This is the NodeToClient counterpart of
-- "Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync". The key
-- simplification is that N2C ChainSync delivers full deserialized blocks (not
-- just headers), so the entire pipeline collapses into a single ChainSync
-- client and no separate BlockFetch client or intermediate header queue is
-- needed.
--
-- @
-- N2N:  ChainSync (headers) -> header queue -> BlockFetch (blocks) -> process
-- N2C:  ChainSync (blocks)  ----------------------------------------> process
-- @
--
-- == Confirmation Depth
--
-- The @confirmationDepth@ parameter controls when transactions are considered
-- confirmed:
--
-- * @0@: Immediate — callback fires as soon as a block is seen (no reorg
--        protection).
-- * @N@: Wait for N additional blocks on top before confirming.
--
-- == Rollback Handling
--
-- When ChainSync announces a rollback:
--
-- * All pending blocks after the rollback point are discarded.
-- * Transactions in discarded blocks are never confirmed.
-- * Transactions already confirmed (buried deeper than the rollback) are
--   unaffected.
--
-- == Usage
--
-- @
-- state <- emptyState Config { confirmationDepth = 6 }
-- sub   <- atomically $ dupTChan (stateBroadcast state)
-- -- Pass 'chainSyncClient state' to 'NodeToClient.connect'
-- -- Read confirmed transactions from sub via readTChan
-- @
module Cardano.Benchmarking.TxCentrifuge.NodeToClient.TxIdSync
  ( -- * Configuration
    Config (..)
    -- * Client type
  , ChainSyncClient
    -- * State
  , State, emptyState
    -- * Subscription.
  , stateBroadcast
    -- * Protocol Client
  , chainSyncClient
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Monad (forM_)
import Data.Foldable (toList)
import Numeric.Natural (Natural)
----------------
-- containers --
----------------
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Block qualified as Block
---------------------------
-- ouroboros-network:api --
---------------------------
import Ouroboros.Network.Block qualified as Net
---------------------------------
-- ouroboros-network:protocols --
---------------------------------
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Block qualified as Block

--------------------------------------------------------------------------------
-- Types.
--------------------------------------------------------------------------------

-- | Configuration for transaction synchronization.
data Config = Config
  { -- | Number of blocks to wait before confirming.
    -- 0 = immediate (no reorg protection), N = wait for N blocks on top.
    confirmationDepth :: !Natural
  }

-- | N2C ChainSync delivers full deserialized blocks (not just headers).
type ChainSyncClient =
  CS.ChainSyncClient
    Block.CardanoBlock
    (Net.Point Block.CardanoBlock)
    (Net.Tip Block.CardanoBlock)
    IO
    ()

--------------------------------------------------------------------------------
-- Internal State.
--------------------------------------------------------------------------------

-- | Shared state for the ChainSync client.
--
-- Compared to the N2N version, there is no @stateHeaders@ queue: N2C
-- ChainSync delivers full blocks directly, so we go straight from
-- roll-forward to block processing.
data State = State
  { -- | Configuration.
    stateConfig        :: !Config
    -- | Current chain tip as reported by the node.
  , stateCurrentTip    :: !(STM.TVar (Net.Tip Block.CardanoBlock))
    -- | Blocks received but not yet confirmed (ordered by block number).
    -- Uses @TVar (Seq …)@ instead of @TBQueue@ because rollbacks need to
    -- filter out blocks past the rollback point.
  , statePendingBlocks :: !(STM.TVar (Seq Block.CardanoBlock))
    -- | Broadcast channel for confirmed transactions.
    -- Write-only end; subscribers obtain a read-end via
    -- @STM.dupTChan . stateBroadcast@.
  , stateBroadcast     :: !(STM.TChan Block.ConfirmedTx)
  }

-- | Create initial sync state.
emptyState :: Config -> IO State
emptyState config = do
  pendingBlocks <- STM.newTVarIO Seq.empty
  currentTip    <- STM.newTVarIO Net.TipGenesis
  broadcast     <- STM.newBroadcastTChanIO
  pure State
    { stateConfig        = config
    , statePendingBlocks = pendingBlocks
    , stateCurrentTip    = currentTip
    , stateBroadcast     = broadcast
    }

--------------------------------------------------------------------------------
-- ChainSync Client.
--------------------------------------------------------------------------------

-- | ChainSync client that receives full blocks and tracks confirmations.
--
-- On @MsgRollForward@:  processes the block immediately (no header queue,
--                       no BlockFetch because the block is already complete).
-- On @MsgRollBackward@: discards pending blocks past the rollback point.
chainSyncClient :: State -> ChainSyncClient
chainSyncClient state = CS.ChainSyncClient $ pure clientStIdle
  where
    -- Request the next update from the server.
    clientStIdle = CS.SendMsgRequestNext
      (pure ())    -- Action when server says "await".
      clientStNext -- Handler for the roll-forward / roll-backward response.
    -- Handle the server's roll-forward or roll-backward response.
    clientStNext = CS.ClientStNext
      { -- A new block arrived. Update the tip and process in one transaction.
        CS.recvMsgRollForward = \block tip -> CS.ChainSyncClient $ do
          STM.atomically $ do
            ---------- STM START ----------
            STM.writeTVar (stateCurrentTip state) tip
            processNewBlock state block
            ---------- STM ENDED ----------
          pure clientStIdle
      , -- Rollback: discard pending blocks past the rollback point.
        -- TODO: Broadcast TxIdOrphaned for txs in discarded blocks so the
        -- RecycleOnConfirm strategy can cancel pending entries and re-queue
        -- original inputs. See docs/Leios.md "fork-and-evict robustness".
        CS.recvMsgRollBackward = \rollbackPoint tip -> CS.ChainSyncClient $ do
          let keepBlock blk = case rollbackPoint of
                Net.BlockPoint newSlot _ -> Block.blockSlot blk <= newSlot
                Net.GenesisPoint         -> False
          STM.atomically $ do
            ---------- STM START ----------
            STM.writeTVar   (stateCurrentTip    state) tip
            STM.modifyTVar' (statePendingBlocks state) (Seq.filter keepBlock)
            ---------- STM ENDED ----------
          pure clientStIdle
      }

--------------------------------------------------------------------------------
-- Block Processing.
--------------------------------------------------------------------------------

-- | Add a block to @statePendingBlocks@ and broadcast any transactions that
-- have reached the configured confirmation depth.
--
-- Must be called inside an @atomically@ block together with the tip update
-- so that the tip write and block processing are a single atomic step.
--
-- TODO: This is identical to 'NodeToNode.TxIdSync.processBlock'. Once recycling
-- of "due" transactions is added to both, extract the shared logic into a
-- common helper (e.g. in Block.hs) parameterised over the common state fields.
processNewBlock :: State -> Block.CardanoBlock -> STM.STM ()
processNewBlock state newBlock = do
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
    forM_ (Block.extractTxIds block) $ \txId ->
      STM.writeTChan (stateBroadcast state) Block.ConfirmedTx
        { Block.confirmedTxId      = txId
        , Block.confirmedTxBlockNo = Block.blockNo block
        , Block.confirmedTxSlotNo  = Block.blockSlot block
        }

