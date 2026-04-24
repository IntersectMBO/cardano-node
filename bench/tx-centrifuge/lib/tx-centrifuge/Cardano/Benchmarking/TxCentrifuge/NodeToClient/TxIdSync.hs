{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | Transaction confirmation tracking via local ChainSync (NodeToClient).
--
-- This is the NodeToClient counterpart of
-- "Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync". N2C ChainSync
-- delivers full deserialized blocks (not just headers), so the entire
-- pipeline collapses into a single ChainSync client and no separate
-- BlockFetch client or intermediate header queue is needed.
--
-- @
-- N2N:  ChainSync (headers) -> header queue -> BlockFetch (blocks) -> process
-- N2C:  ChainSync (blocks)  ----------------------------------------> process
-- @
--
-- The purpose of this module is to recycle funds as soon as possible.
-- Without inspecting the nodes' mempools, we follow the chain and
-- broadcast two kinds of events:
--
-- * @Right@: a transaction reached confirmation depth — its outputs
--   can be recycled.
-- * @Left@: a transaction was in a rolled-back block and did not
--   reappear in the winning fork — its original inputs can be
--   recycled.
--
-- The @Left@ path recovers the main source of lost funds: in Cardano,
-- a rollback does not re-add the rolled-back block's transactions to
-- the mempool. Without this recovery, those inputs would be permanently
-- leaked from the recycling loop.
--
-- Rolled-back transactions are not orphaned immediately. They are held
-- in limbo because the winning fork may contain some (but not all) of
-- them, possibly at different block heights. Only transactions that do
-- not reappear within 2×@confirmationDepth@ blocks are broadcast as
-- orphans.
--
-- The @confirmationDepth@ parameter (D) controls when @Right@ events
-- fire: a block is confirmed after D blocks on top. For orphans,
-- D is applied twice: the rolled-back block's replacement must first
-- be confirmed (D blocks), then the limbo entry waits D more confirmed
-- blocks to allow the tx to reappear at a different height on the
-- winning fork. Total orphan latency: 2×D blocks.
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
import Data.Foldable (forM_, toList)
import Numeric.Natural (Natural)
----------------
-- containers --
----------------
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
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
    -- Uses @TVar (Seq …)@ instead of @TBQueue@ because rollbacks need to filter
    -- out blocks children of the rollback point.
  , statePendingBlocks :: !(STM.TVar (Seq Block.CardanoBlock))
    -- | Tx IDs from rolled-back blocks awaiting resolution.
    -- Resolved when confirmed blocks catch up: entries whose tx ID appears in
    -- a confirmed block are removed; entries whose height has been confirmed
    -- past are broadcast as orphans (Left).
  , stateLimbo         :: !(STM.TVar (Seq Block.BlockTx))
    -- | Broadcast channel for confirmed and orphaned transactions.
    -- 'Right' = confirmed (recycle output inputs).
    -- 'Left'  = orphaned  (recycle original inputs).
    -- Write-only end; subscribers obtain a read-end via
    -- @STM.dupTChan . stateBroadcast@.
  , stateBroadcast     :: !(STM.TChan (Either Block.BlockTx Block.BlockTx))
  }

-- | Create initial sync state.
emptyState :: Config -> IO State
emptyState config = do
  currentTip    <- STM.newTVarIO Net.TipGenesis
  pendingBlocks <- STM.newTVarIO Seq.empty
  limbo         <- STM.newTVarIO Seq.empty
  broadcast     <- STM.newBroadcastTChanIO
  pure State
    { stateConfig        = config
    , stateCurrentTip    = currentTip
    , statePendingBlocks = pendingBlocks
    , stateLimbo         = limbo
    , stateBroadcast     = broadcast
    }

--------------------------------------------------------------------------------
-- ChainSync Client.
--------------------------------------------------------------------------------

-- | ChainSync client that receives full blocks and tracks confirmations.
--
-- On @MsgRollForward@:  processes the block immediately (no header queue,
--                       no BlockFetch because the block is already complete).
-- On @MsgRollBackward@: discards pending blocks children of the rollback point.
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
      , -- Rollback: discard pending blocks that are children of the rollback
        -- point and move discarded blocks' txs IDs to limbo.
        CS.recvMsgRollBackward = \rollbackPoint tip -> CS.ChainSyncClient $ do
          let keepBlock blk = case rollbackPoint of
                Net.BlockPoint newSlot _ -> Block.blockSlot blk <= newSlot
                Net.GenesisPoint         -> False
          STM.atomically $ do
            ---------- STM START ----------
            STM.writeTVar (stateCurrentTip state) tip
            pendingBlocks <- STM.readTVar (statePendingBlocks state)
            -- spanl: pendingBlocks ordered by slot (appended in chain order).
            let (keep, discard) = Seq.spanl keepBlock pendingBlocks
            -- Kept blocks: overrides entirely `statePendingBlocks`.
            STM.writeTVar (statePendingBlocks state) keep
            -- Discarded blocks: Append discarded txs IDs to `stateLimbo`.
            let newLimbo = Seq.fromList
                  [ Block.BlockTx
                      { Block.blockTxId      = txId
                      , Block.blockTxBlockNo = Block.blockNo   block
                      , Block.blockTxSlotNo  = Block.blockSlot block
                      }
                  | block <- toList discard
                  , txId  <- Block.extractTxIds block
                  ]
            STM.modifyTVar' (stateLimbo state) (\q -> q <> newLimbo)
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
-- TODO: This is identical to 'NodeToNode.TxIdSync.processNewBlock'. Once
-- recycling of "due" transactions is added to both, extract the shared logic
-- into a common helper (e.g. in Block.hs) parameterised over the common state
-- fields.
processNewBlock :: State -> Block.CardanoBlock -> STM.STM ()
processNewBlock state newBlock = do
  tip           <- STM.readTVar (stateCurrentTip    state)
  pendingBlocks <- STM.readTVar (statePendingBlocks state)
  -- Appends the new block and splits the ordered `Seq` of blocks into two.
  let (confirmedBlocks, remainingBlocks) =
       let depth = fromIntegral
                     (confirmationDepth (stateConfig state)) :: Block.BlockNo
           isBlockConfirmed block = case tip of
             Net.TipGenesis         -> False
             Net.Tip _ _ tipBlockNo -> tipBlockNo >= Block.blockNo block + depth
       in  -- spanl: pending ordered by blockNo (appended in chain order).
           Seq.spanl isBlockConfirmed (pendingBlocks <> Seq.singleton newBlock)
  -- Remove confirmed blocks from state first.
  STM.writeTVar (statePendingBlocks state) remainingBlocks
  -- Broadcast each confirmed transaction.
  forM_ confirmedBlocks $ \block -> do -- No `toList`, skips intermediate list.
    forM_ (Block.extractTxIds block) $ \txId -> do
      STM.writeTChan
        (stateBroadcast state)
        (Right Block.BlockTx
          { Block.blockTxId      = txId
          , Block.blockTxBlockNo = Block.blockNo   block
          , Block.blockTxSlotNo  = Block.blockSlot block
          }
        )
  -- Resolve limbo against confirmed blocks.
  -- Tx IDs that reappear in a confirmed block are removed (the `Right`
  -- broadcast above already recycles their outputs).
  -- Limbo entries whose height has been confirmed past are true orphans
  -- (broadcast as `Left` below).
  limbo <- STM.readTVar (stateLimbo state)
  let (keepLimbo, toOrphan) =
        let depth = fromIntegral
                      (confirmationDepth (stateConfig state)) :: Block.BlockNo
            lastConfirmedBlockNo = case confirmedBlocks of
              _ Seq.:|> lastConfirmedBlock -> Block.blockNo lastConfirmedBlock
              _                            -> 0
            -- Extract confirmed txs IDs to a set, more efficient queries.
            confirmedTxIdSet =
              Set.fromList
                [ txId
                | block <- toList confirmedBlocks
                , txId  <- Block.extractTxIds block
                ]
            blockTxConfirmed e = Block.blockTxId e `Set.member` confirmedTxIdSet
            -- Allow confirmationDepth extra blocks for the tx to reappear at a
            -- different height on the winning fork. Forks deeper than
            -- confirmationDepth already cause permanent fund loss by design.
            pastConfirmed    e =
              lastConfirmedBlockNo >= Block.blockTxBlockNo e + depth
        in
            -- partition, not spanl!
            -- The limbo is unordered (multiple rollbacks at different heights).
            Seq.partition
              (not . pastConfirmed)
              -- First discard from limbo all confirmed tx IDs.
              (Seq.filter (not . blockTxConfirmed) limbo)
  -- Remove orphaned txs from state first.
  STM.writeTVar (stateLimbo state) keepLimbo
  -- Broadcast orphaned txs.
  forM_ toOrphan $ \entry -> do -- No `toList`, skips intermediate list.
    STM.writeTChan (stateBroadcast state) (Left entry)

