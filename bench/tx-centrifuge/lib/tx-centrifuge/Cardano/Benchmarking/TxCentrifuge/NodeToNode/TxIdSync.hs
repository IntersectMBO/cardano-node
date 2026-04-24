{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | Transaction confirmation tracking via ChainSync and BlockFetch
-- (NodeToNode).
--
-- This is the NodeToNode counterpart of
-- "Cardano.Benchmarking.TxCentrifuge.NodeToClient.TxIdSync". N2N ChainSync
-- delivers only headers, so a separate BlockFetch client is needed to
-- retrieve full block bodies. The two clients share state via
-- @stateHeaders@.
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
-- == Header Queue (@stateHeaders@)
--
-- The header queue is written to by ChainSync and read by BlockFetch.
-- Both protocols can remove headers from it:
-- * __ChainSync__ filters out headers children of the rollback point on
--   @MsgRollBackward@.
-- * __BlockFetch__ claims the head on @MsgBlock@ (block received) or filters
--   it out on @MsgNoBlocks@ (block no longer available on the node).
--
-- == Usage
--
-- @
-- state <- emptyState Config { confirmationDepth = 6 }
-- sub   <- atomically $ dupTChan (stateBroadcast state)
-- N2N.connect ioManager codecConfig networkMagic tracers addrInfo
--   N2N.emptyClients
--     { N2N.clientChainSync  = Just $ chainSyncClient state
--     , N2N.clientBlockFetch = Just $ blockFetchClient state
--     }
-- confirmed <- atomically $ readTChan sub
-- @
module Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync
  ( -- * Configuration
    Config (..)
    -- * Client types
  , BlockFetchClient
  , ChainSyncClient
    -- * State
  , State, emptyState
    -- * Subscription.
  , stateBroadcast
    -- * Protocol Clients
  , chainSyncClient
  , blockFetchClient
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
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BF
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BFType
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

-- | BlockFetch client for retrieving full block bodies.
type BlockFetchClient =
  BF.BlockFetchClient
    Block.CardanoBlock
    (Net.Point Block.CardanoBlock)
    IO
    ()

-- | ChainSync client for following the chain tip (receives headers).
type ChainSyncClient =
  CS.ChainSyncClient
    (Block.Header Block.CardanoBlock)
    (Net.Point Block.CardanoBlock)
    (Net.Tip Block.CardanoBlock)
    IO
    ()

--------------------------------------------------------------------------------
-- Internal State.
--------------------------------------------------------------------------------

-- | Shared state between ChainSync and BlockFetch clients.
data State = State
  { -- | Configuration.
    stateConfig        :: !Config
    -- | Current chain tip as reported by the node.
  , stateCurrentTip    :: !(STM.TVar (Net.Tip Block.CardanoBlock))
    -- | Headers received from ChainSync, waiting for BlockFetch.
    -- Removed by both ChainSync (rollback filter) and BlockFetch (claim on
    -- block received, filter on block unavailable).
    -- Uses @TVar (Seq …)@ instead of @TBQueue@ because both removal patterns
    -- need filtered access, which TBQueue does not support.
  , stateHeaders       :: !(STM.TVar (Seq (Block.Header Block.CardanoBlock)))
    -- | Blocks fetched but not yet confirmed (ordered by block number).
    -- Uses @TVar (Seq …)@ instead of @TBQueue@ because both removal patterns
    -- need filtered access, which TBQueue does not support.
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
  headerQueue   <- STM.newTVarIO Seq.empty
  pendingBlocks <- STM.newTVarIO Seq.empty
  limbo         <- STM.newTVarIO Seq.empty
  broadcast     <- STM.newBroadcastTChanIO
  pure State
    { stateConfig        = config
    , stateCurrentTip    = currentTip
    , stateHeaders       = headerQueue
    , statePendingBlocks = pendingBlocks
    , stateLimbo         = limbo
    , stateBroadcast     = broadcast
    }

--------------------------------------------------------------------------------
-- ChainSync Client.
--------------------------------------------------------------------------------

-- | ChainSync client that follows the chain and adds headers to a queue.
--
-- On @MsgRollForward@:  queues the header for BlockFetch.
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
      { -- Advance the tip and queue the new header for BlockFetch to use.
        CS.recvMsgRollForward = \header tip -> CS.ChainSyncClient $ do
          STM.atomically $ do
            ---------- STM START ----------
            STM.writeTVar   (stateCurrentTip state) tip
            -- Function `blockFetchClient` below blocks on `readTVar`.
            STM.modifyTVar' (stateHeaders    state)
              -- Append the new header at the end!
              (\q -> q <> Seq.singleton header)
            ---------- STM ENDED ----------
          -- Continue following the chain.
          pure clientStIdle
      , -- Rollback: discard pending blocks that are children of the rollback
        -- point and move discarded blocks' txs IDs to limbo.
        CS.recvMsgRollBackward = \rollbackPoint tip -> CS.ChainSyncClient $ do
          let keepHeader header = case rollbackPoint of
                Net.BlockPoint newSlot _ -> Block.blockSlot header <= newSlot
                Net.GenesisPoint         -> False
              keepBlock  block  = case rollbackPoint of
                Net.BlockPoint newSlot _ -> Block.blockSlot block  <= newSlot
                Net.GenesisPoint         -> False
          STM.atomically $ do
            ---------- STM START ----------
            STM.writeTVar   (stateCurrentTip state) tip
            STM.modifyTVar' (stateHeaders    state) (Seq.filter keepHeader)
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
                    then processNewBlock state block
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
claimHeader :: State -> Net.Point Block.CardanoBlock -> STM.STM Bool
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
--
-- TODO: This is identical to 'NodeToClient.TxIdSync.processNewBlock'. Once
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

