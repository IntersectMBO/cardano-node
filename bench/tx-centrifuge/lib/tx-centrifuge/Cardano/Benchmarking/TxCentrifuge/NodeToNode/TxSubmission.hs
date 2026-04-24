{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxSubmission
  ( TxSubmissionClient
  , txSubmissionClient
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Foldable (toList)
import Numeric.Natural (Natural)
import Data.List.NonEmpty qualified as NE
----------------
-- bytestring --
----------------
import Data.ByteString qualified as BS
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
----------------
-- containers --
----------------
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
-------------------
-- contra-tracer --
-------------------
import "contra-tracer" Control.Tracer (Tracer, traceWith)
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Mempool
---------------------------
-- ouroboros-network:api --
---------------------------
import Ouroboros.Network.SizeInBytes qualified as Net
---------------------------------
-- ouroboros-network:protocols --
---------------------------------
import Ouroboros.Network.Protocol.TxSubmission2.Client qualified as TxSub
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as TxSub
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Block qualified as Block
import Cardano.Benchmarking.TxCentrifuge.Tracing qualified as Tracing

--------------------------------------------------------------------------------

-- | TxSubmission2 client for submitting transactions.
type TxSubmissionClient =
  TxSub.TxSubmissionClient
    (Mempool.GenTxId Block.CardanoBlock)
    (Mempool.GenTx Block.CardanoBlock)
    IO
    ()

-- | A pre-computed entry in the unacknowledged sequence.
--
-- All protocol-ready values are computed once at entry time (via 'toEntry')
-- rather than re-derived on every protocol round-trip.
data UnAckedEntry = UnAckedEntry
  { -- | For protocol announcement ('MsgReplyTxIds') and matching ('MsgRequestTxs').
    uaeGenTxId :: !(Mempool.GenTxId Block.CardanoBlock)
    -- | For protocol body delivery ('MsgReplyTxs').
  , uaeGenTx   :: !(Mempool.GenTx Block.CardanoBlock)
    -- | For protocol announcement ('MsgReplyTxIds').
  , uaeSize    :: !Net.SizeInBytes
  }

-- | Internal state: the unacknowledged tx sequence (oldest first, matching the
-- server's FIFO).  Acks remove elements from the front; new announcements are
-- appended at the back.
-- Uses 'Seq' for O(1) length and O(log n) take/drop (vs O(n) for lists).
type UnAcked = Seq.Seq UnAckedEntry

--------------------------------------------------------------------------------

-- | Convert a cardano-api Tx to an 'UnAckedEntry', pre-computing all
-- protocol-ready values. This is the single boundary crossing: every subsequent
-- protocol handler works with native consensus types.
toEntry :: Api.Tx Api.ConwayEra -> UnAckedEntry
toEntry tx =
  let !genTx   = Block.toGenTx tx
      !genTxId = Mempool.txId genTx
      !size    = Net.SizeInBytes
                   (fromIntegral (BS.length (Api.serialiseToCBOR tx)))
  in UnAckedEntry
       { uaeGenTxId = genTxId
       , uaeGenTx   = genTx
       , uaeSize    = size
       }

-- | Extract the protocol announcement pair from a pre-computed entry.
entryToIdSize :: UnAckedEntry -> (Mempool.GenTxId Block.CardanoBlock, Net.SizeInBytes)
entryToIdSize e = (uaeGenTxId e, uaeSize e)

--------------------------------------------------------------------------------

-- | Create a TxSubmission2 client that pulls txs from caller-supplied IO
-- actions. No intermediate queue, the blocking action is called for the first
-- mandatory tx, and the non-blocking action drains the rest up to the
-- requested count, capped by @maxBatchSize@.
txSubmissionClient
  -- | Tracer for structured TxSubmission2 events.
  :: Tracer IO Tracing.TxSubmission
  -- | Target name (remote node identifier).
  -> String
  -- | Max batch size per request.
  -> Natural
  -- | Blocking: wait for a token (must not fail).
  -> IO (Api.Tx Api.ConwayEra)
  -- | NonBlocking: poll for a token.
  -> IO (Maybe (Api.Tx Api.ConwayEra))
  -> TxSubmissionClient
txSubmissionClient tracer targetName maxBatchSize blockingFetch nonBlockingFetch =
  TxSub.TxSubmissionClient $ pure $ TxSub.ClientStIdle
    { TxSub.recvMsgRequestTxIds =
        requestTxIds
          tracer
          targetName maxBatchSize
          blockingFetch nonBlockingFetch
          Seq.empty
    , TxSub.recvMsgRequestTxs   =
        requestTxs
          tracer
          targetName maxBatchSize
          blockingFetch nonBlockingFetch
          Seq.empty
    }

--------------------------------------------------------------------------------

-- | Drain up to @n@ tokens without blocking.
-- This is the primary token consumption path for both 'SingBlocking' (after the
-- first mandatory tx) and 'SingNonBlocking' requests. Stops as soon as the
-- callback returns 'Nothing' (rate-limited).
drainUpTo :: Int
          -> IO (Maybe (Api.Tx Api.ConwayEra))
          -> IO [Api.Tx Api.ConwayEra]
drainUpTo 0 _ = pure []
drainUpTo n fetch = fetch >>= \case
  Nothing -> pure []
  Just x  -> (x :) <$> drainUpTo (n - 1) fetch

-- | Handle @MsgRequestTxIds@.
--
-- TxSubmission2 protocol semantics:
--   SingBlocking    → must return at least 1 tx; may block.
--   SingNonBlocking → return 0..reqNum txs; must not block.
--
-- In both cases, after satisfying the minimum (1 for blocking, 0 for
-- non-blocking), 'drainUpTo' fills the rest via non-blocking calls.
-- Under sustained load a Cardano node operates at near-full mempool capacity
-- and almost exclusively issues 'SingNonBlocking' requests, so the
-- non-blocking path is the dominant token consumption path.
-- See the fairness analysis in WorkloadRunner.runWorkload for details.
requestTxIds
  :: forall blocking.
  -- | Tracer for structured TxSubmission2 events.
     Tracer IO Tracing.TxSubmission
  -- | Target name (remote node identifier).
  -> String
  -- | Max batch size per request.
  -> Natural
  -- | Blocking: wait for a token (must not fail).
  -> IO (Api.Tx Api.ConwayEra)
  -- | NonBlocking: poll for a token.
  -> IO (Maybe (Api.Tx Api.ConwayEra))
  -- | Unacknowledged transactions (oldest first).
  -> UnAcked
  -- | Blocking style singleton:
  -- * 'SingBlocking':    (must return >= 1 tx).
  -- * 'SingNonBlocking': (may return 0).
  -> TxSub.SingBlockingStyle blocking
  -- | Number of tx IDs to ACK.
  -> TxSub.NumTxIdsToAck
  -- | Number of tx IDs requested.
  -> TxSub.NumTxIdsToReq
  -> IO ( TxSub.ClientStTxIds
            blocking
            (Mempool.GenTxId Block.CardanoBlock)
            (Mempool.GenTx Block.CardanoBlock)
            IO
            ()
        )
requestTxIds
  tracer
  targetName maxBatchSize
  blockingFetch nonBlockingFetch
  unacked
  blocking
  (TxSub.NumTxIdsToAck ackNum)
  (TxSub.NumTxIdsToReq reqNum)
  = do
  -- Trace: node asked for tx id announcements.
  ---------------------------------------------
  traceWith tracer $
    Tracing.RequestTxIds
      targetName
      -- TxIds not yet acknowledged.
      (map
        (Block.fromGenTxId . uaeGenTxId)
        (toList unacked)
      )
      (fromIntegral ackNum) -- How many the node is ACKing.
      (fromIntegral reqNum) -- How many new TxIds it wants.
  -- Pull txs from the callbacks, capped by maxBatchSize.
  -------------------------------------------------------
  newTxs <- do
    let !effectiveReq = min
                          (fromIntegral reqNum)
                          (fromIntegral maxBatchSize :: Int)
    case blocking of
      TxSub.SingBlocking -> do
        -- Block for exactly one tx (protocol minimum), then
        -- remaining up to effectiveReq-1 without blocking.
        tx1  <- blockingFetch
        rest <- drainUpTo (effectiveReq - 1) nonBlockingFetch
        pure (tx1 : rest)
      TxSub.SingNonBlocking -> do
        -- Return whatever is available up to effectiveReq.
        drainUpTo effectiveReq nonBlockingFetch
  -- Convert to protocol-ready entries (single boundary crossing).
  ----------------------------------------------------------------
  let !newEntries = map toEntry newTxs
  -- Drop acknowledged entries.
  -----------------------------
  -- Drop acknowledged entries from the front (oldest first, matching the
  -- server's FIFO), then append new announcements at the back.
  let !unacked' =
        let !remaining = Seq.drop (fromIntegral ackNum) unacked
        in  remaining Seq.>< Seq.fromList newEntries
  -- Trace: we replied with tx id announcements.
  ----------------------------------------------
  traceWith tracer $
    Tracing.ReplyTxIds
      targetName
      (fromIntegral ackNum) -- how many the node is ACKing.
      (fromIntegral reqNum) -- how many new TxIds it wants.
      -- updated unacked after ACK + new.
      (map (Block.fromGenTxId . uaeGenTxId) (toList unacked'))
      -- TxIds we announced in this reply.
      (map (Block.fromGenTxId . uaeGenTxId) newEntries)
  -- Build the protocol continuation.
  -----------------------------------
  let nextIdle = TxSub.ClientStIdle
        -- Continues the protocol loop with the updated unacked list.
        { TxSub.recvMsgRequestTxIds =
            requestTxIds
              tracer
              targetName maxBatchSize
              blockingFetch nonBlockingFetch
              unacked'
        , TxSub.recvMsgRequestTxs =
            requestTxs
              tracer
              targetName maxBatchSize
              blockingFetch nonBlockingFetch
              unacked'
        }
  -- Answer with what we obtained from the callbacks.
  ---------------------------------------------------
  case blocking of
    TxSub.SingBlocking -> do
      case NE.nonEmpty newEntries of
        Nothing  -> error "requestTxIds: blocking fetch returned empty list!"
        Just entries -> do
          pure $ TxSub.SendMsgReplyTxIds
                   (TxSub.BlockingReply    $ fmap entryToIdSize entries)
                   nextIdle
    TxSub.SingNonBlocking -> do
          pure $ TxSub.SendMsgReplyTxIds
                   (TxSub.NonBlockingReply $ fmap entryToIdSize newEntries)
                   nextIdle

-- | Handle @MsgRequestTxs@: look up requested tx ids in the unacked list and
-- send back the matching transactions.
requestTxs
  -- | Tracer for structured TxSubmission2 events.
  :: Tracer IO Tracing.TxSubmission
  -- | Target name (remote node identifier).
  -> String
  -- | Max batch size per request.
  -> Natural
  -- | Blocking: wait for a token (must not fail).
  -> IO (Api.Tx Api.ConwayEra)
  -- | NonBlocking: poll for a token.
  -> IO (Maybe (Api.Tx Api.ConwayEra))
  -- | Unacknowledged transactions (oldest first).
  -> UnAcked
  -- | Transaction IDs the node is requesting full bodies for.
  -> [Mempool.GenTxId Block.CardanoBlock]
  -> IO ( TxSub.ClientStTxs
            (Mempool.GenTxId Block.CardanoBlock)
            (Mempool.GenTx Block.CardanoBlock)
            IO
            ()
        )
requestTxs
  tracer
  targetName maxBatchSize
  blockingFetch nonBlockingFetch
  unacked
  requestedTxIds
  = do
  -- Trace: node asked for full transactions by TxId.
  ---------------------------------------------------
  traceWith tracer $
    Tracing.RequestTxs
      targetName
      (map Block.fromGenTxId requestedTxIds) -- TxIds the node requested.
  -- Build response.
  ------------------
  -- Match directly on consensus GenTxId (native protocol type).
  let requestedSet  = Set.fromList requestedTxIds
      entriesToSend = toList $ Seq.filter
                                 (\e -> uaeGenTxId e `Set.member` requestedSet)
                                 unacked
  -- Trace: we replied with the matching transactions.
  ----------------------------------------------------
  traceWith tracer $
    Tracing.ReplyTxs
      targetName
      -- TxIds the node requested.
      (map Block.fromGenTxId requestedTxIds)
      -- TxIds we actually sent.
      (map (Block.fromGenTxId . uaeGenTxId) entriesToSend)
  -- Response and protocol continuation.
  --------------------------------------
  pure $ TxSub.SendMsgReplyTxs (map uaeGenTx entriesToSend) $ TxSub.ClientStIdle
    -- Continues the protocol loop with no changes to the unacked list.
    { TxSub.recvMsgRequestTxIds =
        requestTxIds tracer targetName
          maxBatchSize blockingFetch nonBlockingFetch
          unacked
    , TxSub.recvMsgRequestTxs   =
        requestTxs tracer targetName
          maxBatchSize blockingFetch nonBlockingFetch
          unacked
    }

