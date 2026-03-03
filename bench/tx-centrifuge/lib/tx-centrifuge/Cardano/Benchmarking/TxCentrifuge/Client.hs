{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.Client
  ( mkClient
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Arrow ((&&&))
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
-------------------------
-- ouroboros-consensus --
-------------------------
import Ouroboros.Consensus.Cardano qualified as Consensus (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block qualified as Block
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Mempool
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  qualified as Mempool (TxId(ShelleyTxId))
-----------------------
-- ouroboros-network --
-----------------------
import Ouroboros.Network.Protocol.TxSubmission2.Client qualified as TxSub
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as TxSub
import Ouroboros.Network.SizeInBytes qualified as Net
---------------------
-- tx-centrifuge --
---------------------
import Cardano.Benchmarking.TxCentrifuge.Tracing qualified as Tracing

--------------------------------------------------------------------------------

type CardanoBlock = Consensus.CardanoBlock Eras.StandardCrypto

-- | Internal state: the unacknowledged tx sequence (oldest first, matching the
-- server's FIFO).  Acks remove elements from the front; new announcements are
-- appended at the back.
-- Uses 'Seq' for O(1) length and O(log n) take/drop (vs O(n) for lists).
type UnAcked = Seq.Seq (Api.Tx Api.ConwayEra)

-- | Create a TxSubmission2 client that pulls txs from caller-supplied IO
-- actions. No intermediate queue, the blocking action is called for the first
-- mandatory tx, and the non-blocking action drains the rest up to the
-- requested count, capped by @maxBatchSize@.
mkClient
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
  -> TxSub.TxSubmissionClient
       (Mempool.GenTxId CardanoBlock)
       (Mempool.GenTx CardanoBlock)
       IO
       ()
mkClient tracer targetName maxBatchSize blockingFetch nonBlockingFetch =
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
-- This is the primary token consumption path for both 'SingBlocking'
-- (after the first mandatory tx) and 'SingNonBlocking' requests.
-- Stops as soon as the callback returns 'Nothing' (rate-limited).
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
  -> TxSub.SingBlockingStyle blocking
  -- | Number of tx IDs to ACK.
  -> TxSub.NumTxIdsToAck
  -- | Number of tx IDs requested.
  -> TxSub.NumTxIdsToReq
  -> IO ( TxSub.ClientStTxIds
            blocking
            (Mempool.GenTxId CardanoBlock)
            (Mempool.GenTx CardanoBlock)
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
  ----------------------------------------------
  traceWith tracer $
    Tracing.RequestTxIds
      targetName
      (map txId (toList unacked)) -- TxIds not yet acknowledged.
      (fromIntegral ackNum)       -- how many the node is ACKing.
      (fromIntegral reqNum)       -- how many new TxIds it wants.
  -- Pull txs from the callbacks, capped by maxBatchSize.
  --------------------------------------------------------
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
  -- Drop acknowledged txs.
  --------------------------------------
  -- Drop acknowledged txs from the front (oldest first, matching the server's
  -- FIFO), then append new announcements at the back.
  let !unacked' =
        let !remaining = Seq.drop (fromIntegral ackNum) unacked
        in  remaining Seq.>< Seq.fromList newTxs
  -- Trace: we replied with tx id announcements.
  -----------------------------------------------
  traceWith tracer $
    Tracing.ReplyTxIds
      targetName
      (fromIntegral ackNum)        -- how many the node is ACKing.
      (fromIntegral reqNum)        -- how many new TxIds it wants.
      (map txId (toList unacked')) -- updated unacked after ACK + new.
      (map txId newTxs)            -- TxIds we announced in this reply.
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
      case NE.nonEmpty newTxs of
        Nothing  -> error "requestTxIds: blocking fetch returned empty list!"
        Just txs -> do
          pure $ TxSub.SendMsgReplyTxIds
                   (TxSub.BlockingReply    $ fmap txToIdSize txs   )
                   nextIdle
    TxSub.SingNonBlocking -> do
          pure $ TxSub.SendMsgReplyTxIds
                   (TxSub.NonBlockingReply $ fmap txToIdSize newTxs)
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
  -> [Mempool.GenTxId CardanoBlock]
  -> IO ( TxSub.ClientStTxs
            (Mempool.GenTxId CardanoBlock)
            (Mempool.GenTx CardanoBlock)
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
  ----------------------------------------------------
  traceWith tracer $
    Tracing.RequestTxs
      targetName
      (map fromGenTxId requestedTxIds) -- TxIds the node requested.
  -- Build response.
  ------------------
  -- The list is converted to a Set for efficient filtering.
  let requestedTxIdsSet = Set.fromList (map fromGenTxId requestedTxIds)
      txIdsToSend = filter
                      (\tx ->
                        txId tx `Set.member` requestedTxIdsSet
                      )
                      (toList unacked)
  -- Trace: we replied with the matching transactions.
  -----------------------------------------------------
  traceWith tracer $
    Tracing.ReplyTxs
      targetName
      (map fromGenTxId requestedTxIds) -- TxIds the node requested.
      (map txId txIdsToSend)           -- TxIds we actually sent.
  -- Response and protocol continuation.
  --------------------------------------
  pure $ TxSub.SendMsgReplyTxs (map toGenTx txIdsToSend) $ TxSub.ClientStIdle
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

-- Helpers.
-------------------------------------------------------------------------------

-- | Extract the cardano-api TxId from a signed transaction.
txId :: Api.Tx Api.ConwayEra -> Api.TxId
txId = Api.getTxId . Api.getTxBody

-- | Convert a Tx to (GenTxId, SizeInBytes) for announcement.
txToIdSize :: Api.Tx Api.ConwayEra
           -> (Mempool.GenTxId CardanoBlock, Net.SizeInBytes)
txToIdSize =
  (Mempool.txId . toGenTx)
    &&& (Net.SizeInBytes . fromIntegral . txSize)
  where
    txSize :: Api.Tx Api.ConwayEra -> Int
    txSize = BS.length . Api.serialiseToCBOR

-- | Convert a cardano-api Tx to a consensus GenTx.
toGenTx :: Api.Tx Api.ConwayEra -> Mempool.GenTx CardanoBlock
toGenTx tx = Api.toConsensusGenTx $ Api.TxInMode Api.shelleyBasedEra tx

-- | Convert a consensus GenTxId back to a cardano-api TxId.
--
-- NOTE: this generator only produces Conway-era transactions.
-- If the Cardano network undergoes a hard fork to a new era while the
-- generator is running, this will fail. Update the pattern match when adding
-- support for a new era.
fromGenTxId :: Mempool.GenTxId CardanoBlock -> Api.TxId
fromGenTxId (Block.GenTxIdConway (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId other = error $ "fromGenTxId: Conway only, received " ++ show other
