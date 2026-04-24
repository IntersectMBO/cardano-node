{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

-- | LocalTxSubmission client for NodeToClient connections.
--
-- LocalTxSubmission is a synchronous, push-based protocol: the client submits
-- one transaction at a time and receives an immediate accept\/reject response.
-- This contrasts with the pull-based TxSubmission2 protocol used by
-- NodeToNode, where the remote node drives the conversation by requesting
-- transactions.
--
-- The client loops forever: fetch a transaction, submit it, report the result,
-- repeat.
module Cardano.Benchmarking.TxCentrifuge.NodeToClient.TxSubmission
  ( TxSubmissionClient
  , SubmitResult (..)
  , txSubmissionClient
  ) where

--------------------------------------------------------------------------------

-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Mempool
---------------------------------
-- ouroboros-network:protocols --
---------------------------------
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( SubmitResult (..)
  )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as LTxSub
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Block qualified as Block

--------------------------------------------------------------------------------
-- Client type.
--------------------------------------------------------------------------------

-- | A LocalTxSubmission client submitting Conway-era transactions.
--
-- The protocol is synchronous: each 'SendMsgSubmitTx' blocks until the node
-- replies with 'SubmitSuccess' or @'SubmitFail' reason@. The node validates
-- the transaction against its current ledger state and mempool before
-- replying.
type TxSubmissionClient =
  LTxSub.LocalTxSubmissionClient
    (Mempool.GenTx Block.CardanoBlock)
    (Mempool.ApplyTxErr Block.CardanoBlock)
    IO
    ()

--------------------------------------------------------------------------------
-- Client construction.
--------------------------------------------------------------------------------

-- | Build a 'TxSubmissionClient' that loops forever, submitting
-- transactions one at a time.
--
-- On each iteration:
--
-- 1. Call @blockingFetch@ to obtain the next transaction (may block).
-- 2. Submit the transaction to the local node.
-- 3. Call @onResult@ with the 'Api.TxId' and the submit result
--    ('SubmitSuccess' or @'SubmitFail' reason@ with a @show@-ed rejection).
-- 4. Repeat from (1).
txSubmissionClient
  :: String
  -- ^ Target name (for identification in callbacks).
  -> IO (Api.Tx Api.ConwayEra)
  -- ^ Blocking fetch: wait for the next transaction to submit.
  -> (Api.TxId -> SubmitResult String -> IO ())
  -- ^ Callback after each submission. The rejection reason (if any) is
  --   stringified via 'show' on @'Mempool.ApplyTxErr' Block.CardanoBlock@.
  -> TxSubmissionClient
txSubmissionClient _targetName blockingFetch onResult =
  LTxSub.LocalTxSubmissionClient nextTx
  where
    nextTx :: IO
      ( LTxSub.LocalTxClientStIdle
          (Mempool.GenTx Block.CardanoBlock)
          (Mempool.ApplyTxErr Block.CardanoBlock)
          IO
          ()
      )
    nextTx = do
      tx <- blockingFetch
      let !genTx = Block.toGenTx tx
          !txId  = Api.getTxId (Api.getTxBody tx)
      pure $ LTxSub.SendMsgSubmitTx genTx $ \result -> do
        onResult txId (mapResult result)
        nextTx

    mapResult
      :: SubmitResult (Mempool.ApplyTxErr Block.CardanoBlock)
      -> SubmitResult String
    mapResult SubmitSuccess    = SubmitSuccess
    mapResult (SubmitFail err) = SubmitFail (show err)

