{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.TxSubClient
  ( txSubmissionClient
  ) where

import           Prelude

import           Control.Monad.Class.MonadSTM (TMVar, MonadSTM, atomically,
                                               newEmptyTMVarM, putTMVar, takeTMVar)
import           Data.List.NonEmpty (fromList)

import           Cardano.CLI.Benchmarking.Tx.TxSubmission (RPCTxSubmission(..))
import           Ouroboros.Consensus.Mempool.API (GenTxId, GenTx)
import           Ouroboros.Network.Protocol.TxSubmission.Client (ClientStIdle(..),
                                                                 ClientStTxs(..),
                                                                 ClientStTxIds(..),
                                                                 TxSubmissionClient(..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (BlockingReplyList(..),
                                                               TokBlockingStyle(..))

txSubmissionClient
  :: forall m block txid tx .
     ( MonadSTM m
     , txid ~ GenTxId block
     , tx ~ GenTx block)
  => TMVar m (RPCTxSubmission m txid tx)
  -> TxSubmissionClient txid tx m ()
txSubmissionClient tmvReq =
  TxSubmissionClient $ pure client
 where
  client = ClientStIdle
    { recvMsgRequestTxIds = \blocking acked window ->
        case blocking of
          TokBlocking -> do -- prompt reply not required
            resp' <- newEmptyTMVarM
            atomically . putTMVar tmvReq $
              RPCRequestTxIds (acked, window) resp'
            -- might be some delay at this point
            r <- atomically $ takeTMVar resp'
            pure $
              case r of
                Nothing  -> SendMsgDone ()
                Just txs -> SendMsgReplyTxIds (BlockingReply $ fromList txs) client
          TokNonBlocking -> do -- prompt reply required
            resp' <- newEmptyTMVarM
            atomically . putTMVar tmvReq $
              RPCRequestTxIdsPromptly (acked, window) resp'
            txs <- atomically $ takeTMVar resp'
            pure $ SendMsgReplyTxIds (NonBlockingReply txs) client
    , recvMsgRequestTxs = \txids -> do
        resp' <- newEmptyTMVarM
        atomically $ putTMVar tmvReq $ RPCRequestTxs txids resp'
        r <- atomically $ takeTMVar resp'
        pure $ SendMsgReplyTxs r client
    }
