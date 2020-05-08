{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.LocalStateQuery
  ( queryFilteredUTxOFromLocalState
  , queryPParamsFromLocalState
  ) where

import           Cardano.Prelude hiding (atomically, option, threadDelay)

import           Cardano.Api.TxSubmit.Types (textShow)

import           Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import           Cardano.BM.Trace (Trace, appendName, logInfo)

import           Cardano.Config.Protocol ()
import           Cardano.Config.Types (SocketPath (..))

import qualified Codec.CBOR.Term as CBOR

import           Control.Monad.Class.MonadSTM.Strict
                   (MonadSTM, StrictTMVar, atomically, newEmptyTMVarM, tryPutTMVar, takeTMVar)
import           Control.Monad.Trans.Except.Extra (newExceptT)
import           Control.Tracer (Tracer)

import           Data.Functor.Contravariant (contramap)
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)

import           Network.Mux (MuxTrace, WithMuxBearer)

import           Ouroboros.Consensus.Ledger.Abstract (Query)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Network.Block (Point)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux
                   ( AppType(..), OuroborosApplication(..),
                     MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.NodeToClient (ConnectionId, DictVersion, Handshake,
                   LocalAddress, LocalConnectionId, NetworkConnectTracers (..),
                   NodeToClientProtocols (..), NodeToClientVersion, NodeToClientVersionData (..),
                   TraceSendRecv, Versions)
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
                   (ClientStAcquired (..), ClientStAcquiring (..), ClientStIdle (..),
                    ClientStQuerying (..), LocalStateQueryClient(..), localStateQueryClientPeer)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))

import qualified Shelley.Spec.Ledger.PParams as Ledger (PParams)
import qualified Shelley.Spec.Ledger.TxData as Ledger (Addr)
import qualified Shelley.Spec.Ledger.UTxO as Ledger (UTxO)

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryFilteredUTxOFromLocalState
  :: (RunNode blk, blk ~ ShelleyBlock c)
  => CodecConfig blk
  -> NetworkMagic
  -> SocketPath
  -> Set (Ledger.Addr c)
  -> Point blk
  -> ExceptT AcquireFailure IO (Ledger.UTxO c)
queryFilteredUTxOFromLocalState cfg nm socketPath addrs point = do
  let pointAndQuery = (point, GetFilteredUTxO addrs)
  utxoVar <- liftIO newEmptyTMVarM
  liftIO $ queryNodeLocalState
    utxoVar
    nullTracer
    cfg nm
    socketPath
    pointAndQuery
  newExceptT $ atomically $ takeTMVar utxoVar

-- | Query the current protocol parameters from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryPParamsFromLocalState
  :: (RunNode blk, blk ~ ShelleyBlock c)
  => CodecConfig blk
  -> NetworkMagic
  -> SocketPath
  -> Point blk
  -> ExceptT AcquireFailure IO Ledger.PParams
queryPParamsFromLocalState cfg nm socketPath point = do
  let pointAndQuery = (point, GetCurrentPParams)
  pParamsVar <- liftIO newEmptyTMVarM
  liftIO $ queryNodeLocalState
    pParamsVar
    nullTracer
    cfg nm
    socketPath
    pointAndQuery
  newExceptT $ atomically $ takeTMVar pParamsVar

-- | Establish a connection to a node and execute the provided query
-- via the local state query protocol.
--
-- This one is not specific to any era.
--
queryNodeLocalState
  :: forall blk result. RunNode blk
  => StrictTMVar IO (Either AcquireFailure result)
  -> Trace IO Text
  -> CodecConfig blk
  -> NetworkMagic
  -> SocketPath
  -> (Point blk, Query blk result)
  -> IO ()
queryNodeLocalState resultVar trce cfg nm (SocketFile socketPath) pointAndQuery = do
    logInfo trce $ "queryNodeLocalState: Connecting to node via " <> textShow socketPath
    NodeToClient.withIOManager $ \iocp -> do
      NodeToClient.connectTo
        (NodeToClient.localSnocket iocp socketPath)
        NetworkConnectTracers
          { nctMuxTracer = muxTracer
          , nctHandshakeTracer = handshakeTracer
          }
        (localInitiatorNetworkApplication trce cfg nm resultVar pointAndQuery)
        socketPath
  where
    muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    handshakeTracer :: Tracer IO
                        (WithMuxBearer (ConnectionId LocalAddress)
                        (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce

localInitiatorNetworkApplication
  :: forall blk result. RunNode blk
  => Trace IO Text
  -> CodecConfig blk
  -> NetworkMagic
  -> StrictTMVar IO (Either AcquireFailure result)
  -> (Point blk, Query blk result)
  -> Versions
      NodeToClientVersion
      DictVersion
      (LocalConnectionId
        -> OuroborosApplication 'InitiatorApp LByteString IO (Either AcquireFailure result) Void)
localInitiatorNetworkApplication trce cfg networkMagic
                                 resultVar pointAndQuery =
    NodeToClient.foldMapVersions
      (\v ->
        NodeToClient.versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (protocols v))
      (supportedNodeToClientVersions proxy)
  where
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NodeToClientVersionData { networkMagic }

    protocols clientVersion =
        NodeToClientProtocols
          { localChainSyncProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cChainSyncCodec
                  NodeToClient.chainSyncPeerNull

          , localTxSubmissionProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cTxSubmissionCodec
                  NodeToClient.localTxSubmissionPeerNull

          , localStateQueryProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  (contramap (Text.pack . show) . toLogObject $
                    appendName "cardano-local-state-query" trce)
                  cStateQueryCodec
                  (localStateQueryClientPeer (localStateQueryClient pointAndQuery resultVar))
          }
      where
        Codecs
          { cChainSyncCodec
          , cTxSubmissionCodec
          , cStateQueryCodec
          } = defaultCodecs cfg clientVersion

-- | A 'LocalStateQueryClient' which executes the provided local state query
-- and puts the result in the provided 'StrictTMVar'.
localStateQueryClient
  :: forall block query m result. (Applicative m, MonadIO m, MonadSTM m)
  => (Point block, query result)
  -> StrictTMVar m (Either AcquireFailure result)
  -> LocalStateQueryClient block query m (Either AcquireFailure result)
localStateQueryClient (point, query) resultVar =
  LocalStateQueryClient $ pure $ SendMsgAcquire point $
    ClientStAcquiring
      { recvMsgAcquired = SendMsgQuery query $
          ClientStQuerying
            { recvMsgResult = \result -> do
                void $ atomically $ tryPutTMVar resultVar (Right result)
                pure $ SendMsgRelease $ SendMsgDone (Right result)
            }
      , recvMsgFailure = \failure -> do
          void $ atomically $ tryPutTMVar resultVar (Left failure)
          pure $ SendMsgDone (Left failure)
      }
