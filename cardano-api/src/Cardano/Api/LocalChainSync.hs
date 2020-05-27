{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LocalChainSync
  ( getLocalTip
  ) where

import           Cardano.Prelude hiding (atomically, catch)

import qualified Data.ByteString.Lazy as LBS

import           Cardano.Api.Types (Network(..), toNetworkMagic)

import           Cardano.Config.Types (SocketPath (..))

import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM.Strict
                   (MonadSTM, StrictTMVar, atomically, newEmptyTMVarM, tryPutTMVar, takeTMVar)
import           Control.Monad.Class.MonadThrow (catch)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Tracer (nullTracer)

import           Network.Mux (MuxError)

import           Ouroboros.Consensus.Cardano (CodecConfig)
import           Ouroboros.Consensus.Network.NodeToClient
                   (Codecs'(..), defaultCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run
                   (RunNode(..))

import           Ouroboros.Network.Block (Serialised, Tip)
import           Ouroboros.Network.Mux
                   (AppType(InitiatorApp), OuroborosApplication(..),
                    MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.NodeToClient
                   (IOManager, LocalAddress, NetworkConnectTracers(..),
                    NodeToClientProtocols(..), NodeToClientVersionData(..),
                    NodeToClientVersion, connectTo, localSnocket,
                    localStateQueryPeerNull, localTxSubmissionPeerNull,
                    versionedNodeToClientProtocols, foldMapVersions)
import           Ouroboros.Network.Protocol.ChainSync.Client
                   (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..)
                   , chainSyncClientPeer, recvMsgRollForward)
import           Ouroboros.Network.Protocol.Handshake.Version
                   (DictVersion(..), Versions)

-- | Get the node's tip using the local chain sync protocol.
getLocalTip
  :: forall blk . RunNode blk
  => IOManager
  -> CodecConfig blk
  -> Network
  -> SocketPath
  -> IO (Tip blk)
getLocalTip iomgr cfg nm sockPath = do
  tipVar <- newEmptyTMVarM
  createNodeConnection iomgr cfg nm sockPath tipVar
  atomically $ takeTMVar tipVar

createNodeConnection
  :: forall blk . RunNode blk
  => IOManager
  -> CodecConfig blk
  -> Network
  -> SocketPath
  -> StrictTMVar IO (Tip blk)
  -> IO ()
createNodeConnection iomgr cfg nm (SocketPath path) tipVar =
    connectTo
      (localSnocket iomgr path)
      (NetworkConnectTracers nullTracer nullTracer)
      (localInitiatorNetworkApplication cfg nm tipVar)
      path
    `catch` handleMuxError

handleMuxError :: MuxError -> IO ()
handleMuxError err = print err

localInitiatorNetworkApplication
  :: forall blk m.
     ( RunNode blk
     , MonadIO m
     , MonadST    m
     , MonadTimer m
     )
  => CodecConfig blk
  -> Network
  -> StrictTMVar m (Tip blk)
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication 'InitiatorApp LocalAddress LBS.ByteString m () Void)
localInitiatorNetworkApplication cfg nm tipVar =
    foldMapVersions
      (\v ->
        versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (const $ protocols v))
      (supportedNodeToClientVersions proxy)
 where
  proxy :: Proxy blk
  proxy = Proxy

  versionData = NodeToClientVersionData { networkMagic = toNetworkMagic nm }

  protocols clientVersion =
      NodeToClientProtocols {
        localChainSyncProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              cChainSyncCodec
              (chainSyncClientPeer (chainSyncClient tipVar))

      , localTxSubmissionProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              cTxSubmissionCodec
              localTxSubmissionPeerNull
      , localStateQueryProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              cStateQueryCodec
              localStateQueryPeerNull
      }
    where
      Codecs { cChainSyncCodec
             , cTxSubmissionCodec
             , cStateQueryCodec
             }
        = defaultCodecs cfg clientVersion

chainSyncClient
  :: forall blk m . (MonadIO m, MonadSTM m)
  => StrictTMVar m (Tip blk)
  -> ChainSyncClient (Serialised blk) (Tip blk) m ()
chainSyncClient tipVar = ChainSyncClient $ pure $
  SendMsgRequestNext
    clientStNext
    (pure $ ClientStNext
              { recvMsgRollForward = \_ _ -> ChainSyncClient $ pure clientStIdle
              , recvMsgRollBackward = \_ _ -> ChainSyncClient $ pure clientStIdle
              }
    )
 where
  clientStIdle :: ClientStIdle (Serialised blk) (Tip blk) m ()
  clientStIdle =
    SendMsgRequestNext clientStNext (pure clientStNext)
  --TODO: we should be able to simply return the tip as the result with
  -- SendMsgDone and collect this as the result of the overall protocol.
  -- While currently we can have protocols return things, the current OuroborosApplication
  -- stuff gets in the way of returning an overall result, but that's being worked on,
  -- and this can be improved when that's ready.
  clientStNext :: ClientStNext (Serialised blk) (Tip blk) m ()
  clientStNext = ClientStNext
    { recvMsgRollForward = \_blk tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    }
