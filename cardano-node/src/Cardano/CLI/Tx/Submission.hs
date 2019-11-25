{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Tx.Submission (
      handleTxSubmission
    , submitTx
    ) where
import           Cardano.Prelude hiding (ByteString, option, threadDelay)
import           Prelude (String)

import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)

import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadThrow)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Tracer (Tracer, nullTracer, traceWith)

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Mempool (ApplyTxErr, GenTx)
import           Ouroboros.Consensus.Node.ProtocolInfo ( ProtocolInfo(..)
                                                       , protocolInfo)
import           Ouroboros.Consensus.Node.Run (RunNode)
import qualified Ouroboros.Consensus.Node.Run as Node
import           Ouroboros.Consensus.NodeId (NodeId(..))
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Protocol hiding (Protocol)

import           Network.TypedProtocol.Driver (runPeer)
import           Network.TypedProtocol.Codec.Cbor (Codec, DeserialiseFailure)
import           Ouroboros.Network.Mux (AppType(..), OuroborosApplication(..))
import           Ouroboros.Network.Block (Point)
import qualified Ouroboros.Network.Block as Block
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as LocalTxSub
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Codec as LocalTxSub
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import           Ouroboros.Network.Protocol.Handshake.Version ( Versions
                                                              , simpleSingletonVersions)
import           Ouroboros.Network.NodeToClient (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToClient as NodeToClient

import           Cardano.Config.Topology
import           Cardano.Common.LocalSocket
import           Cardano.Config.Types (MiscellaneousFilepaths(..),
                                       NodeCLI(..),  SocketFile(..))




{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

-- | For a given protocol and configuration, submit the given GenTx to the node
-- specified by topology info, while using tracer for logging.
handleTxSubmission :: forall blk.
                      ( RunNode blk
                      , Show (ApplyTxErr blk)
                      )
                   => NodeCLI
                   -> Consensus.Protocol blk
                   -> TopologyInfo
                   -> GenTx blk
                   -> Tracer IO String
                   -> IO ()
handleTxSubmission nCli ptcl tinfo tx tracer = do
    let pinfo :: ProtocolInfo blk
        pinfo = protocolInfo ptcl

    submitTx nCli (pInfoConfig pinfo) (node tinfo) tx tracer


submitTx :: ( RunNode blk
            , Show (ApplyTxErr blk)
            )
         => NodeCLI
         -> NodeConfig (BlockProtocol blk)
         -> NodeId
         -> GenTx blk
         -> Tracer IO String
         -> IO ()
submitTx nCli protoInfoConfig nId tx tracer = do
    socketPath <- localSocketAddrInfo nId (unSocket . socketFile $ mscFp nCli) NoMkdirIfMissing
    NodeToClient.connectTo
      NetworkConnectTracers {
          nctMuxTracer       = nullTracer,
          nctHandshakeTracer = nullTracer
        }
      (localInitiatorNetworkApplication tracer protoInfoConfig tx)
      Nothing
      socketPath

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , MonadST m
     , MonadThrow m
     , MonadTimer m
     , Show (ApplyTxErr blk)
     )
  => Tracer m String
  -> NodeConfig (BlockProtocol blk)
  -> GenTx blk
  -> Versions NodeToClient.NodeToClientVersion NodeToClient.DictVersion
              (OuroborosApplication 'InitiatorApp peer NodeToClient.NodeToClientProtocols
                                    m ByteString () Void)
localInitiatorNetworkApplication tracer protoInfoConfig tx =
    simpleSingletonVersions
      NodeToClient.NodeToClientV_1
      (NodeToClient.NodeToClientVersionData
        { NodeToClient.networkMagic = Node.nodeNetworkMagic (Proxy @blk) protoInfoConfig })
      (NodeToClient.DictVersion NodeToClient.nodeToClientCodecCBORTerm)

  $ OuroborosInitiatorApplication $ \peer ptcl -> case ptcl of
      NodeToClient.LocalTxSubmissionPtcl -> \channel -> do
        traceWith tracer ("Submitting transaction: " {-++ show tx-})
        result <- runPeer
                    nullTracer -- (contramap show tracer)
                    localTxSubmissionCodec
                    peer
                    channel
                    (LocalTxSub.localTxSubmissionClientPeer
                       (txSubmissionClientSingle tx))
        case result of
          Nothing  -> traceWith tracer "Transaction accepted"
          Just msg -> traceWith tracer ("Transaction rejected: " ++ show msg)

      NodeToClient.ChainSyncWithBlocksPtcl -> \channel ->
        runPeer
          nullTracer
          (localChainSyncCodec @blk protoInfoConfig)
          peer
          channel
          (chainSyncClientPeer NodeToClient.chainSyncClientNull)

-- | A 'LocalTxSubmissionClient' that submits exactly one transaction, and then
-- disconnects, returning the confirmation or rejection.
--
txSubmissionClientSingle
  :: forall tx reject m.
     Applicative m
  => tx
  -> LocalTxSub.LocalTxSubmissionClient tx reject m (Maybe reject)
txSubmissionClientSingle tx = LocalTxSub.LocalTxSubmissionClient $ do
    pure $ LocalTxSub.SendMsgSubmitTx tx $ \mreject ->
      pure (LocalTxSub.SendMsgDone mreject)

localTxSubmissionCodec
  :: forall m blk . (RunNode blk, MonadST m)
  => Codec (LocalTxSub.LocalTxSubmission (GenTx blk) (ApplyTxErr blk))
           DeserialiseFailure m ByteString
localTxSubmissionCodec =
  LocalTxSub.codecLocalTxSubmission
    Node.nodeEncodeGenTx
    Node.nodeDecodeGenTx
    (Node.nodeEncodeApplyTxError (Proxy @blk))
    (Node.nodeDecodeApplyTxError (Proxy @blk))

localChainSyncCodec
  :: forall blk m. (RunNode blk, MonadST m)
  => NodeConfig (BlockProtocol blk)
  -> Codec (ChainSync blk (Point blk))
           DeserialiseFailure m ByteString
localChainSyncCodec protoInfoConfig =
    codecChainSync
      (Node.nodeEncodeBlock protoInfoConfig)
      (Node.nodeDecodeBlock protoInfoConfig)
      (Block.encodePoint (Node.nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (Node.nodeDecodeHeaderHash (Proxy @blk)))
      (Block.encodePoint (Node.nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (Node.nodeDecodeHeaderHash (Proxy @blk)))
