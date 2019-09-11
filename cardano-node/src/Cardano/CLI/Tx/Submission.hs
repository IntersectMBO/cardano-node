{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Tx.Submission (
      handleTxSubmission
    ) where
import           Cardano.Prelude hiding (ByteString, option, threadDelay)
import           Prelude (String)

import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)

import           System.Directory (canonicalizePath, makeAbsolute)
import           System.FilePath ((</>))

import           Control.Monad (fail)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run

import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Codec.Cbor
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Block (Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Client
                   (chainSyncClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.NodeToClient

import           Cardano.Node.Configuration.Topology
import           Cardano.Common.LocalSocket
import           Cardano.Config.Types (CardanoConfiguration(..))



{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

handleTxSubmission :: forall blk.
                      ( RunDemo blk
                      , Show (ApplyTxErr blk)
                      )
                   => CardanoConfiguration
                   -> Consensus.Protocol blk
                   -> TopologyInfo
                   -> GenTx blk
                   -> Tracer IO String
                   -> IO ()
handleTxSubmission cc ptcl tinfo tx tracer = do
    topoE <- readTopologyFile (topologyFile tinfo)
    NetworkTopology nodeSetups <-
      case topoE of
        Left e  -> fail e
        Right t -> return t

    nid <- case node tinfo of
      CoreId nid -> return nid
      RelayId{}  -> fail "Only core nodes are supported targets"

    let ProtocolInfo{pInfoConfig} =
          protocolInfo (NumCoreNodes (length nodeSetups))
                       (CoreNodeId nid)
                       ptcl

    submitTx cc pInfoConfig (node tinfo) tx tracer


submitTx :: ( RunDemo blk
            , Show (ApplyTxErr blk)
            )
         => CardanoConfiguration
         -> NodeConfig (BlockProtocol blk)
         -> NodeId
         -> GenTx blk
         -> Tracer IO String
         -> IO ()
submitTx CardanoConfiguration{ccSocketPath} pInfoConfig nodeId tx tracer = do
    socketDir <- canonicalizePath =<< makeAbsolute ccSocketPath
    let addr = localSocketAddrInfo (socketDir </> localSocketFilePath nodeId)
    connectTo
      nullTracer
      (,)
      (localInitiatorNetworkApplication tracer pInfoConfig tx)
      Nothing
      addr

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunDemo blk
     , MonadST m
     , MonadThrow m
     , MonadTimer m
     , Show (ApplyTxErr blk)
     )
  => Tracer m String
  -> NodeConfig (BlockProtocol blk)
  -> GenTx blk
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication 'InitiatorApp peer NodeToClientProtocols
                                    m ByteString () Void)
localInitiatorNetworkApplication tracer pInfoConfig tx =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = 0 })
      (DictVersion nodeToClientCodecCBORTerm)

  $ OuroborosInitiatorApplication $ \peer ptcl -> case ptcl of
      LocalTxSubmissionPtcl -> \channel -> do
        traceWith tracer ("Submitting transaction: " {-++ show tx-})
        result <- runPeer
                    nullTracer -- (contramap show tracer)
                    localTxSubmissionCodec
                    peer
                    channel
                    (localTxSubmissionClientPeer
                       (txSubmissionClientSingle tx))
        case result of
          Nothing  -> traceWith tracer "Transaction accepted"
          Just msg -> traceWith tracer ("Transaction rejected: " ++ show msg)

      ChainSyncWithBlocksPtcl -> \channel ->
        runPeer
          nullTracer
          (localChainSyncCodec @blk pInfoConfig)
          peer
          channel
          (chainSyncClientPeer chainSyncClientNull)


-- | A 'LocalTxSubmissionClient' that submits exactly one transaction, and then
-- disconnects, returning the confirmation or rejection.
--
txSubmissionClientSingle
  :: forall tx reject m.
     Applicative m
  => tx
  -> LocalTxSubmissionClient tx reject m (Maybe reject)
txSubmissionClientSingle tx = LocalTxSubmissionClient $ do
    pure $ SendMsgSubmitTx tx $ \mreject ->
      pure (SendMsgDone mreject)

localTxSubmissionCodec
  :: forall m blk . (RunDemo blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))
           DeserialiseFailure m ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    nodeEncodeGenTx
    nodeDecodeGenTx
    (nodeEncodeApplyTxError (Proxy @blk))
    (nodeDecodeApplyTxError (Proxy @blk))

localChainSyncCodec
  :: forall blk m. (RunDemo blk, MonadST m)
  => NodeConfig (BlockProtocol blk)
  -> Codec (ChainSync blk (Point blk))
           DeserialiseFailure m ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (nodeEncodeBlock pInfoConfig)
      (nodeDecodeBlock pInfoConfig)
      (Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
      (Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
