{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module WalletClient where

import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))

import qualified Codec.Serialise as Serialise (encode, decode)
import           Network.Socket as Socket

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Block (BlockProtocol)

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.Cbor
import           Network.TypedProtocol.Driver
import           Network.Mux.Interface
import           Ouroboros.Network.Block (Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisPoint)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.NodeToClient

runWalletClient :: forall blk.
                   RunDemo blk
                => DemoProtocol blk
                -> CoreNodeId
                -> NumCoreNodes
                -> Tracer IO String
                -> IO ()
runWalletClient ptcl nid numCoreNodes tracer = do

    let ProtocolInfo{pInfoConfig} =
          protocolInfo numCoreNodes
                       nid
                       ptcl

        addr = localSocketAddrInfo (localSocketFilePath nid)

        chainSyncTracer = contramap show tracer
        localTxSubmissionTracer = contramap show tracer

    connectTo
      (muxLocalInitiatorNetworkApplication
        (Proxy :: Proxy blk)
        chainSyncTracer
        localTxSubmissionTracer
        pInfoConfig)
      Nothing
      addr

muxLocalInitiatorNetworkApplication
  :: forall blk m.
     (RunDemo blk, MonadST m, MonadThrow m, MonadTimer m)
  -- TODO: the need of a 'Proxy' is an evidence that blk type is not really
  -- needed here.  The wallet client should use some concrete type of block
  -- from 'cardano-chain'.  This should remove the dependency of this module
  -- from 'ouroboros-consensus'.
  => Proxy blk
  -> Tracer m (TraceSendRecv (ChainSync blk (Point blk)) DeserialiseFailure)
  -- ^ tracer which logs all chain-sync messages send and received by the client
  -- (see 'Ouroboros.Network.Protocol.ChainSync.Type' in 'ouroboros-network'
  -- package)
  -> Tracer m (TraceSendRecv (LocalTxSubmission (GenTx blk) String) DeserialiseFailure)
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> NodeConfig (BlockProtocol blk)
  -> Versions NodeToClientVersion DictVersion
              (MuxApplication InitiatorApp NodeToClientProtocols
                              m ByteString Void Void)
muxLocalInitiatorNetworkApplication Proxy chainSyncTracer localTxSubmissionTracer pInfoConfig =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = 0 })
      (DictVersion nodeToClientCodecCBORTerm)

  $ MuxInitiatorApplication $ \ptcl -> case ptcl of
      LocalTxSubmissionPtcl -> \channel -> do
        txv <- newEmptyTMVarM @_ @(GenTx blk)
        runPeer
          localTxSubmissionTracer
          localTxSubmissionCodec
          channel
          (localTxSubmissionClientPeer
              (txSubmissionClient @(GenTx blk) txv))

      ChainSyncWithBlocksPtcl -> \channel ->
        runPeer
          chainSyncTracer
          (localChainSyncCodec @blk pInfoConfig)
          channel
          (chainSyncClientPeer chainSyncClient)


-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'TMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: forall tx reject m.
     ( Monad    m
     , MonadSTM m
     )
  => TMVar m tx
  -> m (LocalTxSubmissionClient tx reject m Void)
txSubmissionClient txv = do
    tx <- atomically $ readTMVar txv
    pure $ SendMsgSubmitTx tx $ \mbreject -> do
      case mbreject of
        Nothing -> return ()
        Just _r -> return ()
      txSubmissionClient txv


-- | 'ChainSyncClient' which traces received blocks and ignores when it
-- receives a request to rollbackwar.  A real wallet client should:
--
--  * at startup send the list of points of the chain to help synchronise with
--    the node;
--  * update its state when the client receives next block or is requested to
--    rollback, see 'clientStNext' below.
--
chainSyncClient
  :: forall blk m. MonadTimer m
  => ChainSyncClient blk (Point blk) m Void
chainSyncClient = ChainSyncClient $ pure $
    -- Notify the core node about the our latest points at which we are
    -- synchronised.  This client is not persistent and thus it just
    -- synchronises from the genesis block.  A real implementation should send
    -- a list of points up to a point which is k blocks deep.
    SendMsgFindIntersect
      [genesisPoint]
      ClientStIntersect {
        recvMsgIntersectImproved  = \_ _ -> ChainSyncClient (pure clientStIdle), 
        recvMsgIntersectUnchanged = \  _ -> ChainSyncClient (pure clientStIdle)
      }
  where
    clientStIdle :: ClientStIdle blk (Point blk) m Void
    clientStIdle = 
      SendMsgRequestNext clientStNext (pure clientStNext)

    clientStNext :: ClientStNext blk (Point blk) m Void
    clientStNext = ClientStNext {
        recvMsgRollForward = \_blk _tip -> ChainSyncClient $ do
          pure clientStIdle
      , recvMsgRollBackward = \_point _tip -> ChainSyncClient $ do
          -- we are requested to roll backward to point '_point', the core
          -- node's chain's tip is '_tip'.
          pure clientStIdle
      }


localTxSubmissionCodec
  :: (RunDemo blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) String)
           DeserialiseFailure m ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    demoEncodeGenTx
    demoDecodeGenTx
    Serialise.encode
    Serialise.decode

localChainSyncCodec
  :: (RunDemo blk, MonadST m)
  => NodeConfig (BlockProtocol blk)
  -> Codec (ChainSync blk (Point blk))
           DeserialiseFailure m ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (demoEncodeBlock pInfoConfig)
      (demoDecodeBlock pInfoConfig)
      (Block.encodePoint demoEncodeHeaderHash)
      (Block.decodePoint demoDecodeHeaderHash)


-- | Local unix socket file path over which the client communicates with a core
-- node.
--
localSocketFilePath :: CoreNodeId -> FilePath
localSocketFilePath (CoreNodeId  n) = "node-core-" ++ show n ++ ".socket"

localSocketAddrInfo :: FilePath -> Socket.AddrInfo
localSocketAddrInfo socketPath =
    Socket.AddrInfo
      []
      Socket.AF_UNIX
      Socket.Stream
      Socket.defaultProtocol
      (Socket.SockAddrUnix socketPath)
      Nothing
