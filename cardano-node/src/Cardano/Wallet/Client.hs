{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Wallet.Client
  (runWalletClient)
where

import           Cardano.Prelude hiding (ByteString, atomically)
import           Prelude (String)

import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Cardano

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Block (Tip)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.NodeToClient
import           Ouroboros.Network.Snocket (socketSnocket)

import           Cardano.Common.LocalSocket
import           Cardano.Config.Types (SocketPath)
import           Cardano.Tracing.Tracers (TraceConstraints)

runWalletClient :: forall blk.
                   ( RunNode blk
                   , TraceConstraints blk
                   )
                => Protocol blk (BlockProtocol blk)
                -> SocketPath
                -> Tracer IO String
                -> IO ()
runWalletClient ptcl sockFp tracer = withIOManager $ \iocp -> do

    path <- localSocketPath sockFp

    let ProtocolInfo { pInfoConfig = cfg } = protocolInfo ptcl

        chainSyncTracer = contramap show tracer
        localTxSubmissionTracer = contramap show tracer

    connectTo
      (socketSnocket iocp)
      NetworkConnectTracers {
          nctMuxTracer       = nullTracer,
          nctHandshakeTracer = nullTracer
        }
      (localInitiatorNetworkApplication
        (Proxy :: Proxy blk)
        chainSyncTracer
        localTxSubmissionTracer
        cfg)
      path

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , MonadST m
     , MonadThrow m
     , MonadTimer m
     )
  -- TODO: the need of a 'Proxy' is an evidence that blk type is not really
  -- needed here.  The wallet client should use some concrete type of block
  -- from 'cardano-chain'.  This should remove the dependency of this module
  -- from 'ouroboros-consensus'.
  => Proxy blk
  -> Tracer m (TraceSendRecv (ChainSync blk (Tip blk)))
  -- ^ tracer which logs all chain-sync messages send and received by the client
  -- (see 'Ouroboros.Network.Protocol.ChainSync.Type' in 'ouroboros-network'
  -- package)
  -> Tracer m (TraceSendRecv (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> TopLevelConfig blk
  -> Versions NodeToClientVersion DictVersion
              (peer -> OuroborosApplication InitiatorApp ByteString m Void Void)
localInitiatorNetworkApplication Proxy chainSyncTracer localTxSubmissionTracer cfg =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = nodeNetworkMagic (Proxy @blk) cfg })
      (DictVersion nodeToClientCodecCBORTerm) $ \_peerid ->

    nodeToClientProtocols $
      NodeToClientProtocols
        { localChainSyncProtocol =(InitiatorProtocolOnly $
           MuxPeerRaw $ \channel -> do
             txv <- newEmptyTMVarM @_ @(GenTx blk)
             runPeer
               localTxSubmissionTracer
               localTxSubmissionCodec
               channel
               (localTxSubmissionClientPeer
                   (txSubmissionClient @(GenTx blk) txv)))
        , localTxSubmissionProtocol =(InitiatorProtocolOnly $
           MuxPeer
             chainSyncTracer
             (localChainSyncCodec @blk cfg)
             (chainSyncClientPeer chainSyncClient))
        }

-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'TMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: forall tx reject m.
     MonadSTM m
  => TMVar m tx
  -> LocalTxSubmissionClient tx reject m Void
txSubmissionClient txv = LocalTxSubmissionClient go
  where
    go :: m (LocalTxClientStIdle tx reject m Void)
    go = do
      tx <- atomically $ readTMVar txv
      pure $ SendMsgSubmitTx tx $ \_ -> go


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
  => ChainSyncClient blk (Tip blk) m Void
chainSyncClient = ChainSyncClient $ pure $
    -- Notify the core node about the our latest points at which we are
    -- synchronised.  This client is not persistent and thus it just
    -- synchronises from the genesis block.  A real implementation should send
    -- a list of points up to a point which is k blocks deep.
    SendMsgFindIntersect
      [Block.genesisPoint]
      ClientStIntersect {
        recvMsgIntersectFound    = \_ _ -> ChainSyncClient (pure clientStIdle),
        recvMsgIntersectNotFound = \  _ -> ChainSyncClient (pure clientStIdle)
      }
  where
    clientStIdle :: ClientStIdle blk (Tip blk) m Void
    clientStIdle =
      SendMsgRequestNext clientStNext (pure clientStNext)

    clientStNext :: ClientStNext blk (Tip blk) m Void
    clientStNext = ClientStNext {
        recvMsgRollForward = \_blk _tip -> ChainSyncClient $ do
          pure clientStIdle
      , recvMsgRollBackward = \_point _tip -> ChainSyncClient $ do
          -- we are requested to roll backward to point '_point', the core
          -- node's chain's tip is '_tip'.
          pure clientStIdle
      }


localTxSubmissionCodec
  :: forall m blk . (RunNode blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))
           DeserialiseFailure m ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    nodeEncodeGenTx
    nodeDecodeGenTx
    (nodeEncodeApplyTxError (Proxy @blk))
    (nodeDecodeApplyTxError (Proxy @blk))

localChainSyncCodec
  :: forall blk m. (RunNode blk, MonadST m)
  => TopLevelConfig blk
  -> Codec (ChainSync blk (Tip blk))
           DeserialiseFailure m ByteString
localChainSyncCodec cfg =
    codecChainSync
      (Block.wrapCBORinCBOR   (nodeEncodeBlock cfg))
      (Block.unwrapCBORinCBOR (nodeDecodeBlock cfg))
      (Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
      (Block.encodeTip (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodeTip (nodeDecodeHeaderHash (Proxy @blk)))
