{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Tx.BenchmarkingNodeToNode
  ( BenchmarkTxSubmitTracers (..)
  , SendRecvConnect
  , SendRecvTxSubmission
  , benchmarkConnectTxSubmit
  ) where

import           Prelude
import           Cardano.Prelude (Void, forever)

import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Network.Mux.Interface (AppType(InitiatorApp))
import           Network.Socket (AddrInfo)
import           Network.TypedProtocol.Driver (TraceSendRecv, runPeer)
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)

import           Control.Tracer (Tracer, nullTracer)
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Mempool.API (GenTxId, GenTx)
import           Ouroboros.Consensus.Node.Run (RunNode, nodeNetworkMagic)
import           Ouroboros.Consensus.NodeNetwork (ProtocolCodecs(..), protocolCodecs)
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Network.Mux (OuroborosApplication(..))
import           Ouroboros.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient(..), blockFetchClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull, chainSyncClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (Versions, simpleSingletonVersions)
import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSubmissionClient, txSubmissionClientPeer)
import           Ouroboros.Network.Protocol.TxSubmission.Type as TS (TxSubmission)

type SendRecvConnect = TraceSendRecv (Handshake NtN.NodeToNodeVersion CBOR.Term)
                                     NtN.ConnectionId
                                     (DecoderFailureOrTooMuchInput DeserialiseFailure)

type SendRecvTxSubmission blk = TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))
                                              NtN.ConnectionId
                                              DeserialiseFailure

data BenchmarkTxSubmitTracers m blk = BenchmarkTracers
  { trSendRecvConnect      :: Tracer m SendRecvConnect
  , trSendRecvTxSubmission :: Tracer m (SendRecvTxSubmission blk)
  }

benchmarkConnectTxSubmit
  :: forall m blk . (RunNode blk, m ~ IO)
  => BenchmarkTxSubmitTracers m blk
  -- ^ For tracing the send/receive actions
  -> NodeConfig (BlockProtocol blk)
  -- ^ The particular block protocol
  -> Maybe AddrInfo
  -- ^ local address information (typically local interface/port to use)
  -> AddrInfo
  -- ^ remote address information
  -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()
  -- ^ the particular txSubmission peer
  -> m ()
benchmarkConnectTxSubmit trs nc localAddr remoteAddr myTxSubClient = do
  NtN.connectTo
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = trSendRecvConnect trs
      }
    peerMultiplex
    localAddr
    remoteAddr
 where
  myCodecs :: ProtocolCodecs blk DeserialiseFailure m
                ByteString ByteString ByteString ByteString ByteString
  myCodecs  = protocolCodecs nc

  peerMultiplex :: Versions NtN.NodeToNodeVersion NtN.DictVersion
              (OuroborosApplication
                 'InitiatorApp
                 NtN.ConnectionId
                 NtN.NodeToNodeProtocols
                 m
                 ByteString
                 ()
                 Void)
  peerMultiplex =
    simpleSingletonVersions
      NtN.NodeToNodeV_1
      (NtN.NodeToNodeVersionData { NtN.networkMagic = nodeNetworkMagic (Proxy @blk) nc})
      (NtN.DictVersion NtN.nodeToNodeCodecCBORTerm)
      $ OuroborosInitiatorApplication $ \peer ptcl ->
          case ptcl of
            NtN.ChainSyncWithHeadersPtcl -> \channel ->
              runPeer nullTracer (pcChainSyncCodec myCodecs) peer channel
                                 (chainSyncClientPeer chainSyncClientNull)
            NtN.BlockFetchPtcl           -> \channel ->
              runPeer nullTracer (pcBlockFetchCodec myCodecs)  peer channel
                                 (blockFetchClientPeer blockFetchClientNull)
            NtN.TxSubmissionPtcl         -> \channel ->
              runPeer (trSendRecvTxSubmission trs)
                      (pcTxSubmissionCodec myCodecs)
                      peer channel
                      (txSubmissionClientPeer myTxSubClient)

-- the null block fetch client
blockFetchClientNull
  :: forall block m a.  MonadTimer m
  => BlockFetchClient block m a
blockFetchClientNull
  = BlockFetchClient $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}
