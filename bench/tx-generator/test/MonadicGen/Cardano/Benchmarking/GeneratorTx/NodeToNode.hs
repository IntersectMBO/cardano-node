{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-unticked-promoted-constructors -Wno-all-missed-specialisations #-}

module MonadicGen.Cardano.Benchmarking.GeneratorTx.NodeToNode
  ( ConnectClient
  , benchmarkConnectTxSubmit
  ) where

import           Cardano.Prelude (forever, liftIO)
import           Prelude

import           Codec.Serialise (DeserialiseFailure)
import           Control.Concurrent.Class.MonadSTM.Strict (newTVarIO)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Data.Foldable (fold)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)
import           Network.Socket (AddrInfo (..))
import           System.Random (newStdGen)

import           "contra-tracer" Control.Tracer (Tracer, nullTracer)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Byron.Ledger.Mempool (GenTx)
import qualified Ouroboros.Consensus.Cardano as Consensus (CardanoBlock)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..), defaultCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

import           Ouroboros.Network.Channel (Channel (..))
import           Ouroboros.Network.ControlMessage (continueForever)
import           Ouroboros.Network.DeltaQ (defaultGSV)
import           Ouroboros.Network.Driver (runPeer, runPeerWithLimits)
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Mux (MiniProtocolCb (..), OuroborosApplication (..), OuroborosBundle,
                   RunMiniProtocol (..), MuxMode (..))
import           Ouroboros.Network.NodeToClient (IOManager, chainSyncPeerNull)
import           Ouroboros.Network.NodeToNode (micConnectionId, remoteAddress, MinimalInitiatorContext, ResponderContext (..), NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient (..),
                   blockFetchClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import           Ouroboros.Network.Protocol.KeepAlive.Client hiding (SendMsgDone)
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.TxSubmission2.Client (TxSubmissionClient,
                   txSubmissionClientPeer)
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..), encodeRemoteAddress, decodeRemoteAddress)
import           Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..),
                   peerSharingClientPeer)

import           Ouroboros.Network.Snocket (socketSnocket)

import           MonadicGen.Cardano.Benchmarking.LogTypes (SendRecvConnect, SendRecvTxSubmission2)

type CardanoBlock    = Consensus.CardanoBlock  StandardCrypto
type ConnectClient = AddrInfo -> TxSubmissionClient (GenTxId CardanoBlock) (GenTx CardanoBlock) IO () -> IO ()

benchmarkConnectTxSubmit
  :: forall blk. (blk ~ CardanoBlock, RunNode blk )
  => IOManager
  -> Tracer IO SendRecvConnect
  -> Tracer IO SendRecvTxSubmission2
  -> CodecConfig CardanoBlock
  -> NetworkMagic
  -> AddrInfo
  -- ^ remote address information
  -> TxSubmissionClient (GenTxId blk) (GenTx blk) IO ()
  -- ^ the particular txSubmission peer
  -> IO ()

benchmarkConnectTxSubmit ioManager handshakeTracer submissionTracer codecConfig networkMagic remoteAddr myTxSubClient =
  NtN.connectTo
    (socketSnocket ioManager)
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = handshakeTracer
      }
    peerMultiplex
    (addrAddress <$> Nothing)
    (addrAddress remoteAddr)
 where
  ownPeerSharing = NoPeerSharing
  mkApp :: OuroborosBundle      mode initiatorCtx responderCtx bs m a b
        -> OuroborosApplication mode initiatorCtx responderCtx bs m a b
  mkApp bundle =
    OuroborosApplication $ fold bundle

  n2nVer :: NodeToNodeVersion
  n2nVer = NodeToNodeV_10
  blkN2nVer :: BlockNodeToNodeVersion blk
  blkN2nVer = supportedVers Map.! n2nVer
  supportedVers :: Map.Map NodeToNodeVersion (BlockNodeToNodeVersion blk)
  supportedVers = supportedNodeToNodeVersions (Proxy @blk)
  myCodecs :: Codecs blk NtN.RemoteAddress DeserialiseFailure IO
                ByteString ByteString ByteString ByteString ByteString ByteString
                ByteString
  myCodecs  = defaultCodecs codecConfig blkN2nVer encodeRemoteAddress decodeRemoteAddress n2nVer
  peerMultiplex :: NtN.Versions NodeToNodeVersion
                                NtN.NodeToNodeVersionData
                                (OuroborosApplication
                                  'InitiatorMode
                                  (MinimalInitiatorContext NtN.RemoteAddress)
                                  (ResponderContext NtN.RemoteAddress)
                                  ByteString IO () Void)
  peerMultiplex =
    simpleSingletonVersions
      n2nVer
      (NtN.NodeToNodeVersionData
       { NtN.networkMagic = networkMagic
       , NtN.diffusionMode = NtN.InitiatorOnlyDiffusionMode
       , NtN.peerSharing = ownPeerSharing
       , NtN.query = False
       }) $
      mkApp $
      NtN.nodeToNodeProtocols NtN.defaultMiniProtocolParameters
        NtN.NodeToNodeProtocols
          { NtN.chainSyncProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \ctx channel ->
                                      runPeer
                                        mempty
                                        (cChainSyncCodec myCodecs)
                                        channel
                                        chainSyncPeerNull
          , NtN.blockFetchProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \ctx channel ->
                                       runPeer
                                         mempty
                                         (cBlockFetchCodec myCodecs)
                                         channel
                                         (blockFetchClientPeer blockFetchClientNull)
          , NtN.keepAliveProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \ctx channel ->
                                        kaClient n2nVer (remoteAddress $ micConnectionId ctx) channel
          , NtN.txSubmissionProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \ctx channel ->
                                         runPeer
                                           submissionTracer
                                           (cTxSubmission2Codec myCodecs)
                                           channel
                                           (txSubmissionClientPeer myTxSubClient)
          , NtN.peerSharingProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \ctx channel ->
                                         runPeer
                                           mempty
                                           (cPeerSharingCodec myCodecs)
                                           channel
                                           (peerSharingClientPeer peerSharingClientNull)
          }
        n2nVer
        ownPeerSharing
  -- Stolen from: Ouroboros/Consensus/Network/NodeToNode.hs
  kaClient
    :: Ord remotePeer
    => NodeToNodeVersion
    -> remotePeer
    -> Channel IO ByteString
    -> IO ((), Maybe ByteString)
  kaClient _version them channel = do
    keepAliveRng <- newStdGen
    peerGSVMap <- liftIO . newTVarIO $ Map.singleton them defaultGSV
    runPeerWithLimits
      nullTracer
      (cKeepAliveCodec myCodecs)
      (byteLimitsKeepAlive (const 0)) -- TODO: Real Bytelimits, see #1727
      timeLimitsKeepAlive
      channel
      $ keepAliveClientPeer
      $ keepAliveClient
          nullTracer
          keepAliveRng
          (continueForever (Proxy :: Proxy IO)) them peerGSVMap
          (KeepAliveInterval 10)

-- the null block fetch client
blockFetchClientNull
  :: forall block point m a.  MonadTimer m
  => BlockFetchClient block point m a
blockFetchClientNull
  = BlockFetchClient $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}

-- the null peer sharing client
peerSharingClientNull
  :: forall addr m a. MonadTimer m
  => PeerSharingClient addr m a
peerSharingClientNull = SendMsgDone $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}
