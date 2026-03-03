{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.Connection
  ( CardanoBlock
  , ConnectClient
  , connect
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import Data.Proxy (Proxy (..))
import Data.Void (Void, absurd)
----------------
-- bytestring --
----------------
import Data.ByteString.Lazy qualified as BSL
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
----------------
-- io-classes --
----------------
import Control.Monad.Class.MonadTimer qualified as MonadTimer
-------------
-- network --
-------------
import Network.Socket qualified as Socket
-----------------
-- network-mux --
-----------------
import Network.Mux qualified as Mux
--------------------------
-- ouroboros-consensus --
--------------------------
import Ouroboros.Consensus.Block.Abstract qualified as Block
import Ouroboros.Consensus.Cardano qualified as Consensus (CardanoBlock)
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Mempool
import Ouroboros.Consensus.Network.NodeToNode qualified as NetN2N
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as NetVer
import Ouroboros.Consensus.Node.Run ()
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
-- Orphan instances needed for
-- RunNode / SupportedNetworkProtocolVersion CardanoBlock
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
--------------------------
-- ouroboros-network --
--------------------------
import Ouroboros.Network.Channel qualified as Channel
import Ouroboros.Network.Context qualified as NetCtx
import Ouroboros.Network.ControlMessage qualified as ControlMsg
import Ouroboros.Network.DeltaQ qualified as DeltaQ
import Ouroboros.Network.Driver qualified as Driver
import Ouroboros.Network.KeepAlive qualified as KeepAlive
import Ouroboros.Network.Magic qualified as Magic
import Ouroboros.Network.Mux qualified as NetMux
import Ouroboros.Network.NodeToClient qualified as NtC
import Ouroboros.Network.NodeToNode qualified as NtN
import Ouroboros.Network.PeerSelection.PeerSharing qualified as PeerSharing
import Ouroboros.Network.PeerSelection.PeerSharing.Codec qualified as PSCodec
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BFClient
import Ouroboros.Network.Protocol.Handshake.Version qualified as Handshake
import Ouroboros.Network.Protocol.KeepAlive.Client qualified as KAClient
import Ouroboros.Network.Protocol.KeepAlive.Codec qualified as KACodec
import Ouroboros.Network.Protocol.PeerSharing.Client qualified as PSClient
import Ouroboros.Network.Protocol.TxSubmission2.Client qualified as TxSub
import Ouroboros.Network.Snocket qualified as Snocket
-------------------------------
-- ouroboros-network-framework --
-------------------------------
import Ouroboros.Network.IOManager qualified as IOManager
------------
-- random --
------------
import System.Random qualified as Random
---------------
-- serialise --
---------------
import Codec.Serialise qualified as Serialise
---------
-- stm --
---------
import Control.Concurrent.Class.MonadSTM.Strict qualified as StrictSTM
---------------------
-- tx-centrifuge --
---------------------
import Cardano.Benchmarking.TxCentrifuge.Tracing
  qualified as Tracing

--------------------------------------------------------------------------------

type CardanoBlock = Consensus.CardanoBlock Eras.StandardCrypto

-- | A function that connects to a remote node and runs a TxSubmission2 client.
--
-- Returns @Left msg@ when the connection fails (handshake error) or terminates
-- (mux returns). The caller is responsible for adding context (e.g. a
-- workload\/target label) and deciding how to surface the error.
type ConnectClient =
     Socket.AddrInfo
  -> TxSub.TxSubmissionClient
       (Mempool.GenTxId CardanoBlock)
       (Mempool.GenTx CardanoBlock)
       IO
       ()
  -> IO (Either String ())

-- | Connect to a remote cardano-node via NodeToNode protocols.
-- Runs null ChainSync, null BlockFetch, null PeerSharing, KeepAlive, and the
-- given TxSubmission2 client.
--
-- Returns @Left msg@ on handshake failure or unexpected connection termination.
-- The @Right@ case is unreachable (the mux never returns successfully).
connect
  :: IOManager.IOManager
  -> Block.CodecConfig CardanoBlock
  -> Magic.NetworkMagic
  -> Tracing.Tracers
  -> ConnectClient
connect
  ioManager
  codecConfig
  networkMagic
  tracers
  remoteAddr
  myTxSubClient = do
  done <- NtN.connectTo (Snocket.socketSnocket ioManager)
    NtN.NetworkConnectTracers
      { NtN.nctMuxTracers = Mux.nullTracers
      , NtN.nctHandshakeTracer = mempty
      }
    peerMultiplex
    Nothing
    (Socket.addrAddress remoteAddr)
  case done of
    Left err -> pure $ Left $
      "handshake failed: " ++ show err
    Right choice -> case choice of
      Left () -> pure $ Left
        "connection terminated unexpectedly"
      Right void -> absurd void

  where

    n2nVer :: NetVer.NodeToNodeVersion
    n2nVer = NetVer.NodeToNodeV_14

    blkN2nVer :: NetVer.BlockNodeToNodeVersion CardanoBlock
    blkN2nVer = supportedVers Map.! n2nVer

    supportedVers
      :: Map.Map
           NetVer.NodeToNodeVersion
           ( NetVer.BlockNodeToNodeVersion
               CardanoBlock
           )
    supportedVers =
      NetVer.supportedNodeToNodeVersions (Proxy @CardanoBlock)

    myCodecs
      :: NetN2N.Codecs
           CardanoBlock
           NtN.RemoteAddress
           Serialise.DeserialiseFailure
           IO
           BSL.ByteString
           BSL.ByteString
           BSL.ByteString
           BSL.ByteString
           BSL.ByteString
           BSL.ByteString
           BSL.ByteString
    myCodecs =
      NetN2N.defaultCodecs
        codecConfig
        blkN2nVer
        PSCodec.encodeRemoteAddress
        PSCodec.decodeRemoteAddress
        n2nVer

    keepAliveTimeout :: KeepAlive.KeepAliveInterval
    keepAliveTimeout = KeepAlive.KeepAliveInterval 10

    peerMultiplex
      :: NtN.Versions
           NetVer.NodeToNodeVersion
           NtN.NodeToNodeVersionData
           ( NetMux.OuroborosApplication
               'Mux.InitiatorMode
               ( NetCtx.MinimalInitiatorContext
                   NtN.RemoteAddress
               )
               ( NetCtx.ResponderContext
                   NtN.RemoteAddress
               )
               BSL.ByteString
               IO
               ()
               Void
           )
    peerMultiplex =
      Handshake.simpleSingletonVersions
        n2nVer
        ( NtN.NodeToNodeVersionData
            { NtN.networkMagic = networkMagic
            , NtN.diffusionMode = NtN.InitiatorOnlyDiffusionMode
            , NtN.peerSharing = PeerSharing.PeerSharingDisabled
            , NtN.query = False
            }
        )
        $ \n2nData ->
            mkApp $
            NtN.nodeToNodeProtocols
              NtN.defaultMiniProtocolParameters
              NtN.NodeToNodeProtocols
                { NtN.chainSyncProtocol =
                    NetMux.InitiatorProtocolOnly
                    $ NetMux.MiniProtocolCb
                    $ \_ctx channel ->
                        Driver.runPeer
                          mempty
                          ( NetN2N.cChainSyncCodec
                              myCodecs
                          )
                          channel
                          NtC.chainSyncPeerNull

                , NtN.blockFetchProtocol =
                    NetMux.InitiatorProtocolOnly
                    $ NetMux.MiniProtocolCb
                    $ \_ctx channel ->
                        Driver.runPeer
                          mempty
                          ( NetN2N.cBlockFetchCodec
                              myCodecs
                          )
                          channel
                          ( BFClient.blockFetchClientPeer
                              blockFetchClientNull
                          )

                , NtN.keepAliveProtocol =
                    NetMux.InitiatorProtocolOnly
                    $ NetMux.MiniProtocolCb
                    $ \ctx channel ->
                        kaClient
                          ( NetCtx.remoteAddress
                              $ NetCtx.micConnectionId
                                  ctx
                          )
                          channel

                , NtN.txSubmissionProtocol =
                    NetMux.InitiatorProtocolOnly
                    $ NetMux.MiniProtocolCb
                    $ \_ctx channel ->
                        Driver.runPeer
                          ( Tracing.trTxSubmission2
                              tracers
                          )
                          ( NetN2N.cTxSubmission2Codec
                              myCodecs
                          )
                          channel
                          ( TxSub.txSubmissionClientPeer
                              myTxSubClient
                          )

                , NtN.peerSharingProtocol =
                    NetMux.InitiatorProtocolOnly
                    $ NetMux.MiniProtocolCb
                    $ \_ctx channel ->
                        Driver.runPeer
                          mempty
                          ( NetN2N.cPeerSharingCodec
                              myCodecs
                          )
                          channel
                          ( PSClient.peerSharingClientPeer
                              peerSharingClientNull
                          )
                }
              n2nVer
              n2nData

    mkApp
      :: NetMux.OuroborosBundle
           mode initiatorCtx responderCtx
           bs m a b
      -> NetMux.OuroborosApplication
           mode initiatorCtx responderCtx
           bs m a b
    mkApp bundle = NetMux.OuroborosApplication $ fold bundle

    kaClient
      :: Ord remotePeer
      => remotePeer
      -> Channel.Channel IO BSL.ByteString
      -> IO ((), Maybe BSL.ByteString)
    kaClient them channel = do
      keepAliveRng <- Random.newStdGen
      peerGSVMap <-
        liftIO
          . StrictSTM.newTVarIO
          $ Map.singleton them DeltaQ.defaultGSV
      Driver.runPeerWithLimits
        (Tracing.trKeepAlive tracers)
        (NetN2N.cKeepAliveCodec myCodecs)
        (KACodec.byteLimitsKeepAlive (const 0))
        KACodec.timeLimitsKeepAlive
        channel
        $ KAClient.keepAliveClientPeer
        $ KeepAlive.keepAliveClient
            mempty
            keepAliveRng
            ( ControlMsg.continueForever
                (Proxy :: Proxy IO)
            )
            them
            peerGSVMap
            keepAliveTimeout

-- | Null block fetch client.
blockFetchClientNull
  :: MonadTimer.MonadTimer m
  => BFClient.BlockFetchClient block point m a
blockFetchClientNull =
  BFClient.BlockFetchClient
    $ forever
    $ MonadTimer.threadDelay (24 * 60 * 60)

-- | Null peer sharing client.
peerSharingClientNull
  :: MonadTimer.MonadTimer m
  => PSClient.PeerSharingClient addr m a
peerSharingClientNull =
  PSClient.SendMsgDone
    $ forever
    $ MonadTimer.threadDelay (24 * 60 * 60)
