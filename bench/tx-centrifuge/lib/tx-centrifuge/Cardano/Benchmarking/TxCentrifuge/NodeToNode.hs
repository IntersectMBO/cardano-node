{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.NodeToNode
  ( CardanoBlock
    -- * Client bundle.
  , Clients (..), emptyClients
    -- * Connection.
  , connect
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
----------------
-- bytestring --
----------------
import Data.ByteString.Lazy qualified as BSL
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
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
import Ouroboros.Consensus.Network.NodeToNode qualified as NetN2N
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as NetVer
import Ouroboros.Consensus.Node.Run ()
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
-- Orphan instances needed for
-- RunNode / SupportedNetworkProtocolVersion CardanoBlock
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
-----------------------
-- ouroboros-network --
-----------------------
import Ouroboros.Network.Context qualified as NetCtx
import Ouroboros.Network.Driver qualified as Driver
import Ouroboros.Network.Magic qualified as Magic
import Ouroboros.Network.Mux qualified as NetMux
import Ouroboros.Network.NodeToNode qualified as NtN
import Ouroboros.Network.PeerSelection.PeerSharing qualified as PeerSharing
import Ouroboros.Network.PeerSelection.PeerSharing.Codec qualified as PSCodec
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BFClient
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CSClient
import Ouroboros.Network.Protocol.Handshake.Version qualified as Handshake
import Ouroboros.Network.Protocol.KeepAlive.Client qualified as KAClient
import Ouroboros.Network.Protocol.KeepAlive.Codec qualified as KACodec
import Ouroboros.Network.Protocol.TxSubmission2.Client qualified as TxSub
import Ouroboros.Network.Snocket qualified as Snocket
---------------------------------
-- ouroboros-network-framework --
---------------------------------
import Ouroboros.Network.IOManager qualified as IOManager
---------------
-- serialise --
---------------
import Codec.Serialise qualified as Serialise
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.KeepAlive
  qualified as KeepAlive
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync
  qualified as TxIdSync
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxSubmission
  qualified as TxSubmission
import Cardano.Benchmarking.TxCentrifuge.Tracing qualified as Tracing

--------------------------------------------------------------------------------

type CardanoBlock = Consensus.CardanoBlock Eras.StandardCrypto

--------------------------------------------------------------------------------
-- Client bundle.
--------------------------------------------------------------------------------

-- | Bundle of mini-protocol clients for a NodeToNode connection.
--
-- All clients are optional ('Maybe'). When 'Nothing', the protocol is not
-- included in the connection at all, the mux simply doesn't run that
-- mini-protocol. This is the proper way to disable protocols per the
-- ouroboros-network design (using @[]@ / 'mempty' in the protocol bundle).
--
-- This allows callers to selectively enable protocols:
--
-- * TxSubmission only: for submitting transactions without chain following.
-- * ChainSync + BlockFetch: for tracking transaction confirmations.
-- * All: for full closed-loop operation with confirmation-based recycling.
data Clients = Clients
  { clientBlockFetch   :: !(Maybe TxIdSync.BlockFetchClient)
  , clientChainSync    :: !(Maybe TxIdSync.ChainSyncClient)
  , clientKeepAlive    :: !(Maybe KeepAlive.KeepAliveClient)
  , clientTxSubmission :: !(Maybe TxSubmission.TxSubmissionClient)
  }

-- | Empty clients: all protocols disabled (null/idle clients).
emptyClients :: Clients
emptyClients = Clients
  { clientBlockFetch   = Nothing
  , clientChainSync    = Nothing
  , clientKeepAlive    = Nothing
  , clientTxSubmission = Nothing
  }

--------------------------------------------------------------------------------
-- Mini-protocol builders.
--------------------------------------------------------------------------------

-- | Protocol limits matching cardano-diffusion defaults.
-- See Cardano.Network.NodeToNode.defaultMiniProtocolParameters.

blockFetchLimits :: NetMux.MiniProtocolLimits
blockFetchLimits = NetMux.MiniProtocolLimits
                     { NetMux.maximumIngressQueue = 20_000_000 }

chainSyncLimits :: NetMux.MiniProtocolLimits
chainSyncLimits = NetMux.MiniProtocolLimits
                    { NetMux.maximumIngressQueue = 300_000 }

keepAliveLimits :: NetMux.MiniProtocolLimits
keepAliveLimits = NetMux.MiniProtocolLimits
                    { NetMux.maximumIngressQueue = 1_500 }

txSubmissionLimits :: NetMux.MiniProtocolLimits
txSubmissionLimits = NetMux.MiniProtocolLimits
                       { NetMux.maximumIngressQueue = 10_000_000 }

-- | Build a BlockFetch mini-protocol.
mkBlockFetchMiniProtocol
  :: NetN2N.Codecs CardanoBlock NtN.RemoteAddress
       Serialise.DeserialiseFailure IO
       BSL.ByteString BSL.ByteString BSL.ByteString BSL.ByteString
       BSL.ByteString BSL.ByteString BSL.ByteString
  -> TxIdSync.BlockFetchClient
  -> NetMux.MiniProtocol
       'Mux.InitiatorMode
       (NetCtx.MinimalInitiatorContext NtN.RemoteAddress)
       (NetCtx.ResponderContext NtN.RemoteAddress)
       BSL.ByteString IO () Void
mkBlockFetchMiniProtocol codecs client = NetMux.MiniProtocol
  { NetMux.miniProtocolNum    = NetMux.MiniProtocolNum 3
  , NetMux.miniProtocolStart  = Mux.StartOnDemand
  , NetMux.miniProtocolLimits = blockFetchLimits
  , NetMux.miniProtocolRun    = NetMux.InitiatorProtocolOnly
      $ NetMux.MiniProtocolCb $ \_ctx channel ->
          Driver.runPeer mempty (NetN2N.cBlockFetchCodec codecs) channel
            (BFClient.blockFetchClientPeer client)
  }

-- | Build a ChainSync mini-protocol.
mkChainSyncMiniProtocol
  :: NetN2N.Codecs CardanoBlock NtN.RemoteAddress
       Serialise.DeserialiseFailure IO
       BSL.ByteString BSL.ByteString BSL.ByteString BSL.ByteString
       BSL.ByteString BSL.ByteString BSL.ByteString
  -> TxIdSync.ChainSyncClient
  -> NetMux.MiniProtocol
       'Mux.InitiatorMode
       (NetCtx.MinimalInitiatorContext NtN.RemoteAddress)
       (NetCtx.ResponderContext NtN.RemoteAddress)
       BSL.ByteString IO () Void
mkChainSyncMiniProtocol codecs client = NetMux.MiniProtocol
  { NetMux.miniProtocolNum    = NetMux.MiniProtocolNum 2
  , NetMux.miniProtocolStart  = Mux.StartOnDemand
  , NetMux.miniProtocolLimits = chainSyncLimits
  , NetMux.miniProtocolRun    = NetMux.InitiatorProtocolOnly
      $ NetMux.MiniProtocolCb $ \_ctx channel ->
          Driver.runPeer mempty (NetN2N.cChainSyncCodec codecs) channel
            (CSClient.chainSyncClientPeer client)
  }

-- | Build a KeepAlive mini-protocol.
mkKeepAliveMiniProtocol
  :: NetN2N.Codecs CardanoBlock NtN.RemoteAddress
       Serialise.DeserialiseFailure IO
       BSL.ByteString BSL.ByteString BSL.ByteString BSL.ByteString
       BSL.ByteString BSL.ByteString BSL.ByteString
  -> Tracing.Tracers
  -> KeepAlive.KeepAliveClient
  -> NetMux.MiniProtocol
       'Mux.InitiatorMode
       (NetCtx.MinimalInitiatorContext NtN.RemoteAddress)
       (NetCtx.ResponderContext NtN.RemoteAddress)
       BSL.ByteString IO () Void
mkKeepAliveMiniProtocol codecs tracers client = NetMux.MiniProtocol
  { NetMux.miniProtocolNum    = NetMux.MiniProtocolNum 8
  , NetMux.miniProtocolStart  = Mux.StartOnDemandAny
  , NetMux.miniProtocolLimits = keepAliveLimits
  , NetMux.miniProtocolRun    = NetMux.InitiatorProtocolOnly
      $ NetMux.MiniProtocolCb $ \_ctx channel ->
          Driver.runPeerWithLimits
            (Tracing.trKeepAlive tracers)
            (NetN2N.cKeepAliveCodec codecs)
            (KACodec.byteLimitsKeepAlive (const 0))
            KACodec.timeLimitsKeepAlive
            channel
            $ KAClient.keepAliveClientPeer client
  }

-- | Build a TxSubmission mini-protocol.
mkTxSubmissionMiniProtocol
  :: NetN2N.Codecs CardanoBlock NtN.RemoteAddress
       Serialise.DeserialiseFailure IO
       BSL.ByteString BSL.ByteString BSL.ByteString BSL.ByteString
       BSL.ByteString BSL.ByteString BSL.ByteString
  -> Tracing.Tracers
  -> TxSubmission.TxSubmissionClient
  -> NetMux.MiniProtocol
       'Mux.InitiatorMode
       (NetCtx.MinimalInitiatorContext NtN.RemoteAddress)
       (NetCtx.ResponderContext NtN.RemoteAddress)
       BSL.ByteString IO () Void
mkTxSubmissionMiniProtocol codecs tracers client = NetMux.MiniProtocol
  { NetMux.miniProtocolNum    = NetMux.MiniProtocolNum 4
  , NetMux.miniProtocolStart  = Mux.StartOnDemand
  , NetMux.miniProtocolLimits = txSubmissionLimits
  , NetMux.miniProtocolRun    = NetMux.InitiatorProtocolOnly
      $ NetMux.MiniProtocolCb $ \_ctx channel ->
          Driver.runPeer (Tracing.trTxSubmission2 tracers)
            (NetN2N.cTxSubmission2Codec codecs) channel
            (TxSub.txSubmissionClientPeer client)
  }

--------------------------------------------------------------------------------

-- | Connect to a remote cardano-node via NodeToNode protocols.
--
-- Establishes a multiplexed connection running ChainSync, BlockFetch,
-- TxSubmission2, KeepAlive, and null PeerSharing clients.
--
-- For any client set to 'Nothing' in 'Clients', a null/idle client is used
-- that waits forever without participating in the protocol.
--
-- Returns @Left msg@ on handshake failure or unexpected connection termination.
-- The @Right@ case is unreachable (the mux never returns successfully).
connect
  :: IOManager.IOManager
  -> Block.CodecConfig CardanoBlock
  -> Magic.NetworkMagic
  -> Tracing.Tracers
  -> Socket.AddrInfo
  -> Clients
  -> IO (Either String ())
connect
  ioManager
  codecConfig
  networkMagic
  tracers
  remoteAddr
  clients = do
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
      Right {} -> error "connect: unreachable (Void)"

  where

    n2nVer :: NetVer.NodeToNodeVersion
    n2nVer = NetVer.NodeToNodeV_14

    blkN2nVer :: NetVer.BlockNodeToNodeVersion CardanoBlock
    blkN2nVer = case Map.lookup n2nVer supportedVers of
      Just v  -> v
      Nothing -> error $
        "NodeToNode.connect: " ++ show n2nVer
        ++ " is not in supportedNodeToNodeVersions. "
        ++ "Supported: " ++ show (Map.keys supportedVers)

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
        $ \_n2nData -> bundleToApp protocolBundle

    -- | Build the protocol bundle with conditional protocol inclusion.
    -- Protocols with 'Nothing' clients are excluded (empty list).
    protocolBundle :: NetMux.OuroborosBundle
                        'Mux.InitiatorMode
                        (NetCtx.MinimalInitiatorContext NtN.RemoteAddress)
                        (NetCtx.ResponderContext NtN.RemoteAddress)
                        BSL.ByteString
                        IO
                        ()
                        Void
    protocolBundle = NetMux.TemperatureBundle
      -- Hot protocols: ChainSync, BlockFetch, TxSubmission (conditional).
      (NetMux.WithHot $ catMaybes
        [ mkChainSyncMiniProtocol myCodecs <$> clientChainSync clients
        , mkBlockFetchMiniProtocol myCodecs <$> clientBlockFetch clients
        , mkTxSubmissionMiniProtocol myCodecs tracers <$> clientTxSubmission clients
        ])
      -- Warm protocols: none.
      (NetMux.WithWarm [])
      -- Established protocols: KeepAlive (conditional).
      (NetMux.WithEstablished $ catMaybes
        [ mkKeepAliveMiniProtocol myCodecs tracers <$> clientKeepAlive clients
        ])

    -- | Convert bundle to application by folding all protocols.
    bundleToApp :: NetMux.OuroborosBundle mode initiatorCtx responderCtx bs m a b
                -> NetMux.OuroborosApplication mode initiatorCtx responderCtx bs m a b
    bundleToApp (NetMux.TemperatureBundle (NetMux.WithHot h) (NetMux.WithWarm w) (NetMux.WithEstablished e)) =
      NetMux.OuroborosApplication (h <> w <> e)

