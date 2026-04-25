{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

-- | NodeToClient (local socket) connection for the tx-centrifuge.
--
-- Mirrors the interface of "Cardano.Benchmarking.TxCentrifuge.NodeToNode" but
-- connects to a local cardano-node via a Unix domain socket instead of TCP.
--
-- == Key differences from NodeToNode
--
-- * __Transport__: Unix domain socket ('FilePath') instead of TCP
--   ('Network.Socket.AddrInfo').
--
-- * __ChainSync__: Delivers full blocks (not just headers). No separate
--   BlockFetch client is needed because a single ChainSync client can both
--   follow the chain and extract transaction IDs for confirmation tracking.
--
-- * __LocalTxSubmission__: Synchronous, push-based submission (submit one tx,
--   get accept\/reject) instead of the pull-based TxSubmission2 protocol.
--
-- * __Additional protocols__: LocalStateQuery and LocalTxMonitor are
--   available (currently wired as idle\/null clients; callers can extend
--   'Clients' when needed).
--
-- * __No KeepAlive__: The NodeToClient protocol suite does not include a
--   KeepAlive mini-protocol.
module Cardano.Benchmarking.TxCentrifuge.NodeToClient
  ( -- * Client bundle.
    Clients (..)
  , emptyClients
    -- * Connection.
  , connect
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
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
-----------------
-- network-mux --
-----------------
import Network.Mux qualified as Mux
-----------------------
-- cardano-diffusion --
-----------------------
import Cardano.Network.NodeToClient qualified as NtC
-----------------------------------
-- ouroboros-consensus:diffusion --
-----------------------------------
import Ouroboros.Consensus.Network.NodeToClient qualified as NetN2C
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Block.Abstract qualified as Block
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as NetVer
import Ouroboros.Consensus.Node.Run ()
-- Orphan instances needed for
-- RunNode / SupportedNetworkProtocolVersion Block.CardanoBlock
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
---------------------------
-- ouroboros-network:api --
---------------------------
import Ouroboros.Network.Magic qualified as Magic
---------------------------------
-- ouroboros-network:framework --
---------------------------------
import Ouroboros.Network.Driver qualified as Driver
import Ouroboros.Network.Driver.Stateful qualified as StatefulDriver
import Ouroboros.Network.IOManager qualified as IOManager
import Ouroboros.Network.Mux qualified as NetMux
---------------------------------
-- ouroboros-network:protocols --
---------------------------------
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CSClient
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as LSQ
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as LTxSub
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.NodeToClient.TxIdSync
  qualified as TxIdSync
import Cardano.Benchmarking.TxCentrifuge.NodeToClient.TxSubmission
  qualified as TxSubmission
import Cardano.Benchmarking.TxCentrifuge.Block qualified as Block
import Cardano.Benchmarking.TxCentrifuge.Tracing qualified as Tracing

--------------------------------------------------------------------------------
-- Client bundle.
--------------------------------------------------------------------------------

-- | Bundle of mini-protocol clients for a NodeToClient (local) connection.
--
-- All clients are optional ('Maybe'). When 'Nothing', a null\/idle client is
-- used that waits forever without participating in the protocol.
--
-- This allows callers to selectively enable protocols:
-- * TxSubmission only: submitting transactions via local socket.
-- * ChainSync only: following the chain (observation \/ confirmation tracking).
data Clients = Clients
  { clientChainSync    :: !(Maybe TxIdSync.ChainSyncClient)
  , clientTxSubmission :: !(Maybe TxSubmission.TxSubmissionClient)
  }

-- | Empty clients: all protocols disabled (null\/idle clients).
emptyClients :: Clients
emptyClients = Clients
  { clientChainSync    = Nothing
  , clientTxSubmission = Nothing
  }

--------------------------------------------------------------------------------
-- Connection.
--------------------------------------------------------------------------------

-- | Connect to a local cardano-node via NodeToClient protocols.
--
-- Establishes a multiplexed connection over a Unix domain socket running
-- ChainSync, LocalTxSubmission, LocalStateQuery, and LocalTxMonitor clients.
-- LocalStateQuery and LocalTxMonitor are always set to null\/idle clients;
-- ChainSync and LocalTxSubmission are controlled by the 'Clients' bundle.
--
-- Returns @Left msg@ on connection failure or unexpected termination.
-- In normal operation the connection runs indefinitely (the mux never
-- returns successfully).
connect
  :: IOManager.IOManager
  -> Block.CodecConfig Block.CardanoBlock
  -> Magic.NetworkMagic
  -> Tracing.Tracers
  -- | Path to the node's local Unix domain socket.
  -> FilePath
  -> Clients
  -> IO (Either String ())
connect
  ioManager
  codecConfig
  networkMagic
  _tracers
  socketPath
  clients = do
  done <- NtC.connectTo
    (NtC.localSnocket ioManager)
    NtC.nullNetworkConnectTracers
    peerMultiplex
    socketPath
  case done of
    Left err -> pure $ Left $
      "connection failed: " ++ show err
    Right () -> pure $ Left
      "connection terminated unexpectedly"

  where

    -- Protocol version. NodeToClientV_22 is the latest released version as of
    -- cardano-node 10.x.
    n2cVer :: NetVer.NodeToClientVersion
    n2cVer = NetVer.NodeToClientV_22

    blkN2cVer :: NetVer.BlockNodeToClientVersion Block.CardanoBlock
    blkN2cVer = case Map.lookup n2cVer supportedVers of
      Just v  -> v
      Nothing -> error $
        "NodeToClient.connect: " ++ show n2cVer
        ++ " is not in supportedNodeToClientVersions. "
        ++ "Supported: " ++ show (Map.keys supportedVers)

    supportedVers
      :: Map.Map
           NetVer.NodeToClientVersion
           ( NetVer.BlockNodeToClientVersion
               Block.CardanoBlock
           )
    supportedVers =
      NetVer.supportedNodeToClientVersions (Proxy @Block.CardanoBlock)

    -- Use 'clientCodecs' so ChainSync delivers deserialized blocks
    -- (Block.CardanoBlock), not 'Serialised Block.CardanoBlock'. This lets the
    -- ChainSync client extract transaction IDs directly from the block body,
    -- the same way the N2N BlockFetch client does.
    myCodecs
      :: NetN2C.ClientCodecs Block.CardanoBlock IO
    myCodecs =
      NetN2C.clientCodecs
        codecConfig
        blkN2cVer
        n2cVer

    peerMultiplex
      :: NtC.Versions
           NetVer.NodeToClientVersion
           NtC.NodeToClientVersionData
           ( NetMux.OuroborosApplicationWithMinimalCtx
               'Mux.InitiatorMode
               NtC.LocalAddress
               BSL.ByteString
               IO
               ()
               Void
           )
    peerMultiplex =
      NtC.versionedNodeToClientProtocols
        n2cVer
        ( NtC.NodeToClientVersionData
            { NtC.networkMagic = networkMagic
            , NtC.query = False
            }
        )
        protocolBundle

    -- | All four NodeToClient protocols are always present in the bundle.
    -- Protocols without a client in 'Clients' get a null\/idle peer that waits
    -- forever.
    protocolBundle
      :: NtC.NodeToClientProtocols
           'Mux.InitiatorMode
           NtC.LocalAddress
           BSL.ByteString
           IO
           ()
           Void
    protocolBundle = NtC.NodeToClientProtocols
      { NtC.localChainSyncProtocol =
          NetMux.InitiatorProtocolOnly
            $ NetMux.MiniProtocolCb $ \_ctx channel ->
              case clientChainSync clients of
                Just cs ->
                  Driver.runPeer
                    mempty
                    (NetN2C.cChainSyncCodec myCodecs)
                    channel
                    (CSClient.chainSyncClientPeer cs)
                Nothing ->
                  Driver.runPeer
                    mempty
                    (NetN2C.cChainSyncCodec myCodecs)
                    channel
                    NtC.chainSyncPeerNull
      , NtC.localTxSubmissionProtocol =
          NetMux.InitiatorProtocolOnly
            $ NetMux.MiniProtocolCb $ \_ctx channel ->
              case clientTxSubmission clients of
                Just ltx ->
                  Driver.runPeer
                    mempty
                    (NetN2C.cTxSubmissionCodec myCodecs)
                    channel
                    (LTxSub.localTxSubmissionClientPeer ltx)
                Nothing ->
                  Driver.runPeer
                    mempty
                    (NetN2C.cTxSubmissionCodec myCodecs)
                    channel
                    NtC.localTxSubmissionPeerNull
      , NtC.localStateQueryProtocol =
          NetMux.InitiatorProtocolOnly
            $ NetMux.MiniProtocolCb $ \_ctx channel ->
              StatefulDriver.runPeer
                mempty
                (NetN2C.cStateQueryCodec myCodecs)
                channel
                LSQ.StateIdle
                NtC.localStateQueryPeerNull
      , NtC.localTxMonitorProtocol =
          NetMux.InitiatorProtocolOnly
            $ NetMux.MiniProtocolCb $ \_ctx channel ->
              Driver.runPeer
                mempty
                (NetN2C.cTxMonitorCodec myCodecs)
                channel
                NtC.localTxMonitorPeerNull
      }

