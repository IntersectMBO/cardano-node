{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Tracing.Consistency
  ( getAllNamespaces
  , checkNodeTraceConfiguration
  , checkNodeTraceConfiguration'
  ) where

import           Control.Exception (SomeException)
import qualified Data.Text as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types ()
import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.Formatting ()
import qualified Cardano.Node.Tracing.StateRep as SR
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.Diffusion ()
-- import           Cardano.Node.Tracing.Tracers.ForgingThreadStats (ForgeThreadStats,
--                    forgeThreadStats, ForgingStats)
import           Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import           Cardano.Node.Startup
import           Cardano.Node.Tracing.Tracers.KESInfo ()
import           Cardano.Node.Tracing.Tracers.NodeToClient ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NonP2P ()
import           Cardano.Node.Tracing.Tracers.P2P ()
import           Cardano.Node.Tracing.Tracers.Peer
import           Cardano.Node.Tracing.Tracers.Shutdown ()
import           Cardano.Node.Tracing.Tracers.Startup ()

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId)
import           Ouroboros.Consensus.Mempool (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB


import           Ouroboros.Network.Block (Point (..), SlotNo, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.ConnectionManager.Types (ConnectionManagerTrace (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import qualified Ouroboros.Network.Diffusion as Diffusion
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace)
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), RemoteAddress, WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.Governor (DebugPeerSelection (..),
                   PeerSelectionCounters (..), TracePeerSelection (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (TraceLedgerPeers)
import           Ouroboros.Network.PeerSelection.PeerStateActions (PeerSelectionActionsTrace (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS (TraceLocalRootPeers (..),
                   TracePublicRootPeers (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                   UnversionedProtocolData (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LTM
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import           Ouroboros.Network.Server2 (ServerTrace (..))
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..), WithDomainName (..))
import           Ouroboros.Network.Subscription.Worker (SubscriptionTrace (..))
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)



-- | Check the configuration in the given file.
-- If there is no configuration in the file check the standard configuration
-- An empty return list means, everything is well
checkNodeTraceConfiguration ::
     FilePath
  -> IO NSWarnings
checkNodeTraceConfiguration configFileName =
  checkTraceConfiguration
    configFileName
    defaultCardanoConfig
    getAllNamespaces

-- | Check the configuration in the given file.
-- If there is no configuration in the file check the standard configuration
-- An empty return list means, everything is well
checkNodeTraceConfiguration' ::
     TraceConfig
  -> NSWarnings
checkNodeTraceConfiguration' trConfig =
  checkTraceConfiguration'
    trConfig
    getAllNamespaces


-- | Returns a list of all namepsaces from all tracers
getAllNamespaces :: [[T.Text]]
getAllNamespaces =
    -- NodeInfo tracer
    let stateNS = map (nsGetComplete . nsReplacePrefix ["NodeState"])
                      (allNamespaces :: [Namespace SR.NodeState])
        peersNS = map (nsGetComplete . nsReplacePrefix ["Net", "Peers", "List"])
                      (allNamespaces :: [Namespace [PeerT blk]])
        resourcesNS = map nsGetComplete
                          (allNamespaces :: [Namespace ResourceStats])
        startupNS = map (nsGetComplete . nsReplacePrefix ["Startup"])
                        (allNamespaces :: [Namespace (StartupTrace blk)])
        shutdownNS = map (nsGetComplete . nsReplacePrefix ["Shutdown"])
                        (allNamespaces :: [Namespace ShutdownTrace])
        chainDBNS = map (nsGetComplete . nsReplacePrefix ["ChainDB"])
                        (allNamespaces :: [Namespace (ChainDB.TraceEvent blk)])
        replayBlockNS = map (nsGetComplete . nsReplacePrefix ["ChainDB", "ReplayBlock"])
                        (allNamespaces :: [Namespace ReplayBlockStats])
-- Consensus tracers
        chainSyncClientNS = map
                              (nsGetComplete . nsReplacePrefix  ["ChainSync", "Client"])
                              (allNamespaces :: [Namespace (BlockFetch.TraceLabelPeer
                                                           (ConnectionId RemoteAddress)
                                                           (TraceChainSyncClientEvent blk))])

        chainSyncServerHeaderNS = map (nsGetComplete . nsReplacePrefix ["ChainSync", "ServerHeader"])
                        (allNamespaces :: [Namespace (TraceChainSyncServerEvent blk)])
        chainSyncServerBlockNS = map (nsGetComplete . nsReplacePrefix ["ChainSync", "ServerBlock"])
                        (allNamespaces :: [Namespace (TraceChainSyncServerEvent blk)])
        blockFetchDecisionNS = map (nsGetComplete . nsReplacePrefix ["BlockFetch", "Decision"])
                        (allNamespaces :: [Namespace [BlockFetch.TraceLabelPeer
                                                      remotePeer
                                                      (FetchDecision [Point (Header blk)])]])
        blockFetchClientNS = map (nsGetComplete . nsReplacePrefix ["BlockFetch", "Client"])
                        (allNamespaces :: [Namespace (BlockFetch.TraceLabelPeer
                                                      remotePeer
                                                      (BlockFetch.TraceFetchClientState (Header blk)))])

    -- TODO Yup
    -- blockFetchClientMetricsTr <- do
        blockFetchServerNS = map (nsGetComplete . nsReplacePrefix ["BlockFetch", "Server"])
                    (allNamespaces :: [Namespace (TraceBlockFetchServerEvent blk)])

        forgeKESInfoNS = map (nsGetComplete . nsReplacePrefix ["Forge", "StateInfo"])
                    (allNamespaces :: [Namespace (Consensus.TraceLabelCreds HotKey.KESInfo)])
        txInboundNS = map (nsGetComplete . nsReplacePrefix ["TxSubmission", "TxInbound"])
                        (allNamespaces :: [Namespace (BlockFetch.TraceLabelPeer
                            remotePeer
                            (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)))])
        txOutboundNS = map (nsGetComplete . nsReplacePrefix ["TxSubmission", "TxOutbound"])
                        (allNamespaces :: [Namespace (BlockFetch.TraceLabelPeer
                  remotePeer
                  (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)))])
        localTxSubmissionServerNS = map (nsGetComplete . nsReplacePrefix
                                            ["TxSubmission", "LocalServer"])
                        (allNamespaces :: [Namespace
                          (TraceLocalTxSubmissionServerEvent blk)])
        mempoolNS = map (nsGetComplete . nsReplacePrefix ["Mempool"])
                        (allNamespaces :: [Namespace (TraceEventMempool blk)])
        forgeNS = map (nsGetComplete . nsReplacePrefix ["Forge", "Loop"])
                        (allNamespaces :: [Namespace (ForgeTracerType blk)])

        forgeStateNS = [["Forge", "StateInfo"]]

        blockchainTimeNS = map (nsGetComplete . nsReplacePrefix  ["BlockchainTime"])
                        (allNamespaces :: [Namespace (TraceBlockchainTimeEvent RelativeTime)])

-- Node to client
        keepAliveClientNS = map (nsGetComplete . nsReplacePrefix ["Net"])
                                (allNamespaces :: [Namespace  (TraceKeepAliveClient peer)])
        chainSyncNS = map (nsGetComplete . nsReplacePrefix ["ChainSync", "Local"])
                          (allNamespaces :: [Namespace
                            (BlockFetch.TraceLabelPeer peer (TraceSendRecv
                              (ChainSync (Header blk) (Point blk) (Tip blk))))])
        txMonitorNS = map (nsGetComplete . nsReplacePrefix  ["TxSubmission", "MonitorClient"])
                          (allNamespaces :: [Namespace
                                (BlockFetch.TraceLabelPeer
                                    peer
                                    (TraceSendRecv
                                        (LTM.LocalTxMonitor
                                          (GenTxId blk) (GenTx blk) SlotNo)))])
        txSubmissionNS = map (nsGetComplete . nsReplacePrefix ["TxSubmission", "Local"])
                             (allNamespaces :: [Namespace
                                (BlockFetch.TraceLabelPeer
                                  peer
                                  (TraceSendRecv
                                    (LTS.LocalTxSubmission
                                        (GenTx blk) (ApplyTxErr blk))))])
        stateQueryNS = map (nsGetComplete . nsReplacePrefix ["StateQueryServer"])
                           (allNamespaces :: [Namespace
                                (BlockFetch.TraceLabelPeer peer
                                  (TraceSendRecv
                                    (LocalStateQuery blk (Point blk) (Query blk))))])

-- Node to Node
        chainSyncNodeNS = map (nsGetComplete . nsReplacePrefix ["ChainSync", "Remote"])
                              (allNamespaces :: [Namespace
                                (BlockFetch.TraceLabelPeer peer (TraceSendRecv
                                  (ChainSync (Header blk) (Point blk) (Tip blk))))])
        chainSyncSerialisedNS = map (nsGetComplete . nsReplacePrefix
                                        ["ChainSync", "Remote", "Serialised"])
                             (allNamespaces :: [Namespace
                                (BlockFetch.TraceLabelPeer peer (TraceSendRecv
                                  (ChainSync (Header blk) (Point blk) (Tip blk))))])
        blockFetchNS = map (nsGetComplete . nsReplacePrefix ["BlockFetch", "Remote"])
                             (allNamespaces :: [Namespace
                                 (BlockFetch.TraceLabelPeer peer
                                    (TraceSendRecv
                                      (BlockFetch blk (Point blk))))])
        blockFetchSerialisedNS = map (nsGetComplete . nsReplacePrefix
                                        ["BlockFetch", "Remote", "Serialised"])
                             (allNamespaces :: [Namespace
                                 (BlockFetch.TraceLabelPeer peer
                                    (TraceSendRecv
                                      (BlockFetch blk (Point blk))))])
        txSubmission2NS = map (nsGetComplete . nsReplacePrefix
                                        ["TxSubmission", "Remote"])
                             (allNamespaces :: [Namespace
                                 (BlockFetch.TraceLabelPeer peer
                                    (TraceSendRecv
                                      (TxSubmission2 (GenTxId blk) (GenTx blk))))])

-- Diffusion

        dtMuxNS = map (nsGetComplete . nsReplacePrefix ["Net", "Mux", "Remote"])
                             (allNamespaces :: [Namespace
                                 (WithMuxBearer (ConnectionId RemoteAddress) MuxTrace)])
        dtLocalMuxNS = map (nsGetComplete . nsReplacePrefix ["Net", "Mux", "Local"])
                             (allNamespaces :: [Namespace
                                 (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)])
        dtHandshakeNS = map (nsGetComplete . nsReplacePrefix
                                ["Net", "Handshake", "Remote"])
                            (allNamespaces :: [Namespace
                              (NtN.HandshakeTr NtN.RemoteAddress NtN.NodeToNodeVersion)])
        dtLocalHandshakeNS = map (nsGetComplete . nsReplacePrefix
                                   ["Net", "Handshake", "Local"])
                                 (allNamespaces :: [Namespace
                                   (NtC.HandshakeTr LocalAddress
                                      NtC.NodeToClientVersion)])
        dtDiffusionInitializationNS = map (nsGetComplete . nsReplacePrefix
                                            ["Startup", "DiffusionInit"])
                                          (allNamespaces :: [Namespace
                                            (Diffusion.DiffusionTracer Socket.SockAddr
                                                LocalAddress)])
        dtLedgerPeersNS = map (nsGetComplete . nsReplacePrefix
                               ["Net", "Peers", "Ledger"])
                               (allNamespaces :: [Namespace TraceLedgerPeers])

-- DiffusionTracersExtra P2P

        localRootPeersNS = map (nsGetComplete . nsReplacePrefix
                               ["Net", "Peers", "LocalRoot"])
                               (allNamespaces :: [Namespace
                                 (TraceLocalRootPeers RemoteAddress SomeException)])
        publicRootPeersNS = map (nsGetComplete . nsReplacePrefix
                                  ["Net", "Peers", "PublicRoot"])
                               (allNamespaces :: [Namespace TracePublicRootPeers])
        peerSelectionNS = map (nsGetComplete . nsReplacePrefix
                                  ["Net", "PeerSelection", "Selection"])
                               (allNamespaces :: [Namespace
                                          (TracePeerSelection Socket.SockAddr)])
        debugPeerSelectionNS = map (nsGetComplete . nsReplacePrefix
                                  ["Net", "PeerSelection", "Initiator"])
                               (allNamespaces :: [Namespace
                                          (DebugPeerSelection Socket.SockAddr)])
        debugPeerSelectionResponderNS = map (nsGetComplete . nsReplacePrefix
                                  ["Net", "PeerSelection", "Responder"])
                               (allNamespaces :: [Namespace
                                          (DebugPeerSelection Socket.SockAddr)])
        peerSelectionCountersNS = map (nsGetComplete . nsReplacePrefix
                                        ["Net", "PeerSelection", "Counters"])
                                      (allNamespaces :: [Namespace
                                        PeerSelectionCounters])

        peerSelectionActionsNS = map (nsGetComplete . nsReplacePrefix
                                  ["Net", "PeerSelection", "Actions"])
                               (allNamespaces :: [Namespace
                                 (PeerSelectionActionsTrace Socket.SockAddr LocalAddress)])
        connectionManagerNS = map (nsGetComplete . nsReplacePrefix
                                    ["Net", "ConnectionManager", "Remote"])
                                  (allNamespaces :: [Namespace
                                    (ConnectionManagerTrace
                                    Socket.SockAddr
                                      (ConnectionHandlerTrace
                                        UnversionedProtocol
                                        UnversionedProtocolData))])
        connectionManagerTransitionsNS = map (nsGetComplete . nsReplacePrefix
                                                ["Net", "ConnectionManager", "Transition"])
                                            (allNamespaces :: [Namespace
                                                (ConnectionManager.AbstractTransitionTrace
                                                Socket.SockAddr)])
        serverNS = map (nsGetComplete . nsReplacePrefix
                         ["Net", "Server", "Remote"])
                       (allNamespaces :: [Namespace (ServerTrace Socket.SockAddr)])
        inboundGovernorNS = map (nsGetComplete . nsReplacePrefix
                                  ["Net", "InboundGovernor", "Remote"])
                                (allNamespaces :: [Namespace
                                  (InboundGovernorTrace Socket.SockAddr)])
        inboundGovernorTransitionsNS = map (nsGetComplete . nsReplacePrefix
                                      ["Net", "InboundGovernor", "Transition"])
                                           (allNamespaces :: [Namespace
                                      (InboundGovernor.RemoteTransitionTrace Socket.SockAddr)])
        localConnectionManagerNS = map (nsGetComplete . nsReplacePrefix
                                         ["Net", "ConnectionManager", "Local"])
                                       (allNamespaces :: [Namespace
                                       (ConnectionManagerTrace
                                          Socket.SockAddr
                                          (ConnectionHandlerTrace
                                            UnversionedProtocol
                                            UnversionedProtocolData))])
        localServerNS = map (nsGetComplete . nsReplacePrefix
                              ["Net", "Server", "Local"])
                            (allNamespaces :: [Namespace
                              (ServerTrace LocalAddress)])
        localInboundGovernorNS = map (nsGetComplete . nsReplacePrefix
                                        ["Net", "InboundGovernor", "Local"])
                                     (allNamespaces :: [Namespace
                                        (InboundGovernorTrace LocalAddress)])


-- -- DiffusionTracersExtra nonP2P

        dtIpSubscriptionNS = map (nsGetComplete . nsReplacePrefix
                                   ["Net", "Subscription", "IP"])
                                 (allNamespaces :: [Namespace
                                   (SubscriptionTrace Socket.SockAddr)])
        dtDnsSubscriptionNS = map (nsGetComplete . nsReplacePrefix
                                    ["Net", "Subscription", "DNS"])
                                  (allNamespaces :: [Namespace
                                    (WithDomainName (SubscriptionTrace Socket.SockAddr))])
        dtDnsResolverNS = map (nsGetComplete . nsReplacePrefix
                                ["Net", "DNSResolver"])
                              (allNamespaces :: [Namespace
                                (WithDomainName DnsTrace)])
        dtErrorPolicyNS = map (nsGetComplete . nsReplacePrefix
                                ["Net", "ErrorPolicy", "Remote"])
                              (allNamespaces :: [Namespace
                                 (WithAddr Socket.SockAddr ErrorPolicyTrace)])
        dtLocalErrorPolicyNS = map (nsGetComplete . nsReplacePrefix
                                     ["Net", "ErrorPolicy", "Local"])
                                   (allNamespaces :: [Namespace
                                     (WithAddr LocalAddress ErrorPolicyTrace)])
        dtAcceptPolicyNS = map (nsGetComplete . nsReplacePrefix
                                 ["Net", "AcceptPolicy"])
                               (allNamespaces :: [Namespace
                                  NtN.AcceptConnectionsPolicyTrace])

        allNamespaces' :: [[T.Text]] =
            stateNS
            <> peersNS
            <> resourcesNS
            <> startupNS
            <> shutdownNS
            <> chainDBNS
            <> replayBlockNS
-- Consensus
            <> chainSyncClientNS
            <> chainSyncServerHeaderNS
            <> chainSyncServerBlockNS
            <> blockFetchDecisionNS
            <> blockFetchClientNS
            <> blockFetchServerNS
            <> forgeKESInfoNS
            <> txInboundNS
            <> txOutboundNS
            <> localTxSubmissionServerNS
            <> mempoolNS
            <> forgeNS
            <> forgeStateNS
            <> blockchainTimeNS
-- NodeToClient
            <> keepAliveClientNS
            <> chainSyncNS
            <> txMonitorNS
            <> txSubmissionNS
            <> stateQueryNS
-- Node to Node
            <> chainSyncNodeNS
            <> chainSyncSerialisedNS
            <> blockFetchNS
            <> blockFetchSerialisedNS
            <> txSubmission2NS
-- Diffusion
            <> dtMuxNS
            <> dtLocalMuxNS
            <> dtHandshakeNS
            <> dtLocalHandshakeNS
            <> dtDiffusionInitializationNS
            <> dtLedgerPeersNS

-- DiffusionTracersExtra P2P
            <> localRootPeersNS
            <> publicRootPeersNS
            <> peerSelectionNS
            <> debugPeerSelectionNS
            <> debugPeerSelectionResponderNS
            <> peerSelectionCountersNS
            <> peerSelectionActionsNS
            <> connectionManagerNS
            <> connectionManagerTransitionsNS
            <> serverNS
            <> inboundGovernorNS
            <> inboundGovernorTransitionsNS
            <> localConnectionManagerNS
            <> localServerNS
            <> localInboundGovernorNS

-- DiffusionTracersExtra nonP2P
            <> dtIpSubscriptionNS
            <> dtDnsSubscriptionNS
            <> dtDnsResolverNS
            <> dtErrorPolicyNS
            <> dtLocalErrorPolicyNS
            <> dtAcceptPolicyNS
    in allNamespaces'
