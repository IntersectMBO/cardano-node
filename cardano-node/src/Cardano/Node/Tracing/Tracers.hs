{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Tracing.Tracers
  ( mkDispatchTracers
  ) where

import           Codec.CBOR.Read (DeserialiseFailure)

import           Cardano.Logging
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (trace)

import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.ChainDB
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.Diffusion
import           Cardano.Node.Tracing.Tracers.ForgingThreadStats (forgeThreadStats)
import           Cardano.Node.Tracing.Tracers.KESInfo
import           Cardano.Node.Tracing.Tracers.NodeToClient
import           Cardano.Node.Tracing.Tracers.NodeToNode
import           Cardano.Node.Tracing.Tracers.NonP2P
import           Cardano.Node.Tracing.Tracers.P2P
import           Cardano.Node.Tracing.Tracers.Peer
import           Cardano.Node.Tracing.Tracers.Shutdown
import           Cardano.Node.Tracing.Tracers.Startup
import           Trace.Forward.Utils.DataPoint (DataPoint)

import           Cardano.Node.Queries (NodeKernelData)
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Tracing.StateRep (NodeState (..))
import           "contra-tracer" Control.Tracer (Tracer (..))
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import           Ouroboros.Consensus.Node (NetworkP2PMode (..))
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Network.Mux.Trace (TraceLabelPeer (..))
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import           Ouroboros.Network.NodeToClient (LocalAddress, NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion, RemoteAddress)

-- | Construct tracers for all system components.
--
mkDispatchTracers
  :: forall blk p2p.
  ( Consensus.RunNode blk
  , TraceConstraints blk

  , LogFormatting (LedgerEvent blk)
  , LogFormatting
    (TraceLabelPeer
      (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  )
  => NodeKernelData blk
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NetworkP2PMode p2p
  -> IO (Tracers (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk p2p)
mkDispatchTracers nodeKernel trBase trForward mbTrEKG trDataPoint trConfig enableP2P = do
    -- Some special tracers
    -- NodeInfo tracer
    nodeInfoTr <- mkDataPointTracer
                trDataPoint
                (const ["NodeInfo"])
    configureTracers trConfig docNodeInfoTraceEvent [nodeInfoTr]

    nodeStateTr <- mkDataPointTracer
                trDataPoint
                (const ["NodeState"])

    -- Resource tracer
    resourcesTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Resources"
                (const [])
                (const Info)
                allPublic
    configureTracers trConfig docResourceStats [resourcesTr]

    -- BasicInfo tracer
    startupTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Startup"
                namesStartupInfo
                (const Notice)
                allPublic
    configureTracers trConfig docStartupInfo [startupTr]

    shutdownTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Shutdown"
                namesForShutdown
                severityShutdown
                allPublic
    configureTracers trConfig docShutdown [shutdownTr]

    -- Peers tracer
    peersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Peers"
                namesForPeers
                severityPeers
                allPublic
    configureTracers trConfig docPeers [peersTr]

    chainDBTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                withAddedToCurrentChainEmptyLimited
    configureTracers trConfig docChainDBTraceEvent [chainDBTr]

    replayBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ReplayBlock"
                namesForReplayBlockStats
                severityReplayBlockStats
                allPublic
    configureTracers trConfig docReplayedBlock [replayBlockTr]

    -- This tracer handles replayed blocks specially
    replayBlockTr' <- withReplayedBlock replayBlockTr
    -- Filter out replayed blocks for this tracer
    let chainDBTr' = filterTrace
                      (\case (_, Nothing, ChainDB.TraceLedgerReplayEvent
                                            LedgerDB.ReplayedBlock {}) -> False
                             (_, _, _) -> True)
                      chainDBTr

    consensusTr :: Consensus.Tracers
                    IO
                    (ConnectionId RemoteAddress)
                    (ConnectionId LocalAddress)
                    blk <-
      mkConsensusTracers trBase trForward mbTrEKG trDataPoint trConfig nodeKernel

    nodeToClientTr :: NodeToClient.Tracers
                    IO
                    (ConnectionId LocalAddress)
                    blk
                    DeserialiseFailure <-
      mkNodeToClientTracers trBase trForward mbTrEKG trDataPoint trConfig

    nodeToNodeTr :: NodeToNode.Tracers
                    IO
                    (ConnectionId RemoteAddress)
                    blk
                    DeserialiseFailure <-
      mkNodeToNodeTracers trBase trForward mbTrEKG trDataPoint trConfig

    diffusionTr :: Diffusion.Tracers
                    RemoteAddress
                    NodeToNodeVersion
                    LocalAddress
                    NodeToClientVersion
                    IO <-
      mkDiffusionTracers trBase trForward mbTrEKG trDataPoint trConfig

    diffusionTrExtra :: Diffusion.ExtraTracers p2p <-
      mkDiffusionTracersExtra trBase trForward mbTrEKG trDataPoint trConfig enableP2P
    pure Tracers
      { chainDBTracer = Tracer (traceWith chainDBTr')
                        <> Tracer (traceWith replayBlockTr')
      , consensusTracers = consensusTr
      , nodeToClientTracers = nodeToClientTr
      , nodeToNodeTracers = nodeToNodeTr
      , diffusionTracers = diffusionTr
      , diffusionTracersExtra = diffusionTrExtra
      , startupTracer = Tracer $ \x -> do
                          traceWith startupTr x
                          traceWith nodeStateTr $ NodeStartup (ppStartupInfoTrace x)
      , shutdownTracer = Tracer $ \x -> do
                           traceWith shutdownTr x
                           traceWith nodeStateTr $ NodeShutdown x
      , nodeInfoTracer = Tracer (traceWith nodeInfoTr)
      , nodeStateTracer = Tracer (traceWith nodeStateTr)
      , resourcesTracer = Tracer (traceWith resourcesTr)
      , peersTracer = Tracer $ \x -> do
                        traceWith peersTr x
                        traceWith nodeStateTr $ NodePeers (map ppPeer x)
    }

mkConsensusTracers :: forall blk.
  ( Consensus.RunNode blk
  , TraceConstraints blk
  , LogFormatting (TraceLabelPeer
                    (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  )
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NodeKernelData blk
  -> IO (Consensus.Tracers IO (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk)
mkConsensusTracers trBase trForward mbTrEKG _trDataPoint trConfig nodeKernel = do
    chainSyncClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    configureTracers trConfig docChainSyncClientEvent [chainSyncClientTr]
    chainSyncServerHeaderTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEvent [chainSyncServerHeaderTr]
    chainSyncServerBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEvent [chainSyncServerBlockTr]
    blockFetchDecisionTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    configureTracers trConfig docBlockFetchDecision [blockFetchDecisionTr]
    blockFetchClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    configureTracers trConfig docBlockFetchClient [blockFetchClientTr]
    blockFetchServerTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    configureTracers trConfig docBlockFetchServer [blockFetchServerTr]
    forgeKESInfoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ForgeStateInfo"
                namesForKESInfo
                severityKESInfo
                allPublic
    configureTracers trConfig docForgeKESInfo [forgeKESInfoTr]
    txInboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
    configureTracers trConfig docTxInbound [txInboundTr]
    txOutboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
    configureTracers trConfig docTxOutbound [txOutboundTr]
    localTxSubmissionServerTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    configureTracers trConfig docLocalTxSubmissionServer [localTxSubmissionServerTr]
    mempoolTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Mempool"
                namesForMempool
                severityMempool
                allPublic
    configureTracers trConfig docMempool [mempoolTr]
    forgeTr    <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "Forge"
                namesForForge
                severityForge
                allPublic
                (forgeTracerTransform nodeKernel)
    forgeThreadStatsTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                forgeThreadStats
    configureTracers trConfig docForge [forgeTr, forgeThreadStatsTr]
    blockchainTimeTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    configureTracers trConfig docBlockchainTime [blockchainTimeTr]
    keepAliveClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
    configureTracers trConfig docKeepAliveClient [keepAliveClientTr]
    pure $ Consensus.Tracers
      { Consensus.chainSyncClientTracer = Tracer $
          traceWith chainSyncClientTr
      , Consensus.chainSyncServerHeaderTracer = Tracer $
          traceWith chainSyncServerHeaderTr
      , Consensus.chainSyncServerBlockTracer = Tracer $
          traceWith chainSyncServerBlockTr
      , Consensus.blockFetchDecisionTracer = Tracer $
          traceWith blockFetchDecisionTr
      , Consensus.blockFetchClientTracer = Tracer $
          traceWith blockFetchClientTr
      , Consensus.blockFetchServerTracer = Tracer $
          traceWith blockFetchServerTr
      , Consensus.forgeStateInfoTracer = Tracer $
          traceWith (traceAsKESInfo (Proxy @blk) forgeKESInfoTr)
      , Consensus.txInboundTracer = Tracer $
          traceWith txInboundTr
      , Consensus.txOutboundTracer = Tracer $
          traceWith txOutboundTr
      , Consensus.localTxSubmissionServerTracer = Tracer $
          traceWith localTxSubmissionServerTr
      , Consensus.mempoolTracer = Tracer $
          traceWith mempoolTr
      , Consensus.forgeTracer =
          Tracer (traceWith (contramap Left forgeTr))
          <> Tracer (traceWith (contramap Left forgeThreadStatsTr))
      , Consensus.blockchainTimeTracer = Tracer $
          traceWith blockchainTimeTr
      , Consensus.keepAliveClientTracer = Tracer $
          traceWith keepAliveClientTr
      }

mkNodeToClientTracers :: forall blk.
     Consensus.RunNode blk
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (NodeToClient.Tracers IO (ConnectionId LocalAddress) blk DeserialiseFailure)
mkNodeToClientTracers trBase trForward mbTrEKG _trDataPoint trConfig = do
    chainSyncTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        "ChainSyncClient"
        namesForTChainSync
        severityTChainSync
        allPublic
    configureTracers trConfig docTChainSync [chainSyncTr]
    txMonitorTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        "TxMonitorClient"
        namesForTTxMonitor
        severityTTxMonitor
        allPublic
    configureTracers trConfig docTTxMonitor [txMonitorTr]
    txSubmissionTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        "TxSubmissionClient"
        namesForTTxSubmission
        severityTTxSubmission
        allPublic
    configureTracers trConfig docTTxSubmission [txSubmissionTr]
    stateQueryTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        "StateQueryClient"
        namesForTStateQuery
        severityTStateQuery
        allPublic
    configureTracers trConfig docTStateQuery [stateQueryTr]
    pure $ NtC.Tracers
      { NtC.tChainSyncTracer = Tracer $
          traceWith chainSyncTr
      , NtC.tTxMonitorTracer = Tracer $
          traceWith txMonitorTr
      , NtC.tTxSubmissionTracer = Tracer $
          traceWith txSubmissionTr
      , NtC.tStateQueryTracer = Tracer $
          traceWith stateQueryTr
      }

mkNodeToNodeTracers :: forall blk.
  ( Consensus.RunNode blk
  , TraceConstraints blk)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (NodeToNode.Tracers IO (ConnectionId RemoteAddress) blk DeserialiseFailure)
mkNodeToNodeTracers trBase trForward mbTrEKG _trDataPoint trConfig = do
    chainSyncTracer <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    configureTracers trConfig docTChainSync [chainSyncTracer]
    chainSyncSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    configureTracers trConfig docTChainSync [chainSyncSerialisedTr]
    blockFetchTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchTr]
    blockFetchSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchSerialisedTr]
    txSubmissionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
    configureTracers trConfig docTTxSubmissionNode [txSubmissionTr]
    txSubmission2Tracer  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    configureTracers trConfig docTTxSubmission2Node [txSubmission2Tracer]
    pure $ NtN.Tracers
      { NtN.tChainSyncTracer = Tracer $
          traceWith chainSyncTracer
      , NtN.tChainSyncSerialisedTracer = Tracer $
          traceWith chainSyncSerialisedTr
      , NtN.tBlockFetchTracer = Tracer $
          traceWith blockFetchTr
      , NtN.tBlockFetchSerialisedTracer = Tracer $
          traceWith blockFetchSerialisedTr
      , NtN.tTxSubmissionTracer = Tracer $
          traceWith txSubmissionTr
      , NtN.tTxSubmission2Tracer = Tracer $
          traceWith txSubmission2Tracer
      }

mkDiffusionTracers
  :: Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (Diffusion.Tracers RemoteAddress NodeToNodeVersion
        LocalAddress NodeToClientVersion IO)
mkDiffusionTracers  trBase trForward mbTrEKG _trDataPoint trConfig = do
    dtMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Mux"
                namesForMux
                severityMux
                allPublic
    configureTracers trConfig docMuxRemote [dtMuxTr]
    dtLocalMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
    configureTracers trConfig docMuxLocal [dtLocalMuxTr]
    dtHandshakeTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
    configureTracers trConfig docHandshake [dtHandshakeTr]
    dtLocalHandshakeTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
    configureTracers trConfig docLocalHandshake [dtLocalHandshakeTr]
    dtDiffusionInitializationTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
    configureTracers trConfig docDiffusionInit [dtDiffusionInitializationTr]
    dtLedgerPeersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LedgerPeers"
                namesForLedgerPeers
                severityLedgerPeers
                allPublic
    configureTracers trConfig docLedgerPeers [dtLedgerPeersTr]
    pure $ Diffusion.Tracers
       { Diffusion.dtMuxTracer                     = Tracer $
           traceWith dtMuxTr
       , Diffusion.dtHandshakeTracer               = Tracer $
           traceWith dtHandshakeTr
       , Diffusion.dtLocalMuxTracer                = Tracer $
           traceWith dtLocalMuxTr
       , Diffusion.dtLocalHandshakeTracer          = Tracer $
           traceWith dtLocalHandshakeTr
       , Diffusion.dtDiffusionInitializationTracer = Tracer $
           traceWith dtDiffusionInitializationTr
       , Diffusion.dtLedgerPeersTracer             = Tracer $
           traceWith dtLedgerPeersTr
       }

mkDiffusionTracersExtra  :: forall p2p.
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NetworkP2PMode p2p
  -> IO (Diffusion.ExtraTracers p2p)
mkDiffusionTracersExtra trBase trForward mbTrEKG _trDataPoint trConfig EnabledP2PMode = do
    localRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalRootPeers"
      namesForLocalRootPeers
      severityLocalRootPeers
      allPublic
    configureTracers trConfig docLocalRootPeers [localRootPeersTr]
    publicRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PublicRootPeers"
      namesForPublicRootPeers
      severityPublicRootPeers
      allPublic
    configureTracers trConfig docPublicRootPeers [publicRootPeersTr]
    peerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PeerSelection"
      namesForPeerSelection
      severityPeerSelection
      allPublic
    configureTracers trConfig docPeerSelection [peerSelectionTr]
    debugPeerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "DebugPeerSelection"
      namesForDebugPeerSelection
      severityDebugPeerSelection
      allPublic
    configureTracers trConfig docDebugPeerSelection [debugPeerSelectionTr]
    debugPeerSelectionResponderTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "DebugPeerSelectionResponder"
      namesForDebugPeerSelection
      severityDebugPeerSelection
      allPublic
    configureTracers trConfig docDebugPeerSelection [debugPeerSelectionResponderTr]
    peerSelectionCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PeerSelectionCounters"
      namesForPeerSelectionCounters
      severityPeerSelectionCounters
      allPublic
    configureTracers trConfig docPeerSelectionCounters [peerSelectionCountersTr]
    peerSelectionActionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PeerSelectionActions"
      namesForPeerSelectionActions
      severityPeerSelectionActions
      allPublic
    configureTracers trConfig docPeerSelectionActions [peerSelectionActionsTr]
    connectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "ConnectionManager"
      namesForConnectionManager
      severityConnectionManager
      allPublic
    configureTracers trConfig docConnectionManager [connectionManagerTr]
    connectionManagerTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "ConnectionManagerTransition"
      (namesForConnectionManagerTransition @RemoteAddress)
      severityConnectionManagerTransition
      allPublic
    configureTracers trConfig docConnectionManagerTransition [connectionManagerTransitionsTr]
    serverTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "Server"
      namesForServer
      severityServer
      allPublic
    configureTracers trConfig docServer [serverTr]
    inboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "InboundGovernor"
      namesForInboundGovernor
      severityInboundGovernor
      allPublic
    configureTracers trConfig docInboundGovernorRemote [inboundGovernorTr]
    inboundGovernorTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "InboundGovernorTransition"
      namesForInboundGovernorTransition
      severityInboundGovernorTransition
      allPublic
    configureTracers trConfig docInboundGovernorRemote [inboundGovernorTr]
    localConnectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalConnectionManager"
      namesForConnectionManager
      severityConnectionManager
      allPublic
    configureTracers trConfig docConnectionManager [localConnectionManagerTr]
    localServerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalServer"
      namesForServer
      severityServer
      allPublic
    configureTracers trConfig docServer [localServerTr]
    localInboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalInboundGovernor"
      namesForInboundGovernor
      severityInboundGovernor
      allPublic
    configureTracers trConfig docInboundGovernorLocal [localInboundGovernorTr]
    pure $ Diffusion.P2PTracers P2P.TracersExtra
             { P2P.dtTraceLocalRootPeersTracer = Tracer $
                 traceWith localRootPeersTr
             , P2P.dtTracePublicRootPeersTracer = Tracer $
                 traceWith publicRootPeersTr
             , P2P.dtTracePeerSelectionTracer = Tracer $
                 traceWith peerSelectionTr
             , P2P.dtDebugPeerSelectionInitiatorTracer = Tracer $
                 traceWith debugPeerSelectionTr
             , P2P.dtDebugPeerSelectionInitiatorResponderTracer = Tracer $
                 traceWith debugPeerSelectionResponderTr
             , P2P.dtTracePeerSelectionCounters = Tracer $
                 traceWith peerSelectionCountersTr
             , P2P.dtPeerSelectionActionsTracer = Tracer $
                 traceWith peerSelectionActionsTr
             , P2P.dtConnectionManagerTracer = Tracer $
                 traceWith connectionManagerTr
             , P2P.dtConnectionManagerTransitionTracer = Tracer $
                 traceWith connectionManagerTransitionsTr
             , P2P.dtServerTracer = Tracer $
                 traceWith serverTr
             , P2P.dtInboundGovernorTracer = Tracer $
                 traceWith inboundGovernorTr
             , P2P.dtInboundGovernorTransitionTracer = Tracer $
                 traceWith inboundGovernorTransitionsTr
             , P2P.dtLocalConnectionManagerTracer =  Tracer $
                 traceWith localConnectionManagerTr
             , P2P.dtLocalServerTracer = Tracer $
                 traceWith localServerTr
             , P2P.dtLocalInboundGovernorTracer = Tracer $
                 traceWith localInboundGovernorTr
             }

mkDiffusionTracersExtra trBase trForward mbTrEKG _trDataPoint trConfig DisabledP2PMode = do
    dtIpSubscriptionTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
    configureTracers trConfig docIPSubscription [dtIpSubscriptionTr]
    dtDnsSubscriptionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
    configureTracers trConfig docDNSSubscription [dtDnsSubscriptionTr]
    dtDnsResolverTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
    configureTracers trConfig docDNSResolver [dtDnsResolverTr]
    dtErrorPolicyTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
    configureTracers trConfig docErrorPolicy [dtErrorPolicyTr]
    dtLocalErrorPolicyTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
    configureTracers trConfig docLocalErrorPolicy [dtLocalErrorPolicyTr]
    dtAcceptPolicyTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
    configureTracers trConfig docAcceptPolicy [dtAcceptPolicyTr]
    pure $ Diffusion.NonP2PTracers NonP2P.TracersExtra
       { NonP2P.dtIpSubscriptionTracer = Tracer $
           traceWith dtIpSubscriptionTr
       , NonP2P.dtDnsSubscriptionTracer = Tracer $
           traceWith dtDnsSubscriptionTr
       , NonP2P.dtDnsResolverTracer = Tracer $
           traceWith dtDnsResolverTr
       , NonP2P.dtErrorPolicyTracer = Tracer $
           traceWith dtErrorPolicyTr
       , NonP2P.dtLocalErrorPolicyTracer = Tracer $
           traceWith dtLocalErrorPolicyTr
       , NonP2P.dtAcceptPolicyTracer = Tracer $
           traceWith dtAcceptPolicyTr
       }
