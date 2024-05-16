{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- needs different instances on ghc8 and on ghc9

module Cardano.Node.Tracing.Tracers
  ( mkDispatchTracers
  ) where

import           Cardano.Logging
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Cardano.Node.Queries (NodeKernelData)
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Tracing.Consistency (checkNodeTraceConfiguration')
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Peers
import qualified Cardano.Node.Tracing.StateRep as SR
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.ChainDB
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.Diffusion ()
import           Cardano.Node.Tracing.Tracers.ForgingThreadStats (forgeThreadStats)
import           Cardano.Node.Tracing.Tracers.KESInfo
import           Cardano.Node.Tracing.Tracers.NodeToClient ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NonP2P ()
import           Cardano.Node.Tracing.Tracers.P2P ()
import           Cardano.Node.Tracing.Tracers.Peer ()
import           Cardano.Node.Tracing.Tracers.Shutdown ()
import           Cardano.Node.Tracing.Tracers.Startup ()
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import           Ouroboros.Consensus.Node (NetworkP2PMode (..))
import           Ouroboros.Consensus.Node.GSM
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import           Ouroboros.Network.NodeToClient (LocalAddress)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad (unless)
import           "contra-tracer" Control.Tracer (Tracer (..))
import           Data.Proxy (Proxy (..))
import           Network.Mux.Trace (TraceLabelPeer (..))

import           Trace.Forward.Utils.DataPoint (DataPoint)

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
  , LogFormatting (TraceGsmEvent (Tip blk))
  , MetaTrace (TraceGsmEvent (Tip blk))
  )
  => NodeKernelData blk
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NetworkP2PMode p2p
  -> SomeConsensusProtocol
  -> IO (Tracers (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk p2p)
mkDispatchTracers nodeKernel trBase trForward mbTrEKG trDataPoint trConfig enableP2P p = do

    configReflection <- emptyConfigReflection

    !nodeInfoDP <- mkDataPointTracer trDataPoint
    configureTracers configReflection trConfig [nodeInfoDP]

    !nodeStartupInfoDP <- mkDataPointTracer trDataPoint
    configureTracers configReflection trConfig [nodeStartupInfoDP]

    !nodeStateDP <- mkDataPointTracer trDataPoint
    configureTracers configReflection trConfig [nodeStateDP]

    !stateTr <- mkCardanoTracer trBase trForward mbTrEKG ["NodeState"]
    configureTracers configReflection trConfig [stateTr]

    !nodePeersDP <- mkDataPointTracer trDataPoint
    configureTracers configReflection trConfig [nodePeersDP]

    !peersTr <- mkCardanoTracer trBase trForward mbTrEKG
                    ["Net", "Peers", "List"]
    configureTracers configReflection trConfig [peersTr]

    !resourcesTr <- mkCardanoTracer trBase trForward mbTrEKG []
    configureTracers configReflection trConfig [resourcesTr]

    !startupTr <- mkCardanoTracer trBase trForward mbTrEKG ["Startup"]
    configureTracers configReflection trConfig [startupTr]

    !shutdownTr <- mkCardanoTracer trBase trForward mbTrEKG ["Shutdown"]
    configureTracers configReflection trConfig  [shutdownTr]

    !chainDBTr <- mkCardanoTracer' trBase trForward mbTrEKG ["ChainDB"]
                                    withAddedToCurrentChainEmptyLimited
    configureTracers configReflection trConfig [chainDBTr]
    -- Filter out replayed blocks for this tracer
    let chainDBTr' = filterTrace
                      (\case (_, ChainDB.TraceLedgerReplayEvent
                                            LedgerDB.ReplayedBlock {}) -> False
                             (_, _) -> True)
                      chainDBTr


    !replayBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainDB", "ReplayBlock"]
    configureTracers configReflection trConfig [replayBlockTr]

    -- This tracer handles replayed blocks specially
    !replayBlockTr' <- withReplayedBlock replayBlockTr


    !consensusTr <-
      mkConsensusTracers configReflection trBase trForward mbTrEKG trDataPoint trConfig nodeKernel

    !nodeToClientTr <-
      mkNodeToClientTracers configReflection trBase trForward mbTrEKG trDataPoint trConfig

    !nodeToNodeTr <-
      mkNodeToNodeTracers configReflection trBase trForward mbTrEKG trDataPoint trConfig

    !(diffusionTr :: Diffusion.Tracers
                    RemoteAddress
                    NodeToNodeVersion
                    LocalAddress
                    NodeToClientVersion
                    IO) <-
      mkDiffusionTracers configReflection trBase trForward mbTrEKG trDataPoint trConfig

    !diffusionTrExtra <-
      mkDiffusionTracersExtra configReflection trBase trForward mbTrEKG trDataPoint trConfig enableP2P

    traceTracerInfo trBase trForward configReflection

    let warnings = checkNodeTraceConfiguration' trConfig
    unless (null warnings) $
      traceConfigWarnings trBase trForward warnings

    traceEffectiveConfiguration trBase trForward trConfig

    pure Tracers
      {
        chainDBTracer = Tracer (traceWith chainDBTr')
                      <> Tracer (traceWith replayBlockTr')
                      <> Tracer (SR.traceNodeStateChainDB p nodeStateDP)
      , consensusTracers = consensusTr
      , nodeToClientTracers = nodeToClientTr
      , nodeToNodeTracers = nodeToNodeTr
      , diffusionTracers = diffusionTr
      , diffusionTracersExtra = diffusionTrExtra
      , startupTracer   = Tracer (traceWith startupTr)
                         <> Tracer (SR.traceNodeStateStartup nodeStateDP)
      , shutdownTracer  = Tracer (traceWith shutdownTr)
                         <> Tracer (SR.traceNodeStateShutdown nodeStateDP)
      , nodeInfoTracer  = Tracer (traceWith nodeInfoDP)
      , nodeStartupInfoTracer = Tracer (traceWith nodeStartupInfoDP)
      , nodeStateTracer = Tracer (traceWith stateTr)
                          <> Tracer (traceWith nodeStateDP)
      , resourcesTracer = Tracer (traceWith resourcesTr)
      , peersTracer     = Tracer (traceWith peersTr)
                          <> Tracer (traceNodePeers nodePeersDP)
    }

mkConsensusTracers :: forall blk.
  ( Consensus.RunNode blk
  , TraceConstraints blk
  , LogFormatting (TraceLabelPeer
                    (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  , LogFormatting (TraceGsmEvent (Tip blk))
  , MetaTrace (TraceGsmEvent (Tip blk))
  )
  => ConfigReflection
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NodeKernelData blk
  -> IO (Consensus.Tracers IO (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk)
mkConsensusTracers configReflection trBase trForward mbTrEKG _trDataPoint trConfig nodeKernel = do
    !chainSyncClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Client"]
    configureTracers configReflection trConfig [chainSyncClientTr]
    !chainSyncServerHeaderTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerHeader"]
    configureTracers configReflection trConfig [chainSyncServerHeaderTr]

    -- Special chainSync server metrics
    -- any server header event advances the counter
    let chainSyncServerHeaderMetricsTr =
           contramap
              (const
                (FormattedMetrics
                  [CounterM "ChainSync.HeadersServed" Nothing]))
              (mkMetricsTracer mbTrEKG)

    !chainSyncServerBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerBlock"]
    configureTracers configReflection trConfig [chainSyncServerBlockTr]

    !blockFetchDecisionTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Decision"]
    configureTracers configReflection trConfig [blockFetchDecisionTr]

    !blockFetchClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Client"]
    configureTracers configReflection trConfig [blockFetchClientTr]

    -- Special blockFetch client metrics, send directly to EKG
    !blockFetchClientMetricsTr <- do
        tr1 <- foldTraceM calculateBlockFetchClientMetrics initialClientMetrics
                    (metricsFormatter
                      (mkMetricsTracer mbTrEKG))
        pure $ filterTrace (\ (_, TraceLabelPeer _ m) -> case m of
                                              BlockFetch.CompletedBlockFetch {} -> True
                                              _ -> False)
                 tr1

    !blockFetchServerTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Server"]
    configureTracers configReflection trConfig [blockFetchServerTr]

    !forgeKESInfoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "StateInfo"]
    configureTracers configReflection trConfig [forgeKESInfoTr]

    !txInboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxInbound"]
    configureTracers configReflection trConfig [txInboundTr]

    !txOutboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxOutbound"]
    configureTracers configReflection trConfig [txOutboundTr]

    !localTxSubmissionServerTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "LocalServer"]
    configureTracers configReflection trConfig [localTxSubmissionServerTr]

    !mempoolTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Mempool"]
    configureTracers configReflection trConfig [mempoolTr]

    !forgeTr    <- mkCardanoTracer'
                trBase trForward mbTrEKG
                ["Forge", "Loop"]
                (forgeTracerTransform nodeKernel)
    configureTracers configReflection trConfig [forgeTr]

    !forgeThreadStatsTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                ["Forge", "ThreadStats"]
                forgeThreadStats
    configureTracers configReflection trConfig [forgeThreadStatsTr]

    !blockchainTimeTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockchainTime"]
    configureTracers configReflection trConfig [blockchainTimeTr]

    !keepAliveClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net"]
    configureTracers configReflection trConfig [keepAliveClientTr]

    !consensusStartupErrorTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "Startup"]

    !consensusGsmTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "GSM"]

    configureTracers configReflection trConfig [consensusStartupErrorTr]

    pure $ Consensus.Tracers
      { Consensus.chainSyncClientTracer = Tracer $
          traceWith chainSyncClientTr
      , Consensus.chainSyncServerHeaderTracer = Tracer $
            traceWith chainSyncServerHeaderTr
           <> traceWith chainSyncServerHeaderMetricsTr
      , Consensus.chainSyncServerBlockTracer = Tracer $
          traceWith chainSyncServerBlockTr
      , Consensus.blockFetchDecisionTracer = Tracer $
          traceWith blockFetchDecisionTr
      , Consensus.blockFetchClientTracer = Tracer $
          traceWith blockFetchClientTr
           <> traceWith blockFetchClientMetricsTr
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
           Tracer (\(Consensus.TraceLabelCreds _ x) -> traceWith (contramap Left forgeTr) x)
           <>
           Tracer (\(Consensus.TraceLabelCreds _ x) -> traceWith (contramap Left forgeThreadStatsTr) x)
      , Consensus.blockchainTimeTracer = Tracer $
          traceWith blockchainTimeTr
      , Consensus.keepAliveClientTracer = Tracer $
          traceWith keepAliveClientTr
      , Consensus.consensusErrorTracer = Tracer $
          traceWith consensusStartupErrorTr . ConsensusStartupException
      , Consensus.gsmTracer = Tracer $
          traceWith consensusGsmTr
      }

mkNodeToClientTracers :: forall blk.
     Consensus.RunNode blk
  => ConfigReflection
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (NodeToClient.Tracers IO (ConnectionId LocalAddress) blk DeserialiseFailure)
mkNodeToClientTracers configReflection trBase trForward mbTrEKG _trDataPoint trConfig = do
    !chainSyncTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        ["ChainSync", "Local"]
    configureTracers configReflection trConfig [chainSyncTr]

    !txMonitorTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        ["TxSubmission", "MonitorClient"]
    configureTracers configReflection trConfig [txMonitorTr]

    !txSubmissionTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        ["TxSubmission", "Local"]
    configureTracers configReflection trConfig [txSubmissionTr]

    !stateQueryTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        ["StateQueryServer"]
    configureTracers configReflection trConfig [stateQueryTr]

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
  => ConfigReflection
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (NodeToNode.Tracers IO (ConnectionId RemoteAddress) blk DeserialiseFailure)
mkNodeToNodeTracers configReflection trBase trForward mbTrEKG _trDataPoint trConfig = do

    !chainSyncTracer <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Remote"]
    configureTracers configReflection trConfig [chainSyncTracer]

    !chainSyncSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Remote", "Serialised"]
    configureTracers configReflection trConfig [chainSyncSerialisedTr]

    !blockFetchTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote"]
    configureTracers configReflection trConfig [blockFetchTr]

    !blockFetchSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote", "Serialised"]
    configureTracers configReflection trConfig [blockFetchSerialisedTr]

    !txSubmission2Tracer  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Remote"]
    configureTracers configReflection trConfig [txSubmission2Tracer]

    pure $ NtN.Tracers
      { NtN.tChainSyncTracer = Tracer $
          traceWith chainSyncTracer
      , NtN.tChainSyncSerialisedTracer = Tracer $
          traceWith chainSyncSerialisedTr
      , NtN.tBlockFetchTracer = Tracer $
          traceWith blockFetchTr
      , NtN.tBlockFetchSerialisedTracer = Tracer $
          traceWith blockFetchSerialisedTr
      , NtN.tTxSubmission2Tracer = Tracer $
          traceWith txSubmission2Tracer
      }

mkDiffusionTracers
  :: ConfigReflection
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (Diffusion.Tracers RemoteAddress NodeToNodeVersion
        LocalAddress NodeToClientVersion IO)
mkDiffusionTracers configReflection trBase trForward mbTrEKG _trDataPoint trConfig = do

    !dtMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote"]
    configureTracers configReflection trConfig [dtMuxTr]

    !dtLocalMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local"]
    configureTracers configReflection trConfig [dtLocalMuxTr]

    !dtHandshakeTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Remote"]
    configureTracers configReflection trConfig [dtHandshakeTr]

    !dtLocalHandshakeTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Local"]
    configureTracers configReflection trConfig [dtLocalHandshakeTr]

    !dtDiffusionInitializationTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup", "DiffusionInit"]
    configureTracers configReflection trConfig [dtDiffusionInitializationTr]

    pure $ Diffusion.Tracers
       { Diffusion.dtMuxTracer                     = Tracer $
           traceWith dtMuxTr
       , Diffusion.dtLocalMuxTracer                = Tracer $
           traceWith dtLocalMuxTr
       , Diffusion.dtHandshakeTracer               = Tracer $
           traceWith dtHandshakeTr
       , Diffusion.dtLocalHandshakeTracer          = Tracer $
           traceWith dtLocalHandshakeTr
       , Diffusion.dtDiffusionTracer               = Tracer $
           traceWith dtDiffusionInitializationTr
       }

mkDiffusionTracersExtra  :: forall p2p.
     ConfigReflection
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NetworkP2PMode p2p
  -> IO (Diffusion.ExtraTracers p2p)
mkDiffusionTracersExtra configReflection trBase trForward mbTrEKG _trDataPoint trConfig EnabledP2PMode = do

    !localRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "LocalRoot"]
    configureTracers configReflection trConfig [localRootPeersTr]

    !publicRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "PublicRoot"]
    configureTracers configReflection trConfig [publicRootPeersTr]

    !peerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Selection"]
    configureTracers configReflection trConfig [peerSelectionTr]

    !debugPeerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Initiator"]
    configureTracers configReflection trConfig [debugPeerSelectionTr]

    !debugPeerSelectionResponderTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Responder"]
    configureTracers configReflection trConfig [debugPeerSelectionResponderTr]

    !peerSelectionCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection"]
    configureTracers configReflection trConfig [peerSelectionCountersTr]

    !churnCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Churn"]
    configureTracers configReflection trConfig [churnCountersTr]

    !peerSelectionActionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Actions"]
    configureTracers configReflection trConfig [peerSelectionActionsTr]

    !connectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Remote"]
    configureTracers configReflection trConfig [connectionManagerTr]

    !connectionManagerTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Transition"]
    configureTracers configReflection trConfig [connectionManagerTransitionsTr]

    !serverTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Remote"]
    configureTracers configReflection trConfig [serverTr]

    !inboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Remote"]
    configureTracers configReflection trConfig [inboundGovernorTr]

    !localInboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Local"]
    configureTracers configReflection trConfig [localInboundGovernorTr]

    !inboundGovernorTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Transition"]
    configureTracers configReflection trConfig [inboundGovernorTransitionsTr]

    !localConnectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Local"]
    configureTracers configReflection trConfig [localConnectionManagerTr]

    !localServerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Local"]
    configureTracers configReflection trConfig [localServerTr]

    !dtLedgerPeersTr   <- mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "Ledger"]
    configureTracers configReflection trConfig [dtLedgerPeersTr]

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
             , P2P.dtTraceChurnCounters = Tracer $
                 traceWith churnCountersTr
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
             , P2P.dtLocalInboundGovernorTracer = Tracer $
                 traceWith localInboundGovernorTr
             , P2P.dtInboundGovernorTransitionTracer = Tracer $
                 traceWith inboundGovernorTransitionsTr
             , P2P.dtLocalConnectionManagerTracer =  Tracer $
                 traceWith localConnectionManagerTr
             , P2P.dtLocalServerTracer = Tracer $
                 traceWith localServerTr
             , P2P.dtTraceLedgerPeersTracer = Tracer $
                 traceWith dtLedgerPeersTr
             }

mkDiffusionTracersExtra configReflection trBase trForward mbTrEKG _trDataPoint trConfig DisabledP2PMode = do

    !dtIpSubscriptionTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "IP"]
    configureTracers configReflection trConfig [dtIpSubscriptionTr]

    !dtDnsSubscriptionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "DNS"]
    configureTracers configReflection trConfig [dtDnsSubscriptionTr]

    !dtDnsResolverTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "DNSResolver"]
    configureTracers configReflection trConfig [dtDnsResolverTr]

    !dtErrorPolicyTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Remote"]
    configureTracers configReflection trConfig [dtErrorPolicyTr]

    !dtLocalErrorPolicyTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Local"]
    configureTracers configReflection trConfig [dtLocalErrorPolicyTr]

    !dtAcceptPolicyTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "AcceptPolicy"]
    configureTracers configReflection trConfig [dtAcceptPolicyTr]

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
