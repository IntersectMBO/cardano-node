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
{-# OPTIONS_GHC -Wno-orphans #-}
-- needs different instances on ghc8 and on ghc9

module Cardano.Node.Tracing.Tracers
  ( mkDispatchTracers
  ) where

import           Cardano.Logging
import qualified Cardano.Network.Diffusion as Cardano.Diffusion
import           Cardano.Network.NodeToClient (LocalAddress)
import           Cardano.Network.NodeToClient.Version ()
import           Cardano.Network.NodeToNode (RemoteAddress)
import           Cardano.Network.NodeToNode.Version ()
import           Cardano.Node.Queries (NodeKernelData)
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Tracing.Consistency (checkNodeTraceConfiguration')
import           Cardano.Node.Tracing.Formatting ()
import qualified Cardano.Node.Tracing.StateRep as SR
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.ChainDB
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.ForgingStats (calcForgeStats)
import           Cardano.Node.Tracing.Tracers.KESInfo
import           Cardano.Node.Tracing.Tracers.LedgerMetrics ()
import           Cardano.Node.Tracing.Tracers.NodeToClient ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NodeVersion (getNodeVersion)
import           Cardano.Node.Tracing.Tracers.Rpc ()
import           Cardano.Node.Tracing.Tracers.Shutdown ()
import           Cardano.Node.Tracing.Tracers.Startup ()
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import           Ouroboros.Consensus.Node.GSM
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.Diffusion as Diffusion

import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad (unless)
import           "contra-tracer" Control.Tracer (Tracer (..), nullTracer)
import           Cardano.Network.OrphanInstances ()
import           Data.Aeson (ToJSON (..))
import           Data.Proxy (Proxy (..))
import           Network.Mux.Trace (TraceLabelPeer (..))
import qualified Network.Mux.Trace as Mux
import           Network.Mux.Tracing ()


-- | Wrap a tracing effect as a Tracer.
-- 'emit' from contra-tracer returns TracerA, not Tracer; 'arrow' wraps it.
mkT :: Applicative m => (a -> m ()) -> Tracer m a
mkT = arrow . emit

-- | Construct tracers for all system components.
--
mkDispatchTracers
  :: forall blk .
  ( Consensus.RunNode blk
  , TraceConstraints blk
  , LogFormatting (LedgerEvent blk)
  , LogFormatting
    (TraceLabelPeer
      (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  , LogFormatting (TraceGsmEvent (Tip blk))
  , MetaTrace (TraceGsmEvent (Tip blk))
  , ToJSON (HeaderHash blk)
  )
  => NodeKernelData blk
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (Tracers RemoteAddress LocalAddress blk IO)

mkDispatchTracers nodeKernel trBase trForward mbTrEKG trDataPoint trConfig = do

    configReflection <- emptyConfigReflection

    !nodeInfoDP <- mkDataPointTracer trDataPoint
    configureTracers configReflection trConfig [nodeInfoDP]

    !nodeStartupInfoDP <- mkDataPointTracer trDataPoint
    configureTracers configReflection trConfig [nodeStartupInfoDP]

    !nodeStateDP <- mkDataPointTracer trDataPoint
    configureTracers configReflection trConfig [nodeStateDP]

    !stateTr <- mkCardanoTracer trBase trForward mbTrEKG ["NodeState"]
    configureTracers configReflection trConfig [stateTr]

    !resourcesTr <- mkCardanoTracer trBase trForward mbTrEKG []
    configureTracers configReflection trConfig [resourcesTr]

    !ledgerMetricsTr <- mkCardanoTracer trBase trForward mbTrEKG []
    configureTracers configReflection trConfig [ledgerMetricsTr]

    !startupTr <- mkCardanoTracer trBase trForward mbTrEKG ["Startup"]
    configureTracers configReflection trConfig [startupTr]

    !shutdownTr <- mkCardanoTracer trBase trForward mbTrEKG ["Shutdown"]
    configureTracers configReflection trConfig  [shutdownTr]

    !chainDBTr <- mkCardanoTracer' trBase trForward mbTrEKG ["ChainDB"]
                                    withAddedToCurrentChainEmptyLimited
    configureTracers configReflection trConfig [chainDBTr]

    !nodeVersionTr <- mkCardanoTracer trBase trForward mbTrEKG ["Version"]
    configureTracers configReflection trConfig  [nodeVersionTr]

    -- Filter out replayed blocks for this tracer
    let chainDBTr' = filterTrace
                      (\case (_, ChainDB.TraceLedgerDBEvent
                                            (LedgerDB.LedgerReplayEvent (LedgerDB.TraceReplayProgressEvent
                                                                        (LedgerDB.ReplayedBlock {})))) -> False
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

    !(diffusionTr :: Cardano.Diffusion.CardanoTracers IO) <-
      mkDiffusionTracers configReflection trBase trForward mbTrEKG trDataPoint trConfig

    !churnModeTr <- mkCardanoTracer trBase trForward mbTrEKG ["Net", "PeerSelection", "ChurnMode"]
    configureTracers configReflection trConfig [churnModeTr]

    !rpcTr <- mkCardanoTracer trBase trForward mbTrEKG ["RPC"]
    configureTracers configReflection trConfig [rpcTr]

    traceTracerInfo trBase trForward configReflection

    let warnings = checkNodeTraceConfiguration' trConfig
    unless (null warnings) $
      traceConfigWarnings trBase trForward warnings

    traceEffectiveConfiguration trBase trForward trConfig

    traceWith nodeVersionTr getNodeVersion

    pure Tracers
      {
        chainDBTracer = mkT(traceWith chainDBTr')
                      <> mkT(traceWith replayBlockTr')
                      <> mkT(SR.traceNodeStateChainDB nodeStateDP)
      , consensusTracers = consensusTr
      , churnModeTracer = mkT(traceWith churnModeTr)
      , nodeToClientTracers = nodeToClientTr
      , nodeToNodeTracers = nodeToNodeTr
      , diffusionTracers = diffusionTr
      , startupTracer   = mkT(traceWith startupTr)
                         <> mkT(SR.traceNodeStateStartup nodeStateDP)
      , shutdownTracer  = mkT(traceWith shutdownTr)
                         <> mkT(SR.traceNodeStateShutdown nodeStateDP)
      , nodeInfoTracer  = mkT(traceWith nodeInfoDP)
      , nodeStartupInfoTracer = mkT(traceWith nodeStartupInfoDP)
      , nodeStateTracer = mkT(traceWith stateTr)
                          <> mkT(traceWith nodeStateDP)
      , nodeVersionTracer = mkT(traceWith nodeVersionTr)
      , resourcesTracer = mkT(traceWith resourcesTr)
      , ledgerMetricsTracer = mkT(traceWith ledgerMetricsTr)
      , rpcTracer = mkT(traceWith rpcTr)
    }

mkConsensusTracers :: forall blk.
  ( Consensus.RunNode blk
  , TraceConstraints blk
  , LogFormatting (TraceLabelPeer
                    (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  , LogFormatting (TraceGsmEvent (Tip blk))
  , MetaTrace (TraceGsmEvent (Tip blk))
  , ToJSON (HeaderHash blk)
  )
  => ConfigReflection
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NodeKernelData blk
  -> IO (Consensus.Tracers IO (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk)
mkConsensusTracers configReflection trBase trForward mbTrEKG _trDataPoint trConfig _nodeKernel = do
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

    !consensusSanityCheckTr <- mkCardanoTracer
                 trBase trForward mbTrEKG
                 ["Consensus", "SanityCheck"]
    configureTracers configReflection trConfig [consensusSanityCheckTr]

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
        tr1 <- foldTraceM (\cm lc -> pure . calculateBlockFetchClientMetrics cm lc) initialClientMetrics
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

    !servedBlockLatestTr <- servedBlockLatest mbTrEKG

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

    !forgeTr    <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "Loop"]
    configureTracers configReflection trConfig [forgeTr]

    !forgeStatsTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                ["Forge", "Stats"]
                calcForgeStats
    configureTracers configReflection trConfig [forgeStatsTr]

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
    configureTracers configReflection trConfig [consensusStartupErrorTr]

    !consensusGddTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "GDD"]
    configureTracers configReflection trConfig [consensusGddTr]

    !consensusGsmTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "GSM"]
    configureTracers configReflection trConfig [consensusGsmTr]

    !consensusCsjTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "CSJ"]
    configureTracers configReflection trConfig [consensusCsjTr]

    !consensusKesAgentTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "KESAgent"]
    configureTracers configReflection trConfig [consensusKesAgentTr]

    !consensusDbfTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "DevotedBlockFetch"]
    configureTracers configReflection trConfig [consensusDbfTr]

    !txLogicTracer  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["txLogic", "Remote"]
    configureTracers configReflection trConfig [txLogicTracer]

    !txCountersTracer  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["txCounters", "Remote"]
    configureTracers configReflection trConfig [txCountersTracer]

    pure $ Consensus.Tracers
      { Consensus.chainSyncClientTracer = mkT$
          traceWith chainSyncClientTr
      , Consensus.chainSyncServerHeaderTracer = mkT$
            traceWith chainSyncServerHeaderTr
           <> traceWith chainSyncServerHeaderMetricsTr
      , Consensus.chainSyncServerBlockTracer = mkT$
          traceWith chainSyncServerBlockTr
      , Consensus.consensusSanityCheckTracer = mkT$
          traceWith consensusSanityCheckTr
      , Consensus.blockFetchDecisionTracer = mkT$
          traceWith blockFetchDecisionTr
      , Consensus.blockFetchClientTracer = mkT$
          traceWith blockFetchClientTr
           <> traceWith blockFetchClientMetricsTr
      , Consensus.blockFetchServerTracer = mkT$
          traceWith blockFetchServerTr
          <> traceWith servedBlockLatestTr
      , Consensus.forgeStateInfoTracer = mkT$
          traceWith (traceAsKESInfo (Proxy @blk) forgeKESInfoTr)
      , Consensus.gddTracer = mkT$
          traceWith consensusGddTr
      , Consensus.txInboundTracer = mkT$
           traceWith txInboundTr
      , Consensus.txOutboundTracer = mkT$
          traceWith txOutboundTr
      , Consensus.localTxSubmissionServerTracer = mkT$
          traceWith localTxSubmissionServerTr
      , Consensus.mempoolTracer = mkT$
          traceWith mempoolTr
      , Consensus.forgeTracer =
           mkT (\(Consensus.TraceLabelCreds _ x) -> traceWith forgeTr x)
           <>
           mkT (\(Consensus.TraceLabelCreds _ x) -> traceWith forgeStatsTr x)
      , Consensus.blockchainTimeTracer = mkT$
          traceWith blockchainTimeTr
      , Consensus.keepAliveClientTracer = mkT$
          traceWith keepAliveClientTr
      , Consensus.consensusErrorTracer = mkT$
          traceWith consensusStartupErrorTr . ConsensusStartupException
      , Consensus.gsmTracer = mkT$
          traceWith consensusGsmTr
      , Consensus.csjTracer = mkT$
          traceWith consensusCsjTr
      , Consensus.dbfTracer = mkT$
          traceWith consensusDbfTr
      , Consensus.kesAgentTracer = mkT$
          traceWith consensusKesAgentTr
      , Consensus.txLogicTracer = mkT$
          traceWith txLogicTracer
      , Consensus.txCountersTracer = mkT$
          traceWith txCountersTracer
      , Consensus.perasCertDiffusionInboundTracer = nullTracer
      , Consensus.perasCertDiffusionOutboundTracer = nullTracer
      , Consensus.perasVoteDiffusionInboundTracer = nullTracer
      , Consensus.perasVoteDiffusionOutboundTracer = nullTracer
      , Consensus.perasCertInclusionTracer = nullTracer
      , Consensus.perasVoteForgingTracer = nullTracer
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
      { NtC.tChainSyncTracer = mkT$
          traceWith chainSyncTr
      , NtC.tTxMonitorTracer = mkT$
          traceWith txMonitorTr
      , NtC.tTxSubmissionTracer = mkT$
          traceWith txSubmissionTr
      , NtC.tStateQueryTracer = mkT$
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
  -> IO (NodeToNode.Tracers IO RemoteAddress blk DeserialiseFailure)
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

    !keepAliveTracer  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["KeepAlive", "Remote"]
    configureTracers configReflection trConfig [keepAliveTracer]

    !peerSharingTracer  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["PeerSharing", "Remote"]
    configureTracers configReflection trConfig [peerSharingTracer]

    pure $ NtN.Tracers
      { NtN.tChainSyncTracer = mkT$
          traceWith chainSyncTracer
      , NtN.tChainSyncSerialisedTracer = mkT$
          traceWith chainSyncSerialisedTr
      , NtN.tBlockFetchTracer = mkT$
          traceWith blockFetchTr
      , NtN.tBlockFetchSerialisedTracer = mkT$
          traceWith blockFetchSerialisedTr
      , NtN.tTxSubmission2Tracer = mkT$
          traceWith txSubmission2Tracer
      , NtN.tPerasCertDiffusionTracer = nullTracer
      , NtN.tPerasVoteDiffusionTracer = nullTracer
      , NtN.tKeepAliveTracer = mkT$
          traceWith keepAliveTracer
      , NtN.tPeerSharingTracer = mkT$
          traceWith peerSharingTracer
      }

mkDiffusionTracers ::
    ( LogFormatting
        ( Mux.WithBearer
            (ConnectionId RemoteAddress)
            Mux.Trace
        )
    ) =>
    ConfigReflection ->
    Trace IO FormattedMessage ->
    Trace IO FormattedMessage ->
    Maybe (Trace IO FormattedMessage) ->
    Trace IO DataPoint ->
    TraceConfig ->
    IO (Cardano.Diffusion.CardanoTracers IO)
mkDiffusionTracers configReflection trBase trForward mbTrEKG _trDataPoint trConfig = do

    !dtMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote"]
    configureTracers configReflection trConfig [dtMuxTr]

    !dtChannelTracer <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote", "Channel"]
    configureTracers configReflection trConfig [dtChannelTracer]

    !dtBearerTracer <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote", "Bearer"]
    configureTracers configReflection trConfig [dtBearerTracer]

    !dtHandshakeTracer <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Remote"]
    configureTracers configReflection trConfig [dtHandshakeTracer]

    !dtLocalMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local"]
    configureTracers configReflection trConfig [dtLocalMuxTr]

    !dtLocalChannelTracer <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local", "Channel"]
    configureTracers configReflection trConfig [dtLocalChannelTracer]

    !dtLocalBearerTracer <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local", "Bearer"]
    configureTracers configReflection trConfig [dtLocalBearerTracer]

    !dtLocalHandshakeTracer <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Local"]
    configureTracers configReflection trConfig [dtLocalHandshakeTracer]

    !dtDiffusionInitializationTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup", "DiffusionInit"]
    configureTracers configReflection trConfig [dtDiffusionInitializationTr]

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

    !peerSelectionCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection"]
    configureTracers configReflection trConfig [peerSelectionCountersTr]

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
      trBase trForward Nothing -- never conflate metrics of the same name with those originating from `connectionManagerTr`
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

    !dtDnsTr  <- mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "DNS"]
    configureTracers configReflection trConfig [dtDnsTr]

    pure $ Diffusion.Tracers
       { Diffusion.dtMuxTracer = mkT$
           traceWith dtMuxTr
       , Diffusion.dtChannelTracer = mkT$
           traceWith dtChannelTracer
       , Diffusion.dtBearerTracer = mkT$
           traceWith dtBearerTracer
       , Diffusion.dtHandshakeTracer = mkT$
           traceWith dtHandshakeTracer
       , Diffusion.dtLocalMuxTracer = mkT$
           traceWith dtLocalMuxTr
       , Diffusion.dtLocalChannelTracer = mkT$
           traceWith dtLocalChannelTracer
       , Diffusion.dtLocalBearerTracer = mkT$
           traceWith dtLocalBearerTracer
       , Diffusion.dtLocalHandshakeTracer = mkT$
           traceWith dtLocalHandshakeTracer
       , Diffusion.dtDiffusionTracer = mkT$
           traceWith dtDiffusionInitializationTr
       , Diffusion.dtTraceLocalRootPeersTracer = mkT$
           traceWith localRootPeersTr
       , Diffusion.dtTracePublicRootPeersTracer = mkT$
           traceWith publicRootPeersTr
       , Diffusion.dtTracePeerSelectionTracer = mkT$
           traceWith peerSelectionTr
       , Diffusion.dtDebugPeerSelectionTracer = mkT$
           traceWith debugPeerSelectionTr
       , Diffusion.dtTracePeerSelectionCounters = mkT$
           traceWith peerSelectionCountersTr
       , Diffusion.dtPeerSelectionActionsTracer = mkT$
           traceWith peerSelectionActionsTr
       , Diffusion.dtConnectionManagerTracer = mkT$
           traceWith connectionManagerTr
       , Diffusion.dtConnectionManagerTransitionTracer = mkT$
           traceWith connectionManagerTransitionsTr
       , Diffusion.dtServerTracer = mkT$
           traceWith serverTr
       , Diffusion.dtInboundGovernorTracer = mkT$
           traceWith inboundGovernorTr
       , Diffusion.dtLocalInboundGovernorTracer = mkT$
           traceWith localInboundGovernorTr
       , Diffusion.dtInboundGovernorTransitionTracer = mkT$
           traceWith inboundGovernorTransitionsTr
       , Diffusion.dtLocalConnectionManagerTracer = mkT$
           traceWith localConnectionManagerTr
       , Diffusion.dtLocalServerTracer = mkT$
           traceWith localServerTr
       , Diffusion.dtTraceLedgerPeersTracer = mkT$
           traceWith dtLedgerPeersTr
       , Diffusion.dtDnsTracer = mkT$
           traceWith dtDnsTr
       }
