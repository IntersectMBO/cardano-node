{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- needs different instances on ghc8 and on ghc9

module Cardano.Node.Tracing.Tracers
  ( mkDispatchTracers
  ) where

import qualified Cardano.Network.Diffusion as Cardano.Diffusion
import           Cardano.Network.NodeToClient (LocalAddress)
import           Cardano.Network.NodeToClient.Version ()
import           Cardano.Network.NodeToNode (RemoteAddress)
import           Cardano.Network.NodeToNode.Version ()
import           Cardano.Network.OrphanInstances ()
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
import           Data.Aeson (ToJSON (..))
import           Data.Proxy (Proxy (..))
import           Network.Mux.Trace (TraceLabelPeer (..))
import qualified Network.Mux.Trace as Mux
import           Network.Mux.Tracing ()

import           Hermod.Tracing hiding (mkTracer)
import           Hermod.Tracing.API.Tracer (mkTracer)


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

    !stateTr <- mkHermodTracer trBase trForward mbTrEKG ["NodeState"]
    configureTracers configReflection trConfig [stateTr]

    !resourcesTr <- mkHermodTracer trBase trForward mbTrEKG []
    configureTracers configReflection trConfig [resourcesTr]

    !ledgerMetricsTr <- mkHermodTracer trBase trForward mbTrEKG []
    configureTracers configReflection trConfig [ledgerMetricsTr]

    !startupTr <- mkHermodTracer trBase trForward mbTrEKG ["Startup"]
    configureTracers configReflection trConfig [startupTr]

    !shutdownTr <- mkHermodTracer trBase trForward mbTrEKG ["Shutdown"]
    configureTracers configReflection trConfig  [shutdownTr]

    !chainDBTr <- mkHermodTracer' trBase trForward mbTrEKG ["ChainDB"]
                                    withAddedToCurrentChainEmptyLimited
    configureTracers configReflection trConfig [chainDBTr]

    !nodeVersionTr <- mkHermodTracer trBase trForward mbTrEKG ["Version"]
    configureTracers configReflection trConfig  [nodeVersionTr]

    -- Filter out replayed blocks for this tracer
    let chainDBTr' = filterTrace
                      (\case (_, ChainDB.TraceLedgerDBEvent
                                            (LedgerDB.LedgerReplayEvent (LedgerDB.TraceReplayProgressEvent
                                                                        (LedgerDB.ReplayedBlock {})))) -> False
                             (_, _) -> True)
                      chainDBTr


    !replayBlockTr <- mkHermodTracer
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

    !churnModeTr <- mkHermodTracer trBase trForward mbTrEKG ["Net", "PeerSelection", "ChurnMode"]
    configureTracers configReflection trConfig [churnModeTr]

    !rpcTr <- mkHermodTracer trBase trForward mbTrEKG ["RPC"]
    configureTracers configReflection trConfig [rpcTr]

    traceTracerInfo trBase trForward configReflection

    let warnings = checkNodeTraceConfiguration' trConfig
    unless (null warnings) $
      traceConfigWarnings trBase trForward warnings

    traceEffectiveConfiguration trBase trForward trConfig

    traceWith nodeVersionTr getNodeVersion

    pure Tracers
      {
        chainDBTracer = mkTracer (traceWith chainDBTr')
                      <> mkTracer (traceWith replayBlockTr')
                      <> mkTracer (SR.traceNodeStateChainDB nodeStateDP)
      , consensusTracers = consensusTr
      , churnModeTracer = mkTracer (traceWith churnModeTr)
      , nodeToClientTracers = nodeToClientTr
      , nodeToNodeTracers = nodeToNodeTr
      , diffusionTracers = diffusionTr
      , startupTracer   = mkTracer (traceWith startupTr)
                         <> mkTracer (SR.traceNodeStateStartup nodeStateDP)
      , shutdownTracer  = mkTracer (traceWith shutdownTr)
                         <> mkTracer (SR.traceNodeStateShutdown nodeStateDP)
      , nodeInfoTracer  = mkTracer (traceWith nodeInfoDP)
      , nodeStartupInfoTracer = mkTracer (traceWith nodeStartupInfoDP)
      , nodeStateTracer = mkTracer (traceWith stateTr)
                          <> mkTracer (traceWith nodeStateDP)
      , nodeVersionTracer = mkTracer (traceWith nodeVersionTr)
      , resourcesTracer = mkTracer (traceWith resourcesTr)
      , ledgerMetricsTracer = mkTracer (traceWith ledgerMetricsTr)
      , rpcTracer = mkTracer (traceWith rpcTr)
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
    !chainSyncClientTr  <- mkHermodTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Client"]
    configureTracers configReflection trConfig [chainSyncClientTr]
    !chainSyncServerHeaderTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerHeader"]
    configureTracers configReflection trConfig [chainSyncServerHeaderTr]

    -- Special chainSync server metrics
    -- any server header event advances the counter
    let chainSyncServerHeaderMetricsTr =
           contramap
              (const
                (FormattedMetrics
                  [CounterM "ChainSync.HeadersServed" CounterIncrement]))
              (mkMetricsTracer mbTrEKG)

    !chainSyncServerBlockTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerBlock"]
    configureTracers configReflection trConfig [chainSyncServerBlockTr]

    !consensusSanityCheckTr <- mkHermodTracer
                 trBase trForward mbTrEKG
                 ["Consensus", "SanityCheck"]
    configureTracers configReflection trConfig [consensusSanityCheckTr]

    !blockFetchDecisionTr  <- mkHermodTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Decision"]
    configureTracers configReflection trConfig [blockFetchDecisionTr]

    !blockFetchClientTr  <- mkHermodTracer
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

    !blockFetchServerTr  <- mkHermodTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Server"]
    configureTracers configReflection trConfig [blockFetchServerTr]

    !servedBlockLatestTr <- servedBlockLatest mbTrEKG

    !forgeKESInfoTr  <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Forge", "StateInfo"]
    configureTracers configReflection trConfig [forgeKESInfoTr]

    !txInboundTr  <- mkHermodTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxInbound"]
    configureTracers configReflection trConfig [txInboundTr]

    !txOutboundTr  <- mkHermodTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxOutbound"]
    configureTracers configReflection trConfig [txOutboundTr]

    !localTxSubmissionServerTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "LocalServer"]
    configureTracers configReflection trConfig [localTxSubmissionServerTr]

    !mempoolTr   <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Mempool"]
    configureTracers configReflection trConfig [mempoolTr]

    !forgeTr    <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Forge", "Loop"]
    configureTracers configReflection trConfig [forgeTr]

    !forgeStatsTr <- mkHermodTracer'
                trBase trForward mbTrEKG
                ["Forge", "Stats"]
                calcForgeStats
    configureTracers configReflection trConfig [forgeStatsTr]

    !blockchainTimeTr   <- mkHermodTracer
                trBase trForward mbTrEKG
                ["BlockchainTime"]
    configureTracers configReflection trConfig [blockchainTimeTr]

    !keepAliveClientTr  <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Net"]
    configureTracers configReflection trConfig [keepAliveClientTr]

    !consensusStartupErrorTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Consensus", "Startup"]
    configureTracers configReflection trConfig [consensusStartupErrorTr]

    !consensusGddTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Consensus", "GDD"]
    configureTracers configReflection trConfig [consensusGddTr]

    !consensusGsmTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Consensus", "GSM"]
    configureTracers configReflection trConfig [consensusGsmTr]

    !consensusCsjTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Consensus", "CSJ"]
    configureTracers configReflection trConfig [consensusCsjTr]

    !consensusKesAgentTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Consensus", "KESAgent"]
    configureTracers configReflection trConfig [consensusKesAgentTr]

    !consensusDbfTr <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Consensus", "DevotedBlockFetch"]
    configureTracers configReflection trConfig [consensusDbfTr]

    !txLogicTracer  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["txLogic", "Remote"]
    configureTracers configReflection trConfig [txLogicTracer]

    !txCountersTracer  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["txCounters", "Remote"]
    configureTracers configReflection trConfig [txCountersTracer]

    !txPerasCertIn  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Peras", "Cert", "Inbound"]
    configureTracers configReflection trConfig [txPerasCertIn]

    !txPerasCertOut  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Peras", "Cert", "Outbound"]
    configureTracers configReflection trConfig [txPerasCertOut]

    !txPerasVoteIn  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Peras", "Vote", "Inbound"]
    configureTracers configReflection trConfig [txPerasVoteIn]

    !txPerasVoteOut  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Peras", "Vote", "Outbound"]
    configureTracers configReflection trConfig [txPerasVoteOut]

    pure $ Consensus.Tracers
      { Consensus.chainSyncClientTracer = mkTracer $
          traceWith chainSyncClientTr
      , Consensus.chainSyncServerHeaderTracer = mkTracer $
            traceWith chainSyncServerHeaderTr
           <> traceWith chainSyncServerHeaderMetricsTr
      , Consensus.chainSyncServerBlockTracer = mkTracer $
          traceWith chainSyncServerBlockTr
      , Consensus.consensusSanityCheckTracer = mkTracer $
          traceWith consensusSanityCheckTr
      , Consensus.blockFetchDecisionTracer = mkTracer $
          traceWith blockFetchDecisionTr
      , Consensus.blockFetchClientTracer = mkTracer $
          traceWith blockFetchClientTr
           <> traceWith blockFetchClientMetricsTr
      , Consensus.blockFetchServerTracer = mkTracer $
          traceWith blockFetchServerTr
          <> traceWith servedBlockLatestTr
      , Consensus.forgeStateInfoTracer = mkTracer $
          traceWith (traceAsKESInfo (Proxy @blk) forgeKESInfoTr)
      , Consensus.gddTracer = mkTracer $
          traceWith consensusGddTr
      , Consensus.txInboundTracer = mkTracer $
           traceWith txInboundTr
      , Consensus.txOutboundTracer = mkTracer $
          traceWith txOutboundTr
      , Consensus.localTxSubmissionServerTracer = mkTracer $
          traceWith localTxSubmissionServerTr
      , Consensus.mempoolTracer = mkTracer $
          traceWith mempoolTr
      , Consensus.forgeTracer =
           mkTracer (\(Consensus.TraceLabelCreds _ x) -> traceWith forgeTr x)
           <>
           mkTracer (\(Consensus.TraceLabelCreds _ x) -> traceWith forgeStatsTr x)
      , Consensus.blockchainTimeTracer = mkTracer $
          traceWith blockchainTimeTr
      , Consensus.keepAliveClientTracer = mkTracer $
          traceWith keepAliveClientTr
      , Consensus.consensusErrorTracer = mkTracer $
          traceWith consensusStartupErrorTr . ConsensusStartupException
      , Consensus.gsmTracer = mkTracer $
          traceWith consensusGsmTr
      , Consensus.csjTracer = mkTracer $
          traceWith consensusCsjTr
      , Consensus.dbfTracer = mkTracer $
          traceWith consensusDbfTr
      , Consensus.kesAgentTracer = mkTracer $
          traceWith consensusKesAgentTr
      , Consensus.txLogicTracer = mkTracer $
          traceWith txLogicTracer
      , Consensus.txCountersTracer = mkTracer $
          traceWith txCountersTracer
      , Consensus.perasCertDiffusionInboundTracer = mkTracer $ traceWith txPerasCertIn
      , Consensus.perasCertDiffusionOutboundTracer = mkTracer $ traceWith txPerasCertOut
      , Consensus.perasVoteDiffusionInboundTracer = mkTracer $ traceWith txPerasVoteIn
      , Consensus.perasVoteDiffusionOutboundTracer = mkTracer $ traceWith txPerasVoteOut
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
      mkHermodTracer
        trBase trForward mbTrEKG
        ["ChainSync", "Local"]
    configureTracers configReflection trConfig [chainSyncTr]

    !txMonitorTr <-
      mkHermodTracer
        trBase trForward mbTrEKG
        ["TxSubmission", "MonitorClient"]
    configureTracers configReflection trConfig [txMonitorTr]

    !txSubmissionTr <-
      mkHermodTracer
        trBase trForward mbTrEKG
        ["TxSubmission", "Local"]
    configureTracers configReflection trConfig [txSubmissionTr]

    !stateQueryTr <-
      mkHermodTracer
        trBase trForward mbTrEKG
        ["StateQueryServer"]
    configureTracers configReflection trConfig [stateQueryTr]

    pure $ NtC.Tracers
      { NtC.tChainSyncTracer = mkTracer $
          traceWith chainSyncTr
      , NtC.tTxMonitorTracer = mkTracer $
          traceWith txMonitorTr
      , NtC.tTxSubmissionTracer = mkTracer $
          traceWith txSubmissionTr
      , NtC.tStateQueryTracer = mkTracer $
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

    !chainSyncTracer <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Remote"]
    configureTracers configReflection trConfig [chainSyncTracer]

    !chainSyncSerialisedTr <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Remote", "Serialised"]
    configureTracers configReflection trConfig [chainSyncSerialisedTr]

    !blockFetchTr  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote"]
    configureTracers configReflection trConfig [blockFetchTr]

    !blockFetchSerialisedTr <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote", "Serialised"]
    configureTracers configReflection trConfig [blockFetchSerialisedTr]

    !txSubmission2Tracer  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Remote"]
    configureTracers configReflection trConfig [txSubmission2Tracer]

    !keepAliveTracer  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["KeepAlive", "Remote"]
    configureTracers configReflection trConfig [keepAliveTracer]

    !peerSharingTracer  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["PeerSharing", "Remote"]
    configureTracers configReflection trConfig [peerSharingTracer]

    !txPerasCertDiffusion  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Peras", "Cert", "Remote"]
    configureTracers configReflection trConfig [txPerasCertDiffusion]

    !txPerasVoteDiffusion  <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Peras", "Vote", "Remote"]
    configureTracers configReflection trConfig [txPerasVoteDiffusion]

    pure $ NtN.Tracers
      { NtN.tChainSyncTracer = mkTracer $
          traceWith chainSyncTracer
      , NtN.tChainSyncSerialisedTracer = mkTracer $
          traceWith chainSyncSerialisedTr
      , NtN.tBlockFetchTracer = mkTracer $
          traceWith blockFetchTr
      , NtN.tBlockFetchSerialisedTracer = mkTracer $
          traceWith blockFetchSerialisedTr
      , NtN.tTxSubmission2Tracer = mkTracer $
          traceWith txSubmission2Tracer
      , NtN.tKeepAliveTracer = mkTracer $
          traceWith keepAliveTracer
      , NtN.tPeerSharingTracer = mkTracer $
          traceWith peerSharingTracer
      , NtN.tPerasCertDiffusionTracer = mkTracer $
          traceWith txPerasCertDiffusion
      , NtN.tPerasVoteDiffusionTracer = mkTracer $
          traceWith txPerasVoteDiffusion
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

    !dtMuxTr   <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote"]
    configureTracers configReflection trConfig [dtMuxTr]

    !dtChannelTracer <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote", "Channel"]
    configureTracers configReflection trConfig [dtChannelTracer]

    !dtBearerTracer <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote", "Bearer"]
    configureTracers configReflection trConfig [dtBearerTracer]

    !dtHandshakeTracer <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Remote"]
    configureTracers configReflection trConfig [dtHandshakeTracer]

    !dtLocalMuxTr   <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local"]
    configureTracers configReflection trConfig [dtLocalMuxTr]

    !dtLocalChannelTracer <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local", "Channel"]
    configureTracers configReflection trConfig [dtLocalChannelTracer]

    !dtLocalBearerTracer <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local", "Bearer"]
    configureTracers configReflection trConfig [dtLocalBearerTracer]

    !dtLocalHandshakeTracer <- mkHermodTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Local"]
    configureTracers configReflection trConfig [dtLocalHandshakeTracer]

    !dtDiffusionInitializationTr   <-  mkHermodTracer
                trBase trForward mbTrEKG
                ["Startup", "DiffusionInit"]
    configureTracers configReflection trConfig [dtDiffusionInitializationTr]

    !localRootPeersTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "LocalRoot"]
    configureTracers configReflection trConfig [localRootPeersTr]

    !publicRootPeersTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "PublicRoot"]
    configureTracers configReflection trConfig [publicRootPeersTr]

    !peerSelectionTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Selection"]
    configureTracers configReflection trConfig [peerSelectionTr]

    !debugPeerSelectionTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Initiator"]
    configureTracers configReflection trConfig [debugPeerSelectionTr]

    !peerSelectionCountersTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection"]
    configureTracers configReflection trConfig [peerSelectionCountersTr]

    !peerSelectionActionsTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Actions"]
    configureTracers configReflection trConfig [peerSelectionActionsTr]

    !connectionManagerTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Remote"]
    configureTracers configReflection trConfig [connectionManagerTr]

    !connectionManagerTransitionsTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Transition"]
    configureTracers configReflection trConfig [connectionManagerTransitionsTr]

    !serverTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Remote"]
    configureTracers configReflection trConfig [serverTr]

    !inboundGovernorTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Remote"]
    configureTracers configReflection trConfig [inboundGovernorTr]

    !localInboundGovernorTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Local"]
    configureTracers configReflection trConfig [localInboundGovernorTr]

    !inboundGovernorTransitionsTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Transition"]
    configureTracers configReflection trConfig [inboundGovernorTransitionsTr]

    !localConnectionManagerTr  <-  mkHermodTracer
      trBase trForward Nothing -- never conflate metrics of the same name with those originating from `connectionManagerTr`
      ["Net", "ConnectionManager", "Local"]
    configureTracers configReflection trConfig [localConnectionManagerTr]

    !localServerTr  <-  mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Local"]
    configureTracers configReflection trConfig [localServerTr]

    !dtLedgerPeersTr   <- mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "Ledger"]
    configureTracers configReflection trConfig [dtLedgerPeersTr]

    !dtDnsTr  <- mkHermodTracer
      trBase trForward mbTrEKG
      ["Net", "DNS"]
    configureTracers configReflection trConfig [dtDnsTr]

    pure $ Diffusion.Tracers
       { Diffusion.dtMuxTracer = mkTracer $
           traceWith dtMuxTr
       , Diffusion.dtChannelTracer = mkTracer $
           traceWith dtChannelTracer
       , Diffusion.dtBearerTracer = mkTracer $
           traceWith dtBearerTracer
       , Diffusion.dtHandshakeTracer = mkTracer $
           traceWith dtHandshakeTracer
       , Diffusion.dtLocalMuxTracer = mkTracer $
           traceWith dtLocalMuxTr
       , Diffusion.dtLocalChannelTracer = mkTracer $
           traceWith dtLocalChannelTracer
       , Diffusion.dtLocalBearerTracer = mkTracer $
           traceWith dtLocalBearerTracer
       , Diffusion.dtLocalHandshakeTracer = mkTracer $
           traceWith dtLocalHandshakeTracer
       , Diffusion.dtDiffusionTracer = mkTracer $
           traceWith dtDiffusionInitializationTr
       , Diffusion.dtTraceLocalRootPeersTracer = mkTracer $
           traceWith localRootPeersTr
       , Diffusion.dtTracePublicRootPeersTracer = mkTracer $
           traceWith publicRootPeersTr
       , Diffusion.dtTracePeerSelectionTracer = mkTracer $
           traceWith peerSelectionTr
       , Diffusion.dtDebugPeerSelectionTracer = mkTracer $
           traceWith debugPeerSelectionTr
       , Diffusion.dtTracePeerSelectionCounters = mkTracer $
           traceWith peerSelectionCountersTr
       , Diffusion.dtPeerSelectionActionsTracer = mkTracer $
           traceWith peerSelectionActionsTr
       , Diffusion.dtConnectionManagerTracer = mkTracer $
           traceWith connectionManagerTr
       , Diffusion.dtConnectionManagerTransitionTracer = mkTracer $
           traceWith connectionManagerTransitionsTr
       , Diffusion.dtServerTracer = mkTracer $
           traceWith serverTr
       , Diffusion.dtInboundGovernorTracer = mkTracer $
           traceWith inboundGovernorTr
       , Diffusion.dtLocalInboundGovernorTracer = mkTracer $
           traceWith localInboundGovernorTr
       , Diffusion.dtInboundGovernorTransitionTracer = mkTracer $
           traceWith inboundGovernorTransitionsTr
       , Diffusion.dtLocalConnectionManagerTracer =  mkTracer $
           traceWith localConnectionManagerTr
       , Diffusion.dtLocalServerTracer = mkTracer $
           traceWith localServerTr
       , Diffusion.dtTraceLedgerPeersTracer = mkTracer $
           traceWith dtLedgerPeersTr
       , Diffusion.dtDnsTracer = mkTracer $
           traceWith dtDnsTr
       }
