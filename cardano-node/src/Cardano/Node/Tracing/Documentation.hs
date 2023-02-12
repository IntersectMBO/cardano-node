{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Tracing.Documentation
  ( TraceDocumentationCmd (..)
  , parseTraceDocumentationCmd
  , runTraceDocumentationCmd
  , docTracers
  ) where

import           Control.Exception (SomeException)
import           Data.Aeson.Types (ToJSON)
import           Data.Proxy (Proxy (..))
import qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import qualified Options.Applicative as Opt

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types ()

import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.Formatting ()
import qualified Cardano.Node.Tracing.StateRep as SR
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.ChainDB
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.Diffusion ()
-- import           Cardano.Node.Tracing.Tracers.ForgingThreadStats (ForgeThreadStats,
--                    forgeThreadStats, ForgingStats)
import           Cardano.Node.Tracing.Tracers.KESInfo ()
import           Cardano.Node.Tracing.Tracers.NodeToClient ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NonP2P ()
import           Cardano.Node.Tracing.Tracers.P2P ()
import           Cardano.Node.Tracing.Tracers.Peer
import           Cardano.Node.Tracing.Tracers.Shutdown ()
import           Cardano.Node.Tracing.Tracers.Startup ()

import           Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query (Query, ShowQuery)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId,
                   LedgerSupportsMempool)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import qualified Ouroboros.Consensus.Node.Run as Consensus
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
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (SubscriptionTrace (..))
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)

import           Cardano.Api.Orphans ()

data TraceDocumentationCmd
  = TraceDocumentationCmd
    { tdcConfigFile :: FilePath
    , tdcOutput     :: FilePath
    }

parseTraceDocumentationCmd :: Opt.Parser TraceDocumentationCmd
parseTraceDocumentationCmd =
  Opt.subparser
    (mconcat
     [ Opt.commandGroup "Miscellaneous commands"
     , Opt.metavar "trace-documentation"
     , Opt.hidden
     , Opt.command "trace-documentation" $
       Opt.info
         (TraceDocumentationCmd
           <$> Opt.strOption
               ( Opt.long "config"
                 <> Opt.metavar "NODE-CONFIGURATION"
                 <> Opt.help "Configuration file for the cardano-node"
               )
           <*> Opt.strOption
               ( Opt.long "output-file"
                 <> Opt.metavar "FILE"
                 <> Opt.help "Generated documentation output file"
               )
           Opt.<**> Opt.helper)
       $ mconcat [ Opt.progDesc "Generate the trace documentation" ]
     ]
    )

deriving instance Generic UnversionedProtocol
deriving instance Generic UnversionedProtocolData

instance ToJSON UnversionedProtocol
instance ToJSON UnversionedProtocolData

runTraceDocumentationCmd
  :: TraceDocumentationCmd
  -> IO ()
runTraceDocumentationCmd TraceDocumentationCmd{..} = do
  docTracers
    tdcConfigFile tdcOutput (Proxy @(CardanoBlock StandardCrypto))
                            (Proxy @(NtN.ConnectionId LocalAddress))
                            (Proxy @(NtN.ConnectionId NtN.RemoteAddress))

-- Have to repeat the construction of the tracers here,
-- as the tracers are behind old tracer interface after construction in mkDispatchTracers.
-- Can be changed, when old tracers have gone
docTracers :: forall blk peer remotePeer.
  ( TraceConstraints blk
  , InspectLedger blk
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  , Consensus.SerialiseNodeToNodeConstraints blk
  , LogFormatting peer
  , LogFormatting remotePeer
  , Show (BlockNodeToClientVersion blk)
  , Show (BlockNodeToNodeVersion blk)
  , Show remotePeer
  , Show peer
  , Show (ForgeStateUpdateError blk)
  , Show (CannotForge blk)
  , ShowQuery (BlockQuery blk)
  )
  => FilePath
  -> FilePath
  -> Proxy blk
  -> Proxy peer
  -> Proxy remotePeer
  -> IO ()
docTracers configFileName outputFileName _ _ _ = do
    trConfig      <- readConfigurationWithDefault configFileName defaultCardanoConfig
    let trBase    :: Trace IO FormattedMessage = docTracer (Stdout MachineFormat)
        trForward :: Trace IO FormattedMessage = docTracer Forwarder
        trDataPoint = docTracerDatapoint DatapointBackend
        mbTrEKG   :: Maybe (Trace IO FormattedMessage) = Just (docTracer EKGBackend)

    -- NodeInfo tracer
    nodeInfoTr <- mkDataPointTracer
                    trDataPoint
    configureTracers trConfig  [nodeInfoTr]
    nodeInfoTrDoc <- documentTracer (nodeInfoTr :: Trace IO NodeInfo)

    nodeStartupInfoTr <- mkDataPointTracer
                trDataPoint
    configureTracers trConfig [nodeStartupInfoTr]
    nodeStartupInfoTrDoc <- documentTracer
                      (nodeStartupInfoTr :: Trace IO NodeStartupInfo)

    -- State tracer
    stateTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["NodeState"]
    configureTracers trConfig [stateTr]
    stateTrDoc <- documentTracer (stateTr :: Trace IO SR.NodeState)

    --  Peers tracer

    peersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Peers", "List"]
    configureTracers trConfig [peersTr]
    peersTrDoc <- documentTracer (peersTr :: Trace IO  [PeerT blk])

    -- Resource tracer
    resourcesTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                []
    configureTracers trConfig [resourcesTr]
    resourcesTrDoc <- documentTracer (resourcesTr :: Trace IO ResourceStats)

    -- Startup tracer
    startupTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup"]
    configureTracers trConfig [startupTr]
    startupTrDoc <- documentTracer (startupTr :: Trace IO (StartupTrace blk))

    shutdownTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Shutdown"]
    configureTracers trConfig  [shutdownTr]
    shutdownTrDoc <- documentTracer (shutdownTr :: Trace IO ShutdownTrace)


    chainDBTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                ["ChainDB"]
                withAddedToCurrentChainEmptyLimited
    configureTracers trConfig [chainDBTr]
    chainDBTrDoc <- documentTracer (chainDBTr ::
                      Trace IO (ChainDB.TraceEvent blk))

    replayBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainDB", "ReplayBlock"]
    configureTracers trConfig [replayBlockTr]
    replayBlockTrDoc <- documentTracer (replayBlockTr :: Trace IO ReplayBlockStats)

-- Consensus tracers

    chainSyncClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Client"]
    configureTracers trConfig [chainSyncClientTr]
    chainSyncClientTrDoc <- documentTracer (chainSyncClientTr ::
      (Trace IO (BlockFetch.TraceLabelPeer
                  (ConnectionId RemoteAddress)
                  (TraceChainSyncClientEvent blk))))

    chainSyncServerHeaderTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerHeader"]
    configureTracers trConfig [chainSyncServerHeaderTr]
    chainSyncServerHeaderTrDoc <- documentTracer (chainSyncServerHeaderTr ::
      (Trace IO (TraceChainSyncServerEvent blk)))

    chainSyncServerBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerBlock"]
    configureTracers trConfig [chainSyncServerBlockTr]
    chainSyncServerBlockTrDoc <- documentTracer (chainSyncServerBlockTr ::
      (Trace IO (TraceChainSyncServerEvent blk)))

    blockFetchDecisionTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Decision"]
    configureTracers trConfig [blockFetchDecisionTr]
    blockFetchDecisionTrDoc <- documentTracer (blockFetchDecisionTr ::
       Trace IO [BlockFetch.TraceLabelPeer
                                      remotePeer
                                      (FetchDecision [Point (Header blk)])])

    blockFetchClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Client"]
    configureTracers trConfig [blockFetchClientTr]
    blockFetchClientTrDoc <- documentTracer (blockFetchClientTr ::
      Trace IO (BlockFetch.TraceLabelPeer
                  remotePeer
                  (BlockFetch.TraceFetchClientState (Header blk))))

    -- TODO Yup
    -- blockFetchClientMetricsTr <- do
    --         foldMTraceM calculateBlockFetchClientMetrics initialClientMetrics
    --             (metricsFormatter ""
    --               (mkMetricsTracer mbTrEKG))
    -- clientMetricsDoc <- documentTracer (blockFetchClientMetricsTr ::
    --    Trace IO ClientMetrics)

    blockFetchServerTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Server"]
    configureTracers trConfig [blockFetchServerTr]
    blockFetchServerTrDoc <- documentTracer (blockFetchServerTr ::
      Trace IO (TraceBlockFetchServerEvent blk))

    forgeKESInfoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "KESInfo"]
    configureTracers trConfig [forgeKESInfoTr]
    forgeKESInfoTrDoc <- documentTracer (forgeKESInfoTr ::
      Trace IO (Consensus.TraceLabelCreds HotKey.KESInfo))

    txInboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxInbound"]
    configureTracers trConfig [txInboundTr]
    txInboundTrDoc <- documentTracer (txInboundTr ::
      Trace IO (BlockFetch.TraceLabelPeer
                  remotePeer
                  (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))

    txOutboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxOutbound"]
    configureTracers trConfig [txOutboundTr]
    txOutboundTrDoc <- documentTracer (txOutboundTr ::
      Trace IO (BlockFetch.TraceLabelPeer
                  remotePeer
                  (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))

    localTxSubmissionServerTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "LocalServer"]
    configureTracers trConfig [localTxSubmissionServerTr]
    localTxSubmissionServerTrDoc <- documentTracer (localTxSubmissionServerTr ::
      Trace IO (TraceLocalTxSubmissionServerEvent blk))

    mempoolTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Mempool"]
    configureTracers trConfig [mempoolTr]
    mempoolTrDoc <- documentTracer (mempoolTr ::
      Trace IO (TraceEventMempool blk))

    forgeTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "Loop"]
    configureTracers trConfig [forgeTr]
    forgeTrDoc <- documentTracer (forgeTr ::
      Trace IO (ForgeTracerType blk))

    -- TODO YUP
    -- forgeTr' <-  mkCardanoTracer'
    --             trBase trForward mbTrEKG
    --             ["Forge", "Loop"]
    --             forgeThreadStats
    -- configureTracers trConfig [forgeTr']
    -- forgeThreadStatsTrDoc <- documentTracer' forgeThreadStats (forgeTr' ::
    --   Trace IO (ForgeTracerType blk))

    blockchainTimeTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockchainTime"]
    configureTracers trConfig [blockchainTimeTr]
    blockchainTimeTrDoc <- documentTracer (blockchainTimeTr ::
      Trace IO (TraceBlockchainTimeEvent RelativeTime))

-- Node to client

    keepAliveClientTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net"]
    configureTracers trConfig [keepAliveClientTr]
    keepAliveClientTrDoc <- documentTracer (keepAliveClientTr ::
      Trace IO (TraceKeepAliveClient peer))

    chainSyncTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Local"]
    configureTracers trConfig [chainSyncTr]
    chainSyncTrDoc <- documentTracer (chainSyncTr ::
      Trace IO
        (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync (Header blk) (Point blk) (Tip blk)))))

    txMonitorTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        ["TxSubmission", "MonitorClient"]
    configureTracers trConfig [txMonitorTr]
    txMonitorTrDoc <- documentTracer (txMonitorTr ::
      Trace IO
        (BlockFetch.TraceLabelPeer
           peer
           (TraceSendRecv
              (LTM.LocalTxMonitor
                 (GenTxId blk) (GenTx blk) SlotNo))))

    txSubmissionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Local"]
    configureTracers trConfig [txSubmissionTr]
    txSubmissionTrDoc <- documentTracer (txSubmissionTr ::
      Trace IO
         (BlockFetch.TraceLabelPeer
            peer
            (TraceSendRecv
               (LTS.LocalTxSubmission
                  (GenTx blk) (ApplyTxErr blk)))))

    stateQueryTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
               ["StateQueryServer"]
    configureTracers trConfig [stateQueryTr]
    stateQueryTrDoc <- documentTracer (stateQueryTr ::
      Trace IO
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (LocalStateQuery blk (Point blk) (Query blk)))))

-- Node to Node

    chainSyncNodeTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Remote"]
    configureTracers trConfig [chainSyncNodeTr]
    chainSyncNodeTrDoc <- documentTracer (chainSyncNodeTr ::
      Trace IO (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync (Header blk) (Point blk) (Tip blk)))))

    chainSyncSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Remote", "Serialised"]
    configureTracers trConfig [chainSyncSerialisedTr]
    chainSyncSerialisedTrDoc <- documentTracer (chainSyncSerialisedTr ::
      Trace IO (BlockFetch.TraceLabelPeer peer (TraceSendRecv
            (ChainSync (Header blk) (Point blk) (Tip blk)))))

    blockFetchTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote"]
    configureTracers trConfig [blockFetchTr]
    blockFetchTrDoc <- documentTracer (blockFetchTr ::
      Trace IO
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch blk (Point blk)))))

    blockFetchSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote", "Serialised"]
    configureTracers trConfig [blockFetchSerialisedTr]
    blockFetchSerialisedTrDoc <- documentTracer (blockFetchSerialisedTr ::
      Trace IO
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch blk (Point blk)))))

    txSubmission2Tr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Remote"]
    configureTracers trConfig [txSubmission2Tr]
    txSubmission2TrDoc <- documentTracer (txSubmission2Tr ::
      Trace IO
        (BlockFetch.TraceLabelPeer peer
          (TraceSendRecv
            (TxSubmission2 (GenTxId blk) (GenTx blk)))))

-- Diffusion
    dtMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote"]
    configureTracers trConfig [dtMuxTr]
    dtMuxTrDoc <- documentTracer (dtMuxTr ::
      Trace IO (WithMuxBearer (ConnectionId RemoteAddress) MuxTrace))

    dtLocalMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local"]
    configureTracers trConfig [dtLocalMuxTr]
    dtLocalMuxTrDoc <- documentTracer (dtLocalMuxTr ::
      Trace IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace))

    dtHandshakeTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Remote"]
    configureTracers trConfig [dtHandshakeTr]
    dtHandshakeTrDoc <- documentTracer (dtHandshakeTr ::
      Trace IO (NtN.HandshakeTr NtN.RemoteAddress NtN.NodeToNodeVersion))

    dtLocalHandshakeTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                 ["Net", "Handshake", "Local"]
    configureTracers trConfig [dtLocalHandshakeTr]
    dtLocalHandshakeTrDoc <- documentTracer (dtLocalHandshakeTr ::
      Trace IO
        (NtC.HandshakeTr LocalAddress NtC.NodeToClientVersion))

    dtDiffusionInitializationTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup", "DiffusionInit"]
    configureTracers trConfig [dtDiffusionInitializationTr]
    dtDiffusionInitializationTrDoc <- documentTracer (dtDiffusionInitializationTr ::
      Trace IO (Diffusion.DiffusionTracer Socket.SockAddr LocalAddress))

    dtLedgerPeersTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Peers", "Ledger"]
    configureTracers trConfig [dtLedgerPeersTr]
    dtLedgerPeersTrDoc <- documentTracer (dtLedgerPeersTr ::
      Trace IO TraceLedgerPeers)

-- DiffusionTracersExtra P2P

    localRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "LocalRoot"]
    configureTracers trConfig [localRootPeersTr]
    localRootPeersTrDoc <- documentTracer (localRootPeersTr ::
      Trace IO (TraceLocalRootPeers RemoteAddress SomeException))

    publicRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "PublicRoot"]
    configureTracers trConfig [publicRootPeersTr]
    publicRootPeersTrDoc <- documentTracer (publicRootPeersTr ::
      Trace IO TracePublicRootPeers)

    peerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Selection"]
    configureTracers trConfig [peerSelectionTr]
    peerSelectionTrDoc <- documentTracer (peerSelectionTr ::
      Trace IO (TracePeerSelection Socket.SockAddr))

    debugPeerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Initiator"]
    configureTracers trConfig [debugPeerSelectionTr]
    debugPeerSelectionTrDoc <- documentTracer (debugPeerSelectionTr ::
      Trace IO (DebugPeerSelection Socket.SockAddr))

    debugPeerSelectionResponderTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Responder"]
    configureTracers trConfig [debugPeerSelectionResponderTr]
    debugPeerSelectionResponderTrDoc <- documentTracer (debugPeerSelectionResponderTr ::
      Trace IO (DebugPeerSelection Socket.SockAddr))

    peerSelectionCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Counters"]
    configureTracers trConfig [peerSelectionCountersTr]
    peerSelectionCountersTrDoc <- documentTracer (peerSelectionCountersTr ::
      Trace IO PeerSelectionCounters)

    peerSelectionActionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Actions"]
    configureTracers trConfig [peerSelectionActionsTr]
    peerSelectionActionsTrDoc <- documentTracer (peerSelectionActionsTr ::
      Trace IO (PeerSelectionActionsTrace Socket.SockAddr LocalAddress))

    connectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Remote"]
    configureTracers trConfig [connectionManagerTr]
    connectionManagerTrDoc <- documentTracer (connectionManagerTr ::
      Trace IO
        (ConnectionManagerTrace
          Socket.SockAddr
          (ConnectionHandlerTrace UnversionedProtocol UnversionedProtocolData)))

    connectionManagerTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Remote"]
    configureTracers trConfig [connectionManagerTransitionsTr]
    connectionManagerTransitionsTrDoc <- documentTracer (connectionManagerTransitionsTr ::
      Trace IO (ConnectionManager.AbstractTransitionTrace Socket.SockAddr))


    serverTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Remote"]
    configureTracers trConfig [serverTr]
    serverTrDoc <- documentTracer (serverTr ::
      Trace IO (ServerTrace Socket.SockAddr))

    inboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "InboundGovernor", "Remote"]
    configureTracers trConfig [inboundGovernorTr]
    inboundGovernorTrDoc <- documentTracer (inboundGovernorTr ::
      Trace IO (InboundGovernorTrace Socket.SockAddr))

    inboundGovernorTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Remote", "Transition"]
    configureTracers trConfig [inboundGovernorTransitionsTr]
    inboundGovernorTransitionsTrDoc <- documentTracer (inboundGovernorTransitionsTr ::
       Trace IO (InboundGovernor.RemoteTransitionTrace Socket.SockAddr))

    localConnectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "ConnectionManager", "Local"]
    configureTracers trConfig [localConnectionManagerTr]
    localConnectionManagerTrDoc <- documentTracer (localConnectionManagerTr ::
      Trace IO
        (ConnectionManagerTrace
          Socket.SockAddr
          (ConnectionHandlerTrace
            UnversionedProtocol
            UnversionedProtocolData)))

    localServerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Local"]
    configureTracers trConfig [localServerTr]
    localServerTrDoc <- documentTracer (localServerTr ::
      Trace IO (ServerTrace LocalAddress))

    localInboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "InboundGovernor", "Local"]
    configureTracers trConfig [localInboundGovernorTr]
    localInboundGovernorTrDoc <- documentTracer (localInboundGovernorTr ::
      Trace IO (InboundGovernorTrace LocalAddress))

-- -- DiffusionTracersExtra nonP2P

    dtIpSubscriptionTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "IP"]
    configureTracers trConfig [dtIpSubscriptionTr]
    dtIpSubscriptionTrDoc <- documentTracer (dtIpSubscriptionTr ::
      Trace IO (WithIPList (SubscriptionTrace Socket.SockAddr)))

    dtDnsSubscriptionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "DNS"]
    configureTracers trConfig [dtDnsSubscriptionTr]
    dtDnsSubscriptionTrDoc <- documentTracer (dtDnsSubscriptionTr ::
      Trace IO (WithDomainName (SubscriptionTrace Socket.SockAddr)))

    dtDnsResolverTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "DNSResolver"]
    configureTracers trConfig [dtDnsResolverTr]
    dtDnsResolverTrDoc <- documentTracer (dtDnsResolverTr ::
      Trace IO (WithDomainName DnsTrace))

    dtErrorPolicyTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Remote"]
    configureTracers trConfig [dtErrorPolicyTr]
    dtErrorPolicyTrDoc <- documentTracer (dtErrorPolicyTr ::
      Trace IO (WithAddr Socket.SockAddr ErrorPolicyTrace))

    dtLocalErrorPolicyTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Local"]
    configureTracers trConfig [dtLocalErrorPolicyTr]
    dtLocalErrorPolicyTrDoc <- documentTracer (dtLocalErrorPolicyTr ::
      Trace IO (WithAddr LocalAddress ErrorPolicyTrace))

    dtAcceptPolicyTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "AcceptPolicy"]
    configureTracers trConfig [dtAcceptPolicyTr]
    dtAcceptPolicyTrDoc <- documentTracer (dtAcceptPolicyTr ::
      Trace IO NtN.AcceptConnectionsPolicyTrace)

    let bl =   nodeInfoTrDoc
            <> stateTrDoc
            <> nodeStartupInfoTrDoc
            <> resourcesTrDoc
            <> startupTrDoc
            <> shutdownTrDoc
            <> peersTrDoc
            <> chainDBTrDoc
            <> replayBlockTrDoc
-- Consensus
            <> chainSyncClientTrDoc
            <> chainSyncServerHeaderTrDoc
            <> chainSyncServerBlockTrDoc
            <> blockFetchDecisionTrDoc
            <> blockFetchClientTrDoc
            <> blockFetchServerTrDoc
            <> forgeKESInfoTrDoc
            <> txInboundTrDoc
            <> txOutboundTrDoc
            <> localTxSubmissionServerTrDoc
            <> mempoolTrDoc
            <> forgeTrDoc
--            <> forgeThreadStatsTrDoc
            <> blockchainTimeTrDoc
-- NodeToClient
            <> keepAliveClientTrDoc
            <> chainSyncTrDoc
            <> txMonitorTrDoc
            <> txSubmissionTrDoc
            <> stateQueryTrDoc
-- Node to Node
            <> chainSyncNodeTrDoc
            <> chainSyncSerialisedTrDoc
            <> blockFetchTrDoc
            <> blockFetchSerialisedTrDoc
            <> txSubmission2TrDoc
-- Diffusion
            <> dtMuxTrDoc
            <> dtLocalMuxTrDoc
            <> dtHandshakeTrDoc
            <> dtLocalHandshakeTrDoc
            <> dtDiffusionInitializationTrDoc
            <> dtLedgerPeersTrDoc
-- DiffusionTracersExtra P2P
            <> localRootPeersTrDoc
            <> publicRootPeersTrDoc
            <> peerSelectionTrDoc
            <> debugPeerSelectionTrDoc
            <> debugPeerSelectionResponderTrDoc
            <> peerSelectionCountersTrDoc
            <> peerSelectionActionsTrDoc
            <> connectionManagerTrDoc
            <> connectionManagerTransitionsTrDoc
            <> serverTrDoc
            <> inboundGovernorTrDoc
            <> inboundGovernorTransitionsTrDoc
            <> localConnectionManagerTrDoc
            <> localServerTrDoc
            <> localInboundGovernorTrDoc
-- DiffusionTracersExtra nonP2P
            <> dtIpSubscriptionTrDoc
            <> dtDnsSubscriptionTrDoc
            <> dtDnsResolverTrDoc
            <> dtErrorPolicyTrDoc
            <> dtLocalErrorPolicyTrDoc
            <> dtAcceptPolicyTrDoc

    res <- docuResultsToText bl trConfig
    T.writeFile outputFileName res
    pure ()
