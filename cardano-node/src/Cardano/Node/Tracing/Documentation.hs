{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Tracing.Documentation
  ( TraceDocumentationCmd (..)
  , parseTraceDocumentationCmd
  , runTraceDocumentationCmd
  , docTracers
  , docTracersFirstPhase
  ) where


import           Cardano.Logging as Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types ()
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import           Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.Formatting ()
import qualified Cardano.Node.Tracing.StateRep as SR
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.ChainDB
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.ConsensusStartupException
import           Cardano.Node.Tracing.Tracers.Diffusion ()
import           Cardano.Node.Tracing.Tracers.ForgingStats (ForgingStats)
import           Cardano.Node.Tracing.Tracers.KESInfo ()
import           Cardano.Node.Tracing.Tracers.LedgerMetrics (LedgerMetrics)
import           Cardano.Node.Tracing.Tracers.NodeToClient ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NodeVersion (NodeVersionTrace)
import           Cardano.Node.Tracing.Tracers.NonP2P ()
import           Cardano.Node.Tracing.Tracers.P2P ()
import           Cardano.Node.Tracing.Tracers.Peer
import           Cardano.Node.Tracing.Tracers.Shutdown ()
import           Cardano.Node.Tracing.Tracers.Startup ()
import qualified Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState as Cardano
import qualified Ouroboros.Cardano.Network.PeerSelection.Governor.Types as Cardano
import qualified Ouroboros.Cardano.Network.PublicRootPeers as Cardano.PublicRootPeers
import           Ouroboros.Consensus.Block.SupportsSanityCheck (SanityCheckIssue)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Genesis.Governor (TraceGDDEvent (..))
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId)
import           Ouroboros.Consensus.Mempool (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping as Jumping
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.GSM (TraceGsmEvent)
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Network.Block (Point (..), Serialised, SlotNo, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.ConnectionManager.Core as ConnectionManager
import qualified Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import qualified Ouroboros.Network.Diffusion.Common as Common
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), RemoteAddress, WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.Churn (ChurnCounters (..))
import           Ouroboros.Network.PeerSelection.Governor (DebugPeerSelection (..),
                   PeerSelectionCounters, TracePeerSelection (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (TraceLedgerPeers)
import           Ouroboros.Network.PeerSelection.PeerStateActions (PeerSelectionActionsTrace (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
                   (TraceLocalRootPeers (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
                   (TracePublicRootPeers (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                   UnversionedProtocolData (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LTM
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import qualified Ouroboros.Network.Server2 as Server (Trace (..))
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..), WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (SubscriptionTrace (..))
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)

import           Control.Exception (SomeException)
import           Control.Monad (forM_)
import           Data.Aeson.Types (ToJSON)
import           Data.Proxy (Proxy (..))
import qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import qualified Options.Applicative as Opt
import           System.IO


data TraceDocumentationCmd
  = TraceDocumentationCmd
    { tdcConfigFile :: FilePath
    , tdcOutput     :: FilePath
    , tdMetricsHelp :: Maybe FilePath
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
                 <> Opt.metavar "FILE"
                 <> Opt.help "Configuration file for the cardano-node"
               )
           <*> Opt.strOption
               ( Opt.long "output-file"
                 <> Opt.metavar "FILE"
                 <> Opt.help "Generated documentation output file (Markdown)"
               )
           <*> Opt.optional (Opt.strOption
                ( Opt.long "output-metric-help"
                  <> Opt.metavar "FILE"
                  <> Opt.help "Metrics helptext file for cardano-tracer (JSON)"
                )
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
  docTracers tdcConfigFile tdcOutput tdMetricsHelp

-- Have to repeat the construction of the tracers here,
-- as the tracers are behind old tracer interface after construction in mkDispatchTracers.
-- Can be changed, when old tracers have gone
docTracers ::
     FilePath
  -> FilePath
  -> Maybe FilePath
  -> IO ()
docTracers configFileName outputFileName mbMetricsHelpFilename = do
    (bl, trConfig) <- docTracersFirstPhase (Just configFileName)
    docTracersSecondPhase outputFileName mbMetricsHelpFilename trConfig bl


-- Have to repeat the construction of the tracers here,
-- as the tracers are behind old tracer interface after construction in mkDispatchTracers.
-- Can be changed, when old tracers have gone
docTracersFirstPhase :: forall blk peer remotePeer.
  ( TraceConstraints blk
  , Proxy blk ~ Proxy (CardanoBlock StandardCrypto)
  , Proxy peer ~ Proxy (NtN.ConnectionId LocalAddress)
  , Proxy remotePeer ~ Proxy (NtN.ConnectionId NtN.RemoteAddress)
  )
  => Maybe FilePath
  -> IO (DocTracer, TraceConfig)
docTracersFirstPhase condConfigFileName = do
    trConfig      <- case condConfigFileName of
                        Just fn -> readConfigurationWithDefault fn defaultCardanoConfig
                        Nothing -> pure defaultCardanoConfig
    let trBase    :: Logging.Trace IO FormattedMessage = docTracer (Stdout MachineFormat)
        trForward :: Logging.Trace IO FormattedMessage = docTracer Forwarder
        trDataPoint = docTracerDatapoint DatapointBackend
        mbTrEKG   :: Maybe (Logging.Trace IO FormattedMessage) = Just (docTracer EKGBackend)

    configReflection <- emptyConfigReflection

    -- NodeInfo tracer
    nodeInfoDp <- mkDataPointTracer
                    trDataPoint
    configureTracers configReflection trConfig  [nodeInfoDp]
    nodeInfoDpDoc <- documentTracer (nodeInfoDp :: Logging.Trace IO NodeInfo)

    nodeStartupInfoDp <- mkDataPointTracer
                trDataPoint
    configureTracers configReflection trConfig [nodeStartupInfoDp]
    nodeStartupInfoDpDoc <- documentTracer
                      (nodeStartupInfoDp :: Logging.Trace IO NodeStartupInfo)

    nodeVersionTr <- mkCardanoTracer
                      trBase trForward mbTrEKG
                      ["Version"]
    configureTracers configReflection trConfig  [nodeVersionTr]
    nodeVersionDoc <- documentTracer (nodeVersionTr :: Logging.Trace IO NodeVersionTrace)

    -- State tracer
    stateTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["NodeState"]
    configureTracers configReflection trConfig [stateTr]
    stateTrDoc <- documentTracer (stateTr :: Logging.Trace IO SR.NodeState)

    --  Peers tracer
    peersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Peers", "List"]
    configureTracers configReflection trConfig [peersTr]
    peersTrDoc <- documentTracer (peersTr :: Logging.Trace IO  [PeerT blk])

    -- Resource tracer
    resourcesTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                []
    configureTracers configReflection trConfig [resourcesTr]
    resourcesTrDoc <- documentTracer (resourcesTr :: Logging.Trace IO ResourceStats)

    ledgerMetricsTr <- mkCardanoTracer trBase trForward mbTrEKG []
    configureTracers configReflection trConfig [ledgerMetricsTr]
    ledgerMetricsTrDoc <- documentTracer (ledgerMetricsTr :: Trace IO LedgerMetrics)

    -- Startup tracer
    startupTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup"]
    configureTracers configReflection trConfig [startupTr]
    startupTrDoc <- documentTracer (startupTr :: Logging.Trace IO (StartupTrace blk))

    shutdownTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Shutdown"]
    configureTracers configReflection trConfig  [shutdownTr]
    shutdownTrDoc <- documentTracer (shutdownTr :: Logging.Trace IO ShutdownTrace)

    chainDBTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                ["ChainDB"]
                withAddedToCurrentChainEmptyLimited
    configureTracers configReflection trConfig [chainDBTr]
    chainDBTrDoc <- documentTracer (chainDBTr ::
                      Logging.Trace IO (ChainDB.TraceEvent blk))

    replayBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainDB", "ReplayBlock"]
    configureTracers configReflection trConfig [replayBlockTr]
    replayBlockTrDoc <- documentTracer (replayBlockTr :: Logging.Trace IO ReplayBlockStats)

-- Consensus tracers

    chainSyncClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Client"]
    configureTracers configReflection trConfig [chainSyncClientTr]
    chainSyncClientTrDoc <- documentTracer (chainSyncClientTr ::
      (Logging.Trace IO (BlockFetch.TraceLabelPeer
                  (ConnectionId RemoteAddress)
                  (TraceChainSyncClientEvent blk))))

    chainSyncServerHeaderTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerHeader"]
    configureTracers configReflection trConfig [chainSyncServerHeaderTr]
    chainSyncServerHeaderTrDoc <- documentTracer (chainSyncServerHeaderTr ::
      (Logging.Trace IO (TraceChainSyncServerEvent blk)))

    chainSyncServerBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerBlock"]
    configureTracers configReflection trConfig [chainSyncServerBlockTr]
    chainSyncServerBlockTrDoc <- documentTracer (chainSyncServerBlockTr ::
      (Logging.Trace IO (TraceChainSyncServerEvent blk)))

    blockFetchDecisionTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Decision"]
    configureTracers configReflection trConfig [blockFetchDecisionTr]
    blockFetchDecisionTrDoc <- documentTracer (blockFetchDecisionTr ::
       Logging.Trace IO [BlockFetch.TraceLabelPeer
                                      remotePeer
                                      (FetchDecision [Point (Header blk)])])

    blockFetchClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Client"]
    configureTracers configReflection trConfig [blockFetchClientTr]
    blockFetchClientTrDoc <- documentTracer (blockFetchClientTr ::
      Logging.Trace IO (BlockFetch.TraceLabelPeer
                  remotePeer
                  (BlockFetch.TraceFetchClientState (Header blk))))

    blockFetchClientMetricsTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Client"]

    configureTracers configReflection trConfig [blockFetchClientMetricsTr]
    blockFetchClientMetricsDoc <- documentTracer (blockFetchClientMetricsTr ::
        Logging.Trace IO ClientMetrics)

    blockFetchServerTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Server"]
    configureTracers configReflection trConfig [blockFetchServerTr]
    blockFetchServerTrDoc <- documentTracer (blockFetchServerTr ::
      Logging.Trace IO (TraceBlockFetchServerEvent blk))

    forgeKESInfoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge"]
    configureTracers configReflection trConfig [forgeKESInfoTr]
    forgeKESInfoTrDoc <- documentTracer (forgeKESInfoTr ::
      Logging.Trace IO (Consensus.TraceLabelCreds HotKey.KESInfo))

    txInboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxInbound"]
    configureTracers configReflection trConfig [txInboundTr]
    txInboundTrDoc <- documentTracer (txInboundTr ::
      Logging.Trace IO (BlockFetch.TraceLabelPeer
                  remotePeer
                  (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))

    txOutboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxOutbound"]
    configureTracers configReflection trConfig [txOutboundTr]
    txOutboundTrDoc <- documentTracer (txOutboundTr ::
      Logging.Trace IO (BlockFetch.TraceLabelPeer
                  remotePeer
                  (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))

    localTxSubmissionServerTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "LocalServer"]
    configureTracers configReflection trConfig [localTxSubmissionServerTr]
    localTxSubmissionServerTrDoc <- documentTracer (localTxSubmissionServerTr ::
      Logging.Trace IO (TraceLocalTxSubmissionServerEvent blk))

    mempoolTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Mempool"]
    configureTracers configReflection trConfig [mempoolTr]
    mempoolTrDoc <- documentTracer (mempoolTr ::
      Logging.Trace IO (TraceEventMempool blk))

    forgeTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "Loop"]
    configureTracers configReflection trConfig [forgeTr]
    forgeTrDoc <- documentTracer (forgeTr ::
      Logging.Trace IO (Consensus.TraceForgeEvent blk))

    forgeTr' <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "ThreadStats"]
    configureTracers configReflection trConfig [forgeTr']
    forgeStatsTrDoc <- documentTracer (forgeTr' ::
      Logging.Trace IO ForgingStats)

    blockchainTimeTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockchainTime"]
    configureTracers configReflection trConfig [blockchainTimeTr]
    blockchainTimeTrDoc <- documentTracer (blockchainTimeTr ::
      Logging.Trace IO (TraceBlockchainTimeEvent RelativeTime))


    consensusSanityCheckTr <- mkCardanoTracer
                 trBase trForward mbTrEKG
                 ["Consensus", "SanityCheck"]
    configureTracers configReflection trConfig [consensusSanityCheckTr]
    consensusSanityCheckTrDoc <- documentTracer (consensusSanityCheckTr ::
      Logging.Trace IO SanityCheckIssue)

    consensusStartupErrorTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "Startup"]
    configureTracers configReflection trConfig [consensusStartupErrorTr]
    consensusStartupErrorTrDoc <- documentTracer (consensusStartupErrorTr ::
      Logging.Trace IO ConsensusStartupException)

    consensusGddTr <- mkCardanoTracer
                 trBase trForward mbTrEKG
                 ["Consensus", "GDD"]
    configureTracers configReflection trConfig [consensusGddTr]
    consensusGddTrDoc <- documentTracer (consensusGddTr ::
      Logging.Trace IO (TraceGDDEvent peer blk))

    consensusGsmTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "GSM"]
    configureTracers configReflection trConfig [consensusGsmTr]
    consensusGsmTrDoc <- documentTracer (consensusGsmTr ::
      Logging.Trace IO (TraceGsmEvent (Tip blk)))

    consensusCsjTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "CSJ"]
    configureTracers configReflection trConfig [consensusCsjTr]
    consensusCsjTrDoc <- documentTracer (consensusCsjTr ::
      Logging.Trace IO (Jumping.TraceEventCsj peer blk))

    consensusDbfTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Consensus", "DevotedBlockFetch"]
    configureTracers configReflection trConfig [consensusDbfTr]
    consensusDbfTrDoc <- documentTracer (consensusDbfTr ::
      Logging.Trace IO (Jumping.TraceEventDbf peer))


-- Node to client

    keepAliveClientTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net"]
    configureTracers configReflection trConfig [keepAliveClientTr]
    keepAliveClientTrDoc <- documentTracer (keepAliveClientTr ::
      Logging.Trace IO (TraceKeepAliveClient peer))

    chainSyncTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Local"]
    configureTracers configReflection trConfig [chainSyncTr]
    chainSyncTrDoc <- documentTracer (chainSyncTr ::
      Logging.Trace IO
        (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync (Header blk) (Point blk) (Tip blk)))))

    txMonitorTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        ["TxSubmission", "MonitorClient"]
    configureTracers configReflection trConfig [txMonitorTr]
    txMonitorTrDoc <- documentTracer (txMonitorTr ::
      Logging.Trace IO
        (BlockFetch.TraceLabelPeer
           peer
           (TraceSendRecv
              (LTM.LocalTxMonitor
                 (GenTxId blk) (GenTx blk) SlotNo))))

    txSubmissionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Local"]
    configureTracers configReflection trConfig [txSubmissionTr]
    txSubmissionTrDoc <- documentTracer (txSubmissionTr ::
      Logging.Trace IO
         (BlockFetch.TraceLabelPeer
            peer
            (TraceSendRecv
               (LTS.LocalTxSubmission
                  (GenTx blk) (ApplyTxErr blk)))))

    stateQueryTr  <-  mkCardanoTracer
                        trBase trForward mbTrEKG
                       ["StateQueryServer"]
    configureTracers configReflection trConfig [stateQueryTr]
    stateQueryTrDoc <- documentTracer (stateQueryTr ::
      Logging.Trace IO
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (LocalStateQuery blk (Point blk) (Query blk)))))

-- Node to Node

    chainSyncNodeTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Remote"]
    configureTracers configReflection trConfig [chainSyncNodeTr]
    chainSyncNodeTrDoc <- documentTracer (chainSyncNodeTr ::
      Logging.Trace IO (BlockFetch.TraceLabelPeer peer (TraceSendRecv
               (ChainSync (Header blk) (Point blk) (Tip blk)))))

    chainSyncSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Remote", "Serialised"]
    configureTracers configReflection trConfig [chainSyncSerialisedTr]
    chainSyncSerialisedTrDoc <- documentTracer (chainSyncSerialisedTr ::
      Logging.Trace IO (BlockFetch.TraceLabelPeer peer (TraceSendRecv
            (ChainSync (Header blk) (Point blk) (Tip blk)))))

    blockFetchTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote"]
    configureTracers configReflection trConfig [blockFetchTr]
    blockFetchTrDoc <- documentTracer (blockFetchTr ::
      Logging.Trace IO
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch blk (Point blk)))))

    blockFetchSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote", "Serialised"]
    configureTracers configReflection trConfig [blockFetchSerialisedTr]
    blockFetchSerialisedTrDoc <- documentTracer (blockFetchSerialisedTr ::
      Logging.Trace IO
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch (Serialised blk) (Point blk)))))

    txSubmission2Tr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Remote"]
    configureTracers configReflection trConfig [txSubmission2Tr]
    txSubmission2TrDoc <- documentTracer (txSubmission2Tr ::
      Logging.Trace IO
        (BlockFetch.TraceLabelPeer peer
          (TraceSendRecv
            (TxSubmission2 (GenTxId blk) (GenTx blk)))))

-- Diffusion
    dtMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote"]
    configureTracers configReflection trConfig [dtMuxTr]
    dtMuxTrDoc <- documentTracer (dtMuxTr ::
      Logging.Trace IO (Mux.WithBearer (ConnectionId RemoteAddress) Mux.Trace))

    dtLocalMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local"]
    configureTracers configReflection trConfig [dtLocalMuxTr]
    dtLocalMuxTrDoc <- documentTracer (dtLocalMuxTr ::
      Logging.Trace IO (Mux.WithBearer (ConnectionId LocalAddress) Mux.Trace))

    dtHandshakeTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Remote"]
    configureTracers configReflection trConfig [dtHandshakeTr]
    dtHandshakeTrDoc <- documentTracer (dtHandshakeTr ::
      Logging.Trace IO (NtN.HandshakeTr NtN.RemoteAddress NtN.NodeToNodeVersion))

    dtLocalHandshakeTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                 ["Net", "Handshake", "Local"]
    configureTracers configReflection trConfig [dtLocalHandshakeTr]
    dtLocalHandshakeTrDoc <- documentTracer (dtLocalHandshakeTr ::
      Logging.Trace IO
        (NtC.HandshakeTr LocalAddress NtC.NodeToClientVersion))

    dtDiffusionInitializationTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup", "DiffusionInit"]
    configureTracers configReflection trConfig [dtDiffusionInitializationTr]
    dtDiffusionInitializationTrDoc <- documentTracer (dtDiffusionInitializationTr ::
      Logging.Trace IO (Common.DiffusionTracer Socket.SockAddr LocalAddress))

    dtLedgerPeersTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Peers", "Ledger"]
    configureTracers configReflection trConfig [dtLedgerPeersTr]
    dtLedgerPeersTrDoc <- documentTracer (dtLedgerPeersTr ::
      Logging.Trace IO TraceLedgerPeers)

-- DiffusionTracersExtra P2P
    localRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "LocalRoot"]
    configureTracers configReflection trConfig [localRootPeersTr]
    localRootPeersTrDoc <- documentTracer (localRootPeersTr ::
      Logging.Trace IO (TraceLocalRootPeers PeerTrustable RemoteAddress SomeException))

    publicRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "PublicRoot"]
    configureTracers configReflection trConfig [publicRootPeersTr]
    publicRootPeersTrDoc <- documentTracer (publicRootPeersTr ::
      Logging.Trace IO TracePublicRootPeers)

    peerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Selection"]
    configureTracers configReflection trConfig [peerSelectionTr]
    peerSelectionTrDoc <- documentTracer (peerSelectionTr ::
      Logging.Trace IO (TracePeerSelection Cardano.DebugPeerSelectionState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers Socket.SockAddr) Socket.SockAddr))

    debugPeerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Initiator"]
    configureTracers configReflection trConfig [debugPeerSelectionTr]
    debugPeerSelectionTrDoc <- documentTracer (debugPeerSelectionTr ::
      Logging.Trace IO (DebugPeerSelection Cardano.ExtraState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers Socket.SockAddr) Socket.SockAddr))

    debugPeerSelectionResponderTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Responder"]
    configureTracers configReflection trConfig [debugPeerSelectionResponderTr]
    debugPeerSelectionResponderTrDoc <- documentTracer (debugPeerSelectionResponderTr ::
      Logging.Trace IO (DebugPeerSelection Cardano.ExtraState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers Socket.SockAddr) Socket.SockAddr))

    peerSelectionCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Counters"]
    configureTracers configReflection trConfig [peerSelectionCountersTr]
    peerSelectionCountersTrDoc <- documentTracer (peerSelectionCountersTr ::
      Logging.Trace IO (PeerSelectionCounters (Cardano.ExtraPeerSelectionSetsWithSizes Socket.SockAddr)))

    churnCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Churn"]
    configureTracers configReflection trConfig [churnCountersTr]
    churnCountersTrDoc <- documentTracer (churnCountersTr :: Logging.Trace IO ChurnCounters)

    peerSelectionActionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Actions"]
    configureTracers configReflection trConfig [peerSelectionActionsTr]
    peerSelectionActionsTrDoc <- documentTracer (peerSelectionActionsTr ::
      Logging.Trace IO (PeerSelectionActionsTrace Socket.SockAddr LocalAddress))

    connectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Remote"]
    configureTracers configReflection trConfig [connectionManagerTr]
    connectionManagerTrDoc <- documentTracer (connectionManagerTr ::
      Logging.Trace IO
        (ConnectionManager.Trace
          Socket.SockAddr
          (ConnectionHandlerTrace UnversionedProtocol UnversionedProtocolData)))

    connectionManagerTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Transition"]
    configureTracers configReflection trConfig [connectionManagerTransitionsTr]
    connectionManagerTransitionsTrDoc <- documentTracer (connectionManagerTransitionsTr ::
      Logging.Trace IO (ConnectionManager.AbstractTransitionTrace Socket.SockAddr))

    serverTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Remote"]
    configureTracers configReflection trConfig [serverTr]
    serverTrDoc <- documentTracer (serverTr ::
      Logging.Trace IO (Server.Trace Socket.SockAddr))

    inboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "InboundGovernor", "Remote"]
    configureTracers configReflection trConfig [inboundGovernorTr]
    inboundGovernorTrDoc <- documentTracer (inboundGovernorTr ::
      Logging.Trace IO (InboundGovernor.Trace Socket.SockAddr))

    inboundGovernorTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Transition"]
    configureTracers configReflection trConfig [inboundGovernorTransitionsTr]
    inboundGovernorTransitionsTrDoc <- documentTracer (inboundGovernorTransitionsTr ::
       Logging.Trace IO (InboundGovernor.RemoteTransitionTrace Socket.SockAddr))

    localConnectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "ConnectionManager", "Local"]
    configureTracers configReflection trConfig [localConnectionManagerTr]
    localConnectionManagerTrDoc <- documentTracer (localConnectionManagerTr ::
      Logging.Trace IO
        (ConnectionManager.Trace
          Socket.SockAddr
          (ConnectionHandlerTrace
            UnversionedProtocol
            UnversionedProtocolData)))

    localServerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Local"]
    configureTracers configReflection trConfig [localServerTr]
    localServerTrDoc <- documentTracer (localServerTr ::
      Logging.Trace IO (Server.Trace LocalAddress))

    localInboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "InboundGovernor", "Local"]
    configureTracers configReflection trConfig [localInboundGovernorTr]
    localInboundGovernorTrDoc <- documentTracer (localInboundGovernorTr ::
      Logging.Trace IO (InboundGovernor.Trace LocalAddress))


-- -- DiffusionTracersExtra nonP2P

    dtIpSubscriptionTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "IP"]
    configureTracers configReflection trConfig [dtIpSubscriptionTr]
    dtIpSubscriptionTrDoc <- documentTracer (dtIpSubscriptionTr ::
      Logging.Trace IO (WithIPList (SubscriptionTrace Socket.SockAddr)))

    dtDnsSubscriptionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "DNS"]
    configureTracers configReflection trConfig [dtDnsSubscriptionTr]
    dtDnsSubscriptionTrDoc <- documentTracer (dtDnsSubscriptionTr ::
      Logging.Trace IO (WithDomainName (SubscriptionTrace Socket.SockAddr)))

    dtDnsResolverTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "DNSResolver"]
    configureTracers configReflection trConfig [dtDnsResolverTr]
    dtDnsResolverTrDoc <- documentTracer (dtDnsResolverTr ::
      Logging.Trace IO (WithDomainName DnsTrace))

    dtErrorPolicyTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Remote"]
    configureTracers configReflection trConfig [dtErrorPolicyTr]
    dtErrorPolicyTrDoc <- documentTracer (dtErrorPolicyTr ::
      Logging.Trace IO (WithAddr Socket.SockAddr ErrorPolicyTrace))

    dtLocalErrorPolicyTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Local"]
    configureTracers configReflection trConfig [dtLocalErrorPolicyTr]
    dtLocalErrorPolicyTrDoc <- documentTracer (dtLocalErrorPolicyTr ::
      Logging.Trace IO (WithAddr LocalAddress ErrorPolicyTrace))

    dtAcceptPolicyTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "AcceptPolicy"]
    configureTracers configReflection trConfig [dtAcceptPolicyTr]
    dtAcceptPolicyTrDoc <- documentTracer (dtAcceptPolicyTr ::
      Logging.Trace IO NtN.AcceptConnectionsPolicyTrace)

    internalTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Reflection"]
    configureTracers configReflection trConfig [internalTr]
    internalTrDoc <- documentTracer (internalTr ::
      Logging.Trace IO TraceDispatcherMessage)


    let bl =   nodeInfoDpDoc
            <> nodeStartupInfoDpDoc
            <> stateTrDoc
            <> resourcesTrDoc
            <> ledgerMetricsTrDoc
            <> startupTrDoc
            <> shutdownTrDoc
            <> nodeVersionDoc
            <> peersTrDoc
            <> chainDBTrDoc
            <> replayBlockTrDoc
-- Consensus
            <> chainSyncClientTrDoc
            <> chainSyncServerHeaderTrDoc
            <> chainSyncServerBlockTrDoc
            <> blockFetchDecisionTrDoc
            <> blockFetchClientTrDoc
            <> blockFetchClientMetricsDoc
            <> blockFetchServerTrDoc
            <> forgeKESInfoTrDoc
            <> txInboundTrDoc
            <> txOutboundTrDoc
            <> localTxSubmissionServerTrDoc
            <> mempoolTrDoc
            <> forgeTrDoc
            <> forgeStatsTrDoc
            <> blockchainTimeTrDoc
            <> consensusSanityCheckTrDoc
            <> consensusStartupErrorTrDoc
            <> consensusGddTrDoc
            <> consensusGsmTrDoc
            <> consensusCsjTrDoc
            <> consensusDbfTrDoc
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
            <> churnCountersTrDoc
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
-- Internal tracer
            <> internalTrDoc
    pure (bl,trConfig)

docTracersSecondPhase ::
     FilePath
  -> Maybe FilePath
  -> TraceConfig
  -> DocTracer
  -> IO ()
docTracersSecondPhase outputFileName mbMetricsHelpFilename trConfig bl = do
    docuResultsToText bl trConfig
      >>= doWrite outputFileName
    forM_ mbMetricsHelpFilename $ \f ->
       doWrite f (docuResultsToMetricsHelptext bl)
  where
    doWrite outfile text =
      withFile outfile WriteMode $ \handle ->
        hSetEncoding handle utf8 >> T.hPutStr handle text
