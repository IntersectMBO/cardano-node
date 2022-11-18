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

import           Data.Aeson.Types (ToJSON)
import qualified Data.Text.IO as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import qualified Options.Applicative as Opt

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (trace)

import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.Formatting ()
import qualified Cardano.Node.Tracing.StateRep as SR
import           Cardano.Node.Tracing.Tracers.BlockReplayProgress
import           Cardano.Node.Tracing.Tracers.ChainDB
import           Cardano.Node.Tracing.Tracers.Consensus
import           Cardano.Node.Tracing.Tracers.Diffusion
import           Cardano.Node.Tracing.Tracers.ForgingThreadStats (
                   docForgeStats, forgeThreadStats)
import           Cardano.Node.Tracing.Tracers.KESInfo
import           Cardano.Node.Tracing.Tracers.NodeToClient
import           Cardano.Node.Tracing.Tracers.NodeToNode
import           Cardano.Node.Tracing.Tracers.NonP2P
import           Cardano.Node.Tracing.Tracers.P2P
import           Cardano.Node.Tracing.Tracers.Peer
import           Cardano.Node.Tracing.Tracers.Shutdown
import           Cardano.Node.Tracing.Tracers.Startup

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
                (const ["NodeInfo"])
    configureTracers trConfig docNodeInfoTraceEvent [nodeInfoTr]
    nodeInfoTrDoc <- documentTracer trConfig nodeInfoTr
      (docNodeInfoTraceEvent :: Documented NodeInfo)

    nodeStartupInfoTr <- mkDataPointTracer
                trDataPoint
                (const ["NodeStartupInfo"])
    configureTracers trConfig docNodeStartupInfoTraceEvent [nodeStartupInfoTr]
    nodeStartupInfoTrDoc <- documentTracer trConfig nodeStartupInfoTr
      (docNodeStartupInfoTraceEvent :: Documented NodeStartupInfo)

    -- State tracer
    stateTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["NodeState"]
                SR.namesNodeState
                SR.severityNodeState
                allPublic
    configureTracers trConfig SR.docNodeState [stateTr]
    stateTrDoc <- documentTracer trConfig stateTr
      (SR.docNodeState :: Documented SR.NodeState)

--  Peers tracer

    peersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Peers", "List"]
                namesForPeers
                severityPeers
                allPublic
    configureTracers trConfig docPeers [peersTr]
    peersTrDoc <- documentTracer trConfig peersTr
      (docPeers :: Documented [PeerT blk])

    -- Resource tracer
    resourcesTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Resources"]
                (const [])
                (const Info)
                allPublic
    configureTracers trConfig docResourceStats [resourcesTr]
    resourcesTrDoc <- documentTracer trConfig resourcesTr
      (docResourceStats :: Documented ResourceStats)

    -- Startup tracer
    startupTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup"]
                namesStartupInfo
                (const Notice)
                allPublic
    configureTracers trConfig docStartupInfo [startupTr]
    startupTrDoc <- documentTracer trConfig startupTr
      (docStartupInfo :: Documented (StartupTrace blk))

    shutdownTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Shutdown"]
                namesForShutdown
                severityShutdown
                allPublic
    configureTracers trConfig docShutdown [shutdownTr]
    shutdownTrDoc <- documentTracer trConfig shutdownTr
      (docShutdown :: Documented ShutdownTrace)




    chainDBTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                ["ChainDB"]
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                withAddedToCurrentChainEmptyLimited
    configureTracers trConfig docChainDBTraceEvent [chainDBTr]
    chainDBTrDoc <- documentTracer trConfig chainDBTr
      (docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk))

    replayBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainDB", "ReplayBlock"]
                namesForReplayBlockStats
                severityReplayBlockStats
                allPublic
    configureTracers trConfig docReplayedBlock [replayBlockTr]
    replayBlockTrDoc <- documentTracer trConfig replayBlockTr
      (docReplayedBlock :: Documented ReplayBlockStats)

-- Consensus tracers

    chainSyncClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Client"]
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    configureTracers trConfig docChainSyncClientEvent [chainSyncClientTr]
    chainSyncClientTrDoc <- documentTracer trConfig chainSyncClientTr
      (docChainSyncClientEvent :: Documented (BlockFetch.TraceLabelPeer
                                    (ConnectionId RemoteAddress)
                                    (TraceChainSyncClientEvent blk)))

    chainSyncServerHeaderTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerHeader"]
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEventHeader [chainSyncServerHeaderTr]
    chainSyncServerHeaderTrDoc <- documentTracer trConfig chainSyncServerHeaderTr
      (docChainSyncServerEventHeader :: Documented (TraceChainSyncServerEvent blk))

    chainSyncServerBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "ServerBlock"]
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEventBlock [chainSyncServerBlockTr]
    chainSyncServerBlockTrDoc <- documentTracer trConfig chainSyncServerBlockTr
      (docChainSyncServerEventBlock :: Documented (TraceChainSyncServerEvent blk))

    blockFetchDecisionTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Decision"]
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    configureTracers trConfig docBlockFetchDecision [blockFetchDecisionTr]
    blockFetchDecisionTrDoc <- documentTracer trConfig blockFetchDecisionTr
      (docBlockFetchDecision :: Documented [BlockFetch.TraceLabelPeer
                                      remotePeer
                                      (FetchDecision [Point (Header blk)])])

    blockFetchClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Client"]
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    configureTracers trConfig docBlockFetchClient [blockFetchClientTr]
    blockFetchClientTrDoc <- documentTracer trConfig blockFetchClientTr
      (docBlockFetchClient :: Documented (BlockFetch.TraceLabelPeer
                                remotePeer
                                (BlockFetch.TraceFetchClientState (Header blk))))

    clientMetricsDoc <- documentTracer trConfig blockFetchClientTr
      (docBlockFetchClientMetrics :: Documented (BlockFetch.TraceLabelPeer
                                remotePeer
                                (BlockFetch.TraceFetchClientState (Header blk))))


    blockFetchServerTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Server"]
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    configureTracers trConfig docBlockFetchServer [blockFetchServerTr]
    blockFetchServerTrDoc <- documentTracer trConfig blockFetchServerTr
      (docBlockFetchServer :: Documented (TraceBlockFetchServerEvent blk))

    forgeKESInfoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "KESInfo"]
                namesForKESInfo
                severityKESInfo
                allPublic
    configureTracers trConfig docForgeKESInfo [forgeKESInfoTr]
    forgeKESInfoTrDoc <- documentTracer trConfig forgeKESInfoTr
      (docForgeKESInfo :: Documented (Consensus.TraceLabelCreds HotKey.KESInfo))

    txInboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxInbound"]
                namesForTxInbound
                severityTxInbound
                allPublic
    configureTracers trConfig docTxInbound [txInboundTr]
    txInboundTrDoc <- documentTracer trConfig txInboundTr
      (docTxInbound :: Documented (BlockFetch.TraceLabelPeer
                                    remotePeer
                                    (TraceTxSubmissionInbound txid tx)))

    txOutboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "TxOutbound"]
                namesForTxOutbound
                severityTxOutbound
                allPublic
    configureTracers trConfig docTxOutbound [txOutboundTr]
    txOutboundTrDoc <- documentTracer trConfig txOutboundTr
      (docTxOutbound :: Documented (BlockFetch.TraceLabelPeer
                            remotePeer
                            (TraceTxSubmissionOutbound (TxId (GenTx blk)) tx)))

    localTxSubmissionServerTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "LocalServer"]
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    configureTracers trConfig docLocalTxSubmissionServer [localTxSubmissionServerTr]
    localTxSubmissionServerTrDoc <- documentTracer trConfig localTxSubmissionServerTr
      (docLocalTxSubmissionServer :: Documented (TraceLocalTxSubmissionServerEvent blk))


    mempoolTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Mempool"]
                namesForMempool
                severityMempool
                allPublic
    configureTracers trConfig docMempool [mempoolTr]
    mempoolTrDoc <- documentTracer trConfig mempoolTr
      (docMempool :: Documented (TraceEventMempool blk))

    forgeTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Forge", "Loop"]
                namesForForge
                severityForge
                allPublic

    -- TODO Tracers docforgeThreadStatsTr?
    forgeThreadStatsTr <-
                mkCardanoTracer'
                trBase trForward mbTrEKG
                ["Forge", "Stats"]
                namesForForge2
                severityForge2
                allPublic
                forgeThreadStats

    configureTracers trConfig docForge [forgeTr, forgeThreadStatsTr]
    forgeTrDoc <- documentTracer trConfig forgeTr
      (docForge :: Documented (Either (Consensus.TraceForgeEvent blk)
                               TraceStartLeadershipCheckPlus))

    forgeThreadStatsTrDoc <- documentTracer trConfig forgeThreadStatsTr
      (docForgeStats :: Documented (Either (Consensus.TraceForgeEvent blk)
                               TraceStartLeadershipCheckPlus))

    blockchainTimeTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockchainTime"]
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    configureTracers trConfig docBlockchainTime [blockchainTimeTr]
    blockchainTimeTrDoc <- documentTracer trConfig blockchainTimeTr
      (docBlockchainTime :: Documented (TraceBlockchainTimeEvent RelativeTime))

-- Node to client

    keepAliveClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "KeepAliveClient"]
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
    configureTracers trConfig docKeepAliveClient [keepAliveClientTr]
    keepAliveClientTrDoc <- documentTracer trConfig keepAliveClientTr
      (docKeepAliveClient :: Documented (TraceKeepAliveClient peer))

    chainSyncTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Local"]
                namesForTChainSync
                severityTChainSync
                allPublic
    configureTracers trConfig docTChainSyncNodeToClient [chainSyncTr]
    chainSyncTrDoc <- documentTracer trConfig chainSyncTr
      (docTChainSyncNodeToClient :: Documented
        (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync x (Point blk) (Tip blk)))))

    txMonitorTr <-
      mkCardanoTracer
        trBase trForward mbTrEKG
        ["TxSubmission", "MonitorClient"]
        namesForTTxMonitor
        severityTTxMonitor
        allPublic
    configureTracers trConfig docTTxMonitor [txMonitorTr]
    txMonitorTrDoc <- documentTracer trConfig txMonitorTr
      (docTTxMonitor :: Documented
        (BlockFetch.TraceLabelPeer
           peer
           (TraceSendRecv
              (LTM.LocalTxMonitor
                 (GenTxId blk) (GenTx blk) SlotNo))))

    txSubmissionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Local"]
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
    configureTracers trConfig docTTxSubmission [txSubmissionTr]
    txSubmissionTrDoc <- documentTracer trConfig txSubmissionTr
      (docTTxSubmission :: Documented
         (BlockFetch.TraceLabelPeer
            peer
            (TraceSendRecv
               (LTS.LocalTxSubmission
                  (GenTx blk) (ApplyTxErr blk)))))


    stateQueryTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
               ["StateQueryServer"]
                namesForTStateQuery
                severityTStateQuery
                allPublic
    configureTracers trConfig docTStateQuery [stateQueryTr]
    stateQueryTrDoc <- documentTracer trConfig stateQueryTr
      (docTStateQuery :: Documented
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (LocalStateQuery blk (Point blk) (Query blk)))))

-- Node to Node

    chainSyncNodeTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["ChainSync", "Remote"]
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    configureTracers trConfig docTChainSyncNodeToNode [chainSyncNodeTr]
    chainSyncNodeTrDoc <- documentTracer trConfig chainSyncNodeTr
      (docTChainSyncNodeToNode :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync x (Point blk) (Tip blk)))))

    chainSyncSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                 ["ChainSync", "Remote", "Serialised"]
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    configureTracers trConfig docTChainSyncNodeToNodeSerisalised [chainSyncSerialisedTr]
    chainSyncSerialisedTrDoc <- documentTracer trConfig chainSyncSerialisedTr
      (docTChainSyncNodeToNodeSerisalised :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
          (ChainSync x (Point blk) (Tip blk)))))

    blockFetchTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote"]
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchTr]
    blockFetchTrDoc <- documentTracer trConfig blockFetchTr
      (docTBlockFetch :: Documented
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch x (Point blk)))))

    blockFetchSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["BlockFetch", "Remote", "Serialised"]
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchSerialisedTr]
    blockFetchSerialisedTrDoc <- documentTracer trConfig blockFetchSerialisedTr
      (docTBlockFetch :: Documented
            (BlockFetch.TraceLabelPeer peer
             (TraceSendRecv
               (BlockFetch x (Point blk)))))

    txSubmission2Tr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["TxSubmission", "Remote"]
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    configureTracers trConfig docTTxSubmission2Node [txSubmission2Tr]
    txSubmission2TrDoc <- documentTracer trConfig txSubmission2Tr
      (docTTxSubmission2Node :: Documented
        (BlockFetch.TraceLabelPeer peer
          (TraceSendRecv
            (TxSubmission2 (GenTxId blk) (GenTx blk)))))

-- Diffusion
    dtMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Remote"]
                namesForMux
                severityMux
                allPublic
    configureTracers trConfig docMuxRemote [dtMuxTr]
    dtMuxTrDoc <- documentTracer trConfig dtMuxTr
      (docMuxRemote :: Documented (WithMuxBearer (ConnectionId RemoteAddress) MuxTrace))

    dtLocalMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Mux", "Local"]
                namesForMux
                severityMux
                allPublic
    configureTracers trConfig docMuxLocal [dtLocalMuxTr]
    dtLocalMuxTrDoc <- documentTracer trConfig dtLocalMuxTr
      (docMuxLocal :: Documented (WithMuxBearer (ConnectionId LocalAddress) MuxTrace))

    dtHandshakeTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Handshake", "Remote"]
                namesForHandshake
                severityHandshake
                allPublic
    configureTracers trConfig docHandshake [dtHandshakeTr]
    dtHandshakeTrDoc <- documentTracer trConfig dtHandshakeTr
      (docHandshake ::
        Documented (NtN.HandshakeTr NtN.RemoteAddress NtN.NodeToNodeVersion))

    dtLocalHandshakeTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                 ["Net", "Handshake", "Local"]
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
    configureTracers trConfig docLocalHandshake [dtLocalHandshakeTr]
    dtLocalHandshakeTrDoc <- documentTracer trConfig dtLocalHandshakeTr
      (docLocalHandshake ::
        Documented (NtC.HandshakeTr LocalAddress NtC.NodeToClientVersion))

    dtDiffusionInitializationTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Startup", "DiffusionInit"]
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
    configureTracers trConfig docDiffusionInit [dtDiffusionInitializationTr]
    dtDiffusionInitializationTrDoc <- documentTracer trConfig dtDiffusionInitializationTr
      (docDiffusionInit ::
        Documented (Diffusion.DiffusionTracer Socket.SockAddr LocalAddress))

    dtLedgerPeersTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Peers", "Ledger"]
                namesForLedgerPeers
                severityLedgerPeers
                allPublic
    configureTracers trConfig docLedgerPeers [dtLedgerPeersTr]
    dtLedgerPeersTrDoc <- documentTracer trConfig dtLedgerPeersTr
      (docLedgerPeers :: Documented TraceLedgerPeers)

-- DiffusionTracersExtra P2P

    localRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "LocalRoot"]
      namesForLocalRootPeers
      severityLocalRootPeers
      allPublic
    configureTracers trConfig docLocalRootPeers [localRootPeersTr]
    localRootPeersTrDoc <- documentTracer trConfig localRootPeersTr
      (docLocalRootPeers :: Documented (TraceLocalRootPeers RemoteAddress SomeException))

    publicRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Peers", "PublicRoot"]
      namesForPublicRootPeers
      severityPublicRootPeers
      allPublic
    configureTracers trConfig docPublicRootPeers [publicRootPeersTr]
    publicRootPeersTrDoc <- documentTracer trConfig publicRootPeersTr
      (docPublicRootPeers :: Documented TracePublicRootPeers)

    peerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Selection"]
      namesForPeerSelection
      severityPeerSelection
      allPublic
    configureTracers trConfig docPeerSelection [peerSelectionTr]
    peerSelectionTrDoc <- documentTracer trConfig peerSelectionTr
      (docPeerSelection :: Documented (TracePeerSelection Socket.SockAddr))

    debugPeerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Initiator"]
      namesForDebugPeerSelection
      severityDebugPeerSelection
      allPublic
    configureTracers trConfig docDebugPeerSelection [debugPeerSelectionTr]
    debugPeerSelectionTrDoc <- documentTracer trConfig debugPeerSelectionTr
      (docDebugPeerSelection :: Documented (DebugPeerSelection Socket.SockAddr))

    debugPeerSelectionResponderTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Responder"]
      namesForDebugPeerSelection
      severityDebugPeerSelection
      allPublic
    configureTracers trConfig docDebugPeerSelection [debugPeerSelectionResponderTr]
    debugPeerSelectionResponderTrDoc <- documentTracer trConfig debugPeerSelectionResponderTr
      (docDebugPeerSelection :: Documented (DebugPeerSelection Socket.SockAddr))

    peerSelectionCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Counters"]
      namesForPeerSelectionCounters
      severityPeerSelectionCounters
      allPublic
    configureTracers trConfig docPeerSelectionCounters [peerSelectionCountersTr]
    peerSelectionCountersTrDoc <- documentTracer trConfig peerSelectionCountersTr
      (docPeerSelectionCounters :: Documented PeerSelectionCounters)

    peerSelectionActionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "PeerSelection", "Actions"]
      namesForPeerSelectionActions
      severityPeerSelectionActions
      allPublic
    configureTracers trConfig docPeerSelectionActions [peerSelectionActionsTr]
    peerSelectionActionsTrDoc <- documentTracer trConfig peerSelectionActionsTr
      (docPeerSelectionActions ::
        Documented (PeerSelectionActionsTrace Socket.SockAddr LocalAddress))

    connectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Remote"]
      namesForConnectionManager
      severityConnectionManager
      allPublic
    configureTracers trConfig docConnectionManager [connectionManagerTr]
    connectionManagerTrDoc <- documentTracer trConfig connectionManagerTr
      (docConnectionManager :: Documented
        (ConnectionManagerTrace
          Socket.SockAddr
          (ConnectionHandlerTrace UnversionedProtocol UnversionedProtocolData)))

    connectionManagerTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "ConnectionManager", "Remote", "Transition"]
      (namesForConnectionManagerTransition @RemoteAddress)
      severityConnectionManagerTransition
      allPublic
    configureTracers trConfig docConnectionManagerTransition [connectionManagerTransitionsTr]
    connectionManagerTransitionsTrDoc <- documentTracer trConfig connectionManagerTransitionsTr
      (docConnectionManagerTransition ::
        Documented (ConnectionManager.AbstractTransitionTrace Socket.SockAddr))


    serverTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Remote"]
      namesForServer
      severityServer
      allPublic
    configureTracers trConfig docServer [serverTr]
    serverTrDoc <- documentTracer trConfig serverTr
      (docServer :: Documented (ServerTrace Socket.SockAddr))

    inboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "InboundGovernor", "Remote"]
      namesForInboundGovernor
      severityInboundGovernor
      allPublic
    configureTracers trConfig docInboundGovernorRemote [inboundGovernorTr]
    inboundGovernorTrDoc <- documentTracer trConfig inboundGovernorTr
      (docInboundGovernorRemote :: Documented (InboundGovernorTrace Socket.SockAddr))

    inboundGovernorTransitionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "InboundGovernor", "Remote", "Transition"]
      namesForInboundGovernorTransition
      severityInboundGovernorTransition
      allPublic
    configureTracers trConfig docInboundGovernorTransition [inboundGovernorTransitionsTr]
    inboundGovernorTransitionsTrDoc <- documentTracer trConfig inboundGovernorTransitionsTr
       (docInboundGovernorTransition ::
        Documented (InboundGovernor.RemoteTransitionTrace Socket.SockAddr))


    localConnectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "ConnectionManager", "Local"]
      namesForConnectionManager
      severityConnectionManager
      allPublic
    configureTracers trConfig docConnectionManager [localConnectionManagerTr]
    localConnectionManagerTrDoc <- documentTracer trConfig localConnectionManagerTr
      (docConnectionManager :: Documented
        (ConnectionManagerTrace
          Socket.SockAddr
          (ConnectionHandlerTrace
            UnversionedProtocol
            UnversionedProtocolData)))

    localServerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      ["Net", "Server", "Local"]
      namesForServer
      severityServer
      allPublic
    configureTracers trConfig docServer [localServerTr]
    localServerTrDoc <- documentTracer trConfig localServerTr
      (docServer :: Documented (ServerTrace LocalAddress))


    localInboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
       ["Net", "InboundGovernor", "Local"]
      namesForInboundGovernor
      severityInboundGovernor
      allPublic
    configureTracers trConfig docInboundGovernorLocal [localInboundGovernorTr]
    localInboundGovernorTrDoc <- documentTracer trConfig localInboundGovernorTr
      (docInboundGovernorLocal :: Documented (InboundGovernorTrace LocalAddress))

-- DiffusionTracersExtra nonP2P

    dtIpSubscriptionTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "IP"]
                namesForIPSubscription
                severityIPSubscription
                allPublic
    configureTracers trConfig docIPSubscription [dtIpSubscriptionTr]
    dtIpSubscriptionTrDoc <- documentTracer trConfig dtIpSubscriptionTr
      (docIPSubscription ::
        Documented (WithIPList (SubscriptionTrace Socket.SockAddr)))

    dtDnsSubscriptionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "Subscription", "DNS"]
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
    configureTracers trConfig docDNSSubscription [dtDnsSubscriptionTr]
    dtDnsSubscriptionTrDoc <- documentTracer trConfig dtDnsSubscriptionTr
      (docDNSSubscription ::
        Documented (WithDomainName (SubscriptionTrace Socket.SockAddr)))

    dtDnsResolverTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "DNSResolver"]
                namesForDNSResolver
                severityDNSResolver
                allPublic
    configureTracers trConfig docDNSResolver [dtDnsResolverTr]
    dtDnsResolverTrDoc <- documentTracer trConfig dtDnsResolverTr
      (docDNSResolver :: Documented (WithDomainName DnsTrace))

    dtErrorPolicyTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Remote"]
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
    configureTracers trConfig docErrorPolicy [dtErrorPolicyTr]
    dtErrorPolicyTrDoc <- documentTracer trConfig dtErrorPolicyTr
      (docErrorPolicy :: Documented (WithAddr Socket.SockAddr ErrorPolicyTrace))

    dtLocalErrorPolicyTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "ErrorPolicy", "Local"]
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
    configureTracers trConfig docLocalErrorPolicy [dtLocalErrorPolicyTr]
    dtLocalErrorPolicyTrDoc <- documentTracer trConfig dtLocalErrorPolicyTr
      (docLocalErrorPolicy :: Documented (WithAddr LocalAddress ErrorPolicyTrace))

    dtAcceptPolicyTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                ["Net", "AcceptPolicy"]
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
    configureTracers trConfig docAcceptPolicy [dtAcceptPolicyTr]
    dtAcceptPolicyTrDoc <- documentTracer trConfig dtAcceptPolicyTr
      (docAcceptPolicy :: Documented NtN.AcceptConnectionsPolicyTrace)

    let bl =  nodeInfoTrDoc
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
            <> clientMetricsDoc
            <> blockFetchServerTrDoc
            <> forgeKESInfoTrDoc
            <> txInboundTrDoc
            <> txOutboundTrDoc
            <> localTxSubmissionServerTrDoc
            <> mempoolTrDoc
            <> forgeTrDoc
            <> forgeThreadStatsTrDoc
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

    res <- buildersToText bl trConfig
    T.writeFile outputFileName res
    pure ()
