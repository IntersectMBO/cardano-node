{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Node.Tracing.API
  ( initTraceDispatcher
  ) where

import           Cardano.Logging hiding (traceWith)
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import           Cardano.Node.Configuration.NodeAddress (File (..))
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Queries
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.StateRep (NodeState (..))
import           Cardano.Node.Tracing.Tracers
import           Cardano.Node.Tracing.Tracers.Peer (startPeerTracer)
import           Cardano.Node.Tracing.Tracers.Resources (startResourceTracer)
import           Cardano.Node.Types
import qualified Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState as Cardano
import qualified Ouroboros.Cardano.Network.PeerSelection.Governor.Types as Cardano
import qualified Ouroboros.Cardano.Network.PublicRootPeers as Cardano.PublicRootPeers
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.Node (NetworkP2PMode)
import           Ouroboros.Consensus.Node.GSM
import           Ouroboros.Network.Block
import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.NodeToClient (LocalAddress, withIOManager)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Prelude

import           Control.DeepSeq (deepseq)
import           "contra-tracer" Control.Tracer (traceWith)
import           "trace-dispatcher" Control.Tracer (nullTracer)
import           Data.Aeson (ToJSONKey)
import           Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (getCurrentTime)
import           Network.Mux.Trace (TraceLabelPeer (..))
import           System.Metrics as EKG

initTraceDispatcher ::
  forall blk p2p.
  ( TraceConstraints blk
  , LogFormatting (LedgerEvent blk)
  , LogFormatting
    (TraceLabelPeer (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  , LogFormatting (TraceGsmEvent (Tip blk))
  , ToJSONKey (GenTxId blk)
  )
  => NodeConfiguration
  -> SomeConsensusProtocol
  -> NetworkMagic
  -> NodeKernelData blk
  -> NetworkP2PMode p2p
  -> IO (Tracers RemoteAddress LocalAddress blk p2p Cardano.ExtraState Cardano.DebugPeerSelectionState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers RemoteAddress) (Cardano.ExtraPeerSelectionSetsWithSizes RemoteAddress) IO)
initTraceDispatcher nc p networkMagic nodeKernel p2pMode = do
  trConfig <- readConfigurationWithDefault
                (unConfigPath $ ncConfigFile nc)
                defaultCardanoConfig

  (kickoffForwarder, tracers) <- mkTracers trConfig

  -- The NodeInfo DataPoint needs to be fully evaluated and stored
  -- before it is queried for the first time by cardano-tracer.
  -- Hence, we delay initiating the forwarding connection.
  nodeInfo <- prepareNodeInfo nc p trConfig =<< getCurrentTime
  nodeInfo `deepseq` traceWith (nodeInfoTracer tracers) nodeInfo

  kickoffForwarder

  traceWith (nodeStateTracer tracers) NodeTracingOnlineConfiguring

  startResourceTracer
    (resourcesTracer tracers)
    (fromMaybe 1000 (tcResourceFrequency trConfig))

  startPeerTracer
    (peersTracer tracers)
    nodeKernel
    (fromMaybe 2000 (tcPeerFrequency trConfig))


  pure tracers
 where
  mkTracers trConfig = do
    ekgStore <- EKG.newStore
    EKG.registerGcMetrics ekgStore
    ekgTrace <- ekgTracer trConfig (Left ekgStore)

    stdoutTrace <- standardTracer

    -- We should initialize forwarding only if 'Forwarder' backend
    -- is presented in the node's configuration.
    (fwdTracer, dpTracer, kickoffForwarder) <-
      if forwarderBackendEnabled
        then do
          -- TODO: check if this is the correct way to use withIOManager
          (forwardSink, dpStore, kickoffForwarder) <- withIOManager $ \iomgr -> do
            let tracerSocketMode = Just . first unFile =<< ncTraceForwardSocket nc
                forwardingConf = fromMaybe defaultForwarder (tcForwarder trConfig)
            initForwardingDelayed iomgr forwardingConf networkMagic (Just ekgStore) tracerSocketMode
          pure (forwardTracer forwardSink, dataPointTracer dpStore, kickoffForwarder)
        else
          -- Since 'Forwarder' backend isn't enabled, there is no forwarding.
          -- So we use nullTracers to ignore 'TraceObject's and 'DataPoint's.
          pure (Trace nullTracer, Trace nullTracer, pure ())

    (,) kickoffForwarder <$> mkDispatchTracers
      nodeKernel
      stdoutTrace
      fwdTracer
      (Just ekgTrace)
      dpTracer
      trConfig
      p2pMode
      p

   where
    forwarderBackendEnabled =
      (any (any checkForwarder) . Map.elems) $ tcOptions trConfig

    checkForwarder (ConfBackend backends') = Forwarder `elem` backends'
    checkForwarder _ = False
