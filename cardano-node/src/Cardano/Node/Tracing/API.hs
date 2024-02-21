{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Node.Tracing.API
  ( initTraceDispatcher
  ) where

import           Cardano.Logging hiding (traceWith)
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
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.Node (NetworkP2PMode)
import           Ouroboros.Consensus.Node.GSM
import           Ouroboros.Network.Block
import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.NodeToClient (withIOManager)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Prelude

import           "contra-tracer" Control.Tracer (traceWith)
import           "trace-dispatcher" Control.Tracer (nullTracer)
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
  )
  => NodeConfiguration
  -> SomeConsensusProtocol
  -> NetworkMagic
  -> NodeKernelData blk
  -> NetworkP2PMode p2p
  -> IO (Tracers RemoteConnectionId LocalConnectionId blk p2p)
initTraceDispatcher nc p networkMagic nodeKernel p2pMode = do
  trConfig <- readConfigurationWithDefault
                (unConfigPath $ ncConfigFile nc)
                defaultCardanoConfig

  tracers <- mkTracers trConfig

  traceWith (nodeStateTracer tracers) NodeTracingOnlineConfiguring

  startResourceTracer
    (resourcesTracer tracers)
    (fromMaybe 1000 (tcResourceFrequency trConfig))

  startPeerTracer
    (peersTracer tracers)
    nodeKernel
    (fromMaybe 2000 (tcPeerFrequency trConfig))

  now <- getCurrentTime
  prepareNodeInfo nc p trConfig now >>= traceWith (nodeInfoTracer tracers)

  pure tracers
 where
  mkTracers trConfig = do
    ekgStore <- EKG.newStore
    EKG.registerGcMetrics ekgStore
    ekgTrace <- ekgTracer (Left ekgStore)

    stdoutTrace <- standardTracer

    -- We should initialize forwarding only if 'Forwarder' backend
    -- is presented in the node's configuration.
    (fwdTracer, dpTracer) <-
      if forwarderBackendEnabled
        then do
          -- TODO: check if this is the correct way to use withIOManager
          (forwardSink, dpStore) <- withIOManager $ \iomgr -> do
            let tracerSocketMode = Just . first unFile =<< ncTraceForwardSocket nc
                forwardingConf = fromMaybe defaultForwarder (tcForwarder trConfig)
            initForwarding iomgr forwardingConf networkMagic (Just ekgStore) tracerSocketMode
          pure (forwardTracer forwardSink, dataPointTracer dpStore)
        else
          -- Since 'Forwarder' backend isn't enabled, there is no forwarding.
          -- So we use nullTracers to ignore 'TraceObject's and 'DataPoint's.
          pure (Trace nullTracer, Trace nullTracer)

    mkDispatchTracers
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
      any checkForwarder . concat . Map.elems $ tcOptions trConfig

    checkForwarder (ConfBackend backends') = Forwarder `elem` backends'
    checkForwarder _ = False
