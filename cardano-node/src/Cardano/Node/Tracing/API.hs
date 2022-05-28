{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Node.Tracing.API
  ( initTraceDispatcher
  ) where

import           Prelude

import           "contra-tracer" Control.Tracer (traceWith)
import           "trace-dispatcher" Control.Tracer (nullTracer)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (getCurrentTime)


import           System.Metrics as EKG

import           Network.Mux.Trace (TraceLabelPeer (..))

import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.Node (NetworkP2PMode, RunNode)
import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.NodeToClient (withIOManager)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Cardano.Node.Configuration.NodeAddress (SocketPath (..))
import           Cardano.Node.Configuration.POM (NodeConfiguration (..), ncProtocol)
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Queries
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Types

import           Cardano.Logging hiding (traceWith)
import           Cardano.Node.Tracing.StateRep (NodeState (..))
import           Cardano.Node.Tracing.Tracers
import           Cardano.Node.Tracing.Tracers.Peer (startPeerTracer)
import           Cardano.Node.Tracing.Tracers.Resources (startResourceTracer)

initTraceDispatcher ::
  forall blk p2p.
  ( RunNode blk
  , TraceConstraints blk
  , LogFormatting (LedgerEvent blk)
  , LogFormatting
    (TraceLabelPeer (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  )
  => NodeConfiguration
  -> SomeConsensusProtocol
  -> NetworkMagic
  -> NodeKernelData blk
  -> NetworkP2PMode p2p
  -> IO (Tracers RemoteConnectionId LocalConnectionId blk p2p)
initTraceDispatcher nc p networkMagic nodeKernel p2pMode = do
  trConfig <- readConfiguration (unConfigPath $ ncConfigFile nc)
  putStrLn $ "New tracer configuration: " <> show trConfig

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
  prepareNodeInfo (ncProtocol nc) p trConfig now
    >>= traceWith (nodeInfoTracer tracers)

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
            let forwardSocket = (Just . unSocketPath) =<< ncForwardSocket nc
            initForwarding iomgr trConfig networkMagic ekgStore forwardSocket
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
