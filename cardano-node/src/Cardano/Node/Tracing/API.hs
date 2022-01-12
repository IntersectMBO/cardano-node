{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Node.Tracing.API
  ( initTraceDispatcher
  ) where

import           Prelude

import           "contra-tracer" Control.Tracer (traceWith)
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

import           Cardano.Node.Configuration.POM (NodeConfiguration (..), ncProtocol)
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Queries
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Types

import           Cardano.Logging hiding (traceWith)
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
--  trace ("TraceConfig " <> show trConfig) $ pure ()

  ekgStore <- EKG.newStore
  ekgTrace <- ekgTracer (Left ekgStore)

  -- TODO: check if this is the correct way to use withIOManager
  (forwardSink, dpStore) <-
    withIOManager $ \iomgr ->
      initForwarding iomgr trConfig networkMagic ekgStore

  stdoutTrace <- standardTracer

  tracers <-
    mkDispatchTracers
      nodeKernel
      stdoutTrace
      (forwardTracer forwardSink)
      (Just ekgTrace)
      (dataPointTracer dpStore)
      trConfig
      p2pMode

  startResourceTracer
    (resourcesTracer tracers)
    (fromMaybe 1000 (tcResourceFreqency trConfig))

  startPeerTracer
    (peersTracer tracers)
    nodeKernel
    (fromMaybe 2000 (tcPeerFreqency trConfig))

  now <- getCurrentTime
  prepareNodeInfo (ncProtocol nc) p trConfig now
    >>= traceWith (nodeInfoTracer tracers)

  pure tracers
