{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Tracing.API
  ( initTraceDispatcher
  ) where

import           Cardano.Logging hiding (traceWith)
import           Cardano.Logging.Prometheus.TCPServer
import           Cardano.Node.Configuration.NodeAddress (PortNumber)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Queries
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints
import           Cardano.Node.Tracing
import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.StateRep (NodeState (..))
import           Cardano.Node.Tracing.Tracers
import           Cardano.Node.Tracing.Tracers.LedgerMetrics
import           Cardano.Node.Tracing.Tracers.Resources (startResourceTracer)
import           Cardano.Node.Types
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.Node.GSM
import           Ouroboros.Network.Block
import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.NodeToClient (LocalAddress, withIOManager)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Prelude

import           Control.Concurrent.Async (link)
import           Control.DeepSeq (deepseq)
import           Control.Exception (SomeException (..))
import           "contra-tracer" Control.Tracer (traceWith)
import           "trace-dispatcher" Control.Tracer (nullTracer)
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Time.Clock (getCurrentTime)
import           Network.Mux.Trace (TraceLabelPeer (..))
import           Network.Socket (HostName)
import           System.Metrics as EKG

import           Trace.Forward.Forwarding (InitForwardingConfig (..), initForwardingDelayed)
import           Trace.Forward.Utils.TraceObject (writeToSink)


initTraceDispatcher ::
  forall blk.
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
  -> Bool
  -> IO (Tracers RemoteAddress LocalAddress blk  IO)
initTraceDispatcher nc p networkMagic nodeKernel noBlockForging = do
  trConfig <- readConfigurationWithDefault
                defaultCardanoConfig
                (unConfigPath $ ncConfigFile nc)

  (kickoffForwarder, kickoffPrometheusSimple, tracers) <- mkTracers trConfig

  -- The NodeInfo DataPoint needs to be fully evaluated and stored
  -- before it is queried for the first time by cardano-tracer.
  -- Hence, we delay initiating the forwarding connection.
  nodeInfo <- prepareNodeInfo nc p trConfig =<< getCurrentTime
  nodeInfo `deepseq` traceWith (nodeInfoTracer tracers) nodeInfo

  kickoffForwarder

  traceWith (nodeStateTracer tracers) NodeTracingOnlineConfiguring

  kickoffPrometheusSimple

  startResourceTracer
    (resourcesTracer tracers)
    (fromMaybe 1000 (lookupPeriodicDelay "Resources" trConfig))

  startLedgerMetricsTracer
    (ledgerMetricsTracer tracers)
    (fromMaybe ledgerMetricsDefaultFreq (lookupPeriodicDelay "LedgerMetrics" trConfig))
    nodeKernel

  pure tracers
 where
  -- this is the backwards compatible default: block producers emit these metrics every second, relays never.
  ledgerMetricsDefaultFreq = if noBlockForging then 0 else 1

  mkTracers
    :: TraceConfig
    -> IO ( IO ()
          , IO ()
          , Tracers RemoteAddress LocalAddress blk IO
          )
  mkTracers trConfig = mdo
    ekgStore <- EKG.newStore
    EKG.registerGcMetrics ekgStore
    ekgTrace <- ekgTracer trConfig ekgStore

    stdoutTrace <- standardTracer

    -- We should initialize forwarding only if 'Forwarder' backend
    -- is presented in the node's configuration.
    (fwdTracer, dpTracer, kickoffForwarder) <-
      if forwarderBackendEnabled
        then do
          -- TODO: check if this is the correct way to use withIOManager
          (forwardSink, dpStore, kickoffForwarder') <- withIOManager $ \iomgr ->
            let initForwConf :: InitForwardingConfig
                initForwConf = case ncTraceForwardSocket nc of
                  Nothing -> InitForwardingNone
                  Just (initHowToConnect, initForwarderMode) ->
                    InitForwardingWith
                      { initNetworkMagic          = networkMagic
                      , initEKGStore              = Just ekgStore
                      , initOnForwardInterruption = Just $ \(SomeException e) ->
                          traceWith (nodeStateTracer tracers) (NodeTracingForwardingInterrupted initHowToConnect $ show e)
                      , initOnQueueOverflow       = Nothing
                      , ..
                      }

                forwardingConf :: TraceOptionForwarder
                forwardingConf = fromMaybe defaultForwarder (tcForwarder trConfig)
            in initForwardingDelayed iomgr forwardingConf initForwConf

          pure (forwardTracer (writeToSink forwardSink), dataPointTracer dpStore, kickoffForwarder')
        else
          -- Since 'Forwarder' backend isn't enabled, there is no forwarding.
          -- So we use nullTracers to ignore 'TraceObject's and 'DataPoint's.
          pure (Trace nullTracer, Trace nullTracer, pure ())

    tracers <- mkDispatchTracers
      nodeKernel
      stdoutTrace
      fwdTracer
      (Just ekgTrace)
      dpTracer
      trConfig
      p

    let
      kickoffPrometheusSimple = case prometheusSimple of
        Nothing -> pure ()
        Just ps ->
          let
            !nsTr            = nodeStateTracer tracers
            !tracePrometheus = NodePrometheusSimple >$< nsTr
          in runPrometheusSimple tracePrometheus ekgStore ps >>= link

    pure (kickoffForwarder, kickoffPrometheusSimple, tracers)

   where
    -- This backend can only be used globally, i.e. will always apply to the namespace root.
    -- Multiple definitions, especially with differing ports, are considered a *misconfiguration*.
    prometheusSimple :: Maybe (Bool, Maybe HostName, PortNumber)
    prometheusSimple =
      listToMaybe [ (noSuff, mHost, portNo)
                    | options                              <- Map.elems (tcOptions trConfig)
                    , ConfBackend backends'                <- options
                    , PrometheusSimple noSuff mHost portNo <- backends'
                    ]

    forwarderBackendEnabled :: Bool
    forwarderBackendEnabled =
      (any (any checkForwarder) . Map.elems) $ tcOptions trConfig

    checkForwarder :: ConfigOption -> Bool
    checkForwarder (ConfBackend backends') = Forwarder `elem` backends'
    checkForwarder _ = False
