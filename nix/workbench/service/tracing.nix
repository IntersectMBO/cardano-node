{ lib
, nodeSpec
, tracer
, tracing_backend
}:
cfg:

with lib;
let
  trace-dispatcher =
    recursiveUpdate
    (removeLegacyTracingOptions cfg)
  {
    UseTraceDispatcher   = true;

  ## Please see the generated tracing configuration reference at:
  ##
  ## https://github.com/input-output-hk/cardano-node/blob/master/doc/new-tracing/tracers_doc_generated.md#trace-messages
  ##
    TraceOptions  = {
      "" =
        { severity = "Notice";
          backends = [
            "Stdout MachineFormat"
            "EKGBackend"
            ] ++ optional tracer
                 "Forwarder";
        };

      ## These are comparision-specific config deviations from the default.
      ##
      ## "Resources".backends = ["EKGBackend"];

      "Net.AcceptPolicy".severity = "Info";
      "BlockFetch.Client".severity = "Info";
      "BlockFetch.Server".severity = "Info";
      "ChainDB".severity = "Info";
      "ChainSync.Client".severity = "Info";
      "ChainSync.ServerBlock".severity = "Info";
      "ChainSync.ServerHeader".severity = "Info";
      "Forge.Loop".severity = "Info";
      "Net.ConnectionManager".severity = "Info";
      "Net.Startup.DiffusionInit".severity = "Info";
      "Net.DNSResolver".severity = "Info";
      "Net.ErrorPolicy".severity = "Info";
      "Net.InboundGovernor".severity = "Info";
      "Net.Subscription.DNS".severity = "Info";
      "Net.Subscription.IP".severity = "Info";
      "Net.Peers".severity = "Info";
      "Net.PeerSelection".severity = "Info";
      "Net.Server".severity = "Info";
      "Mempool".severity = "Info";
      "Resources".severity= "Info";
      "TxSubmission.TxInbound".severity = "Info";

      "ChainDB.FollowerEvent.NewFollower".severity = "Debug";
      "ChainDB.FollowerEvent.FollowerNoLongerInMem".severity = "Debug";
      "ChainDB.AddBlockEvent.AddedBlockToQueue".severity = "Debug";
      "ChainDB.IteratorEvent.StreamFromVolatileDB".severity = "Debug";
      "ChainDB.GCEvent.PerformedGC".severity = "Debug";
      "ChainDB.GCEvent.ScheduledGC".severity = "Debug";
      "ChainDB.ImmDbEvent.CacheEvent.CurrentChunkHit".severity = "Debug";
      "ChainDB.ImmDbEvent.CacheEvent.PastChunkHit".severity = "Debug";
      "Forge.Loop.BlockContext".severity = "Debug";
      "Forge.Loop.LedgerState".severity = "Debug";
      "Forge.Loop.LedgerView".severity = "Debug";
      "Forge.Loop.ForgingMempoolSnapshot".severity = "Debug";
      "Forge.Loop.ForgeTickedLedgerState".severity = "Debug";
      "TxSubmission.TxInbound.CanRequestMoreTxs".severity = "Debug";
      "TxSubmission.TxInbound.CannotRequestMoreTxs".severity = "Debug";
      "TxSubmission.TxInbound.Collected".severity = "Debug";
      "TxSubmission.TxInbound.Processed".severity = "Debug";

      "TraceBenchTxSubServAck".severity = "Debug";
      "TraceBenchTxSubSummary".severity = "Debug";
      "TraceTxSubmissionCollected".severity = "Debug";
      "TraceTxSubmissionProcessed".severity = "Debug";
    };
  };



  iohk-monitoring =
    recursiveUpdate
    (removeAttrs cfg
      [ "setupScribes" ])
  {
    defaultScribes = [
      [ "StdoutSK" "stdout" ]
    ];
    setupScribes =
      [{
        scKind   = "StdoutSK";
        scName   = "stdout";
        scFormat = "ScJson";
      }];
    minSeverity                 = "Debug";
    TraceMempool                = true;
    TraceTxInbound              = true;
    TraceBlockFetchClient       = true;
    TraceBlockFetchServer       = true;
    TraceChainSyncHeaderServer  = true;
    TraceChainSyncClient        = true;
    options = {
      mapBackends = {
        "cardano.node.resources" = [ "KatipBK" ];
      };
    };
  };

  ##
  ## removeLegacyTracingOptions :: NodeConfig -> NodeConfig
  ##
  removeLegacyTracingOptions = cfg:
    removeAttrs cfg
    [
      "TraceAcceptPolicy"
      "TraceBlockchainTime"
      "TraceBlockFetchClient"
      "TraceBlockFetchDecisions"
      "TraceBlockFetchProtocol"
      "TraceBlockFetchProtocolSerialised"
      "TraceBlockFetchServer"
      "TraceChainDb"
      "TraceChainSyncClient"
      "TraceChainSyncBlockServer"
      "TraceChainSyncHeaderServer"
      "TraceChainSyncProtocol"
      "TraceConnectionManager"
      "TraceConnectionManagerCounters"
      "TraceConnectionManagerTransitions"
      "DebugPeerSelectionInitiator"
      "DebugPeerSelectionInitiatorResponder"
      "TraceDiffusionInitialization"
      "TraceDnsResolver"
      "TraceDnsSubscription"
      "TraceErrorPolicy"
      "TraceForge"
      "TraceForgeStateInfo"
      "TraceHandshake"
      "TraceIpSubscription"
      "TraceKeepAliveClient"
      "TraceLedgerPeers"
      "TraceLocalChainSyncProtocol"
      "TraceLocalConnectionManager"
      "TraceLocalErrorPolicy"
      "TraceLocalHandshake"
      "TraceLocalInboundGovernor"
      "TraceLocalRootPeers"
      "TraceLocalServer"
      "TraceLocalStateQueryProtocol"
      "TraceLocalTxMonitorProtocol"
      "TraceLocalTxSubmissionProtocol"
      "TraceLocalTxSubmissionServer"
      "TraceMempool"
      "TraceMux"
      "TraceLocalMux"
      "TracePeerSelection"
      "TracePeerSelectionCounters"
      "TracePeerSelectionActions"
      "TracePublicRootPeers"
      "TraceServer"
      "TraceInboundGovernor"
      "TraceInboundGovernorCounters"
      "TraceInboundGovernorTransitions"
      "TraceTxInbound"
      "TraceTxOutbound"
      "TraceTxSubmissionProtocol"
      "TraceTxSubmission2Protocol"
      "TracingVerbosity"
      "defaultBackends"
      "defaultScribes"
      "hasEKG"
      "hasPrometheus"
      "minSeverity"
      "options"
      "rotation"
      "setupBackends"
      "setupScribes"
    ];
in
{
  inherit trace-dispatcher iohk-monitoring;
}.${tracing_backend}
