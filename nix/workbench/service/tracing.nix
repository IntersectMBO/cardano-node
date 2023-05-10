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

      ## These are benchmarking-specific config deviations from the default.
      ##
      "BlockFetch".severity = "Debug";
      "BlockFetch.Client.CompletedBlockFetch".maxFrequency = 0;
      "ChainSync".severity = "Debug";
      "ChainSync.Client.DownloadedHeader".maxFrequency = 0;
      "Forge.Loop.BlockContext".severity = "Debug";
      "Forge.Loop.LedgerState".severity = "Debug";
      "Forge.Loop.LedgerView".severity = "Debug";
      "Forge.Loop.MempoolSnapshot".severity = "Debug";
      "Forge.Loop.TickedLedgerState".severity = "Debug";
      "Startup".severity = "Debug";
    };
  };

  iohk-monitoring =
    removeAttrs cfg
      [ "setupScribes" ]
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
