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
    TraceOptionResourceFrequency = 1000;

  ## Please see the generated tracing configuration reference at:
  ##
  ## https://github.com/intersectmbo/cardano-node/blob/master/doc/new-tracing/tracers_doc_generated.md#trace-messages
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

      ## This is a list with all tracers, adopted to the on and off state
      ## in old tracing
      "BlockFetch.Client".severity = "Debug";
      "BlockFetch.Decision".severity = "Notice";
      "BlockFetch.Remote".severity = "Notice";
      "BlockFetch.Remote.Serialised".severity = "Notice";
      "BlockFetch.Server".severity = "Debug";
      "BlockchainTime".severity = "Notice";
      "ChainDB".severity = "Debug";
      "ChainDB.ReplayBlock.LedgerReplay".severity = "Notice";
      "ChainSync.Client".severity = "Debug";
      "ChainSync.Local".severity = "Notice";
      "ChainSync.Remote".severity = "Notice";
      "ChainSync.Remote.Serialised".severity = "Notice";
      "ChainSync.ServerBlock".severity = "Notice";
      "ChainSync.ServerHeader".severity = "Debug";
      "Forge.Loop".severity = "Debug";
      "Forge.StateInfo".severity = "Debug";
      "Mempool".severity = "Debug";
      "Net".severity = "Notice";
      "Net.AcceptPolicy".severity = "Debug";
      "Net.ConnectionManager.Local".severity = "Debug";
      "Net.ConnectionManager.Remote".severity = "Debug";
      "Net.DNSResolver".severity = "Notice";
      "Net.ErrorPolicy.Local".severity = "Debug";
      "Net.ErrorPolicy.Remote".severity = "Debug";
      "Net.Handshake.Local".severity = "Debug";
      "Net.Handshake.Remote".severity = "Debug";
      "Net.InboundGovernor.Local".severity = "Debug";
      "Net.InboundGovernor.Remote".severity = "Debug";
      "Net.InboundGovernor.Transition".severity = "Debug";
      "Net.Mux.Local".severity = "Notice";
      "Net.Mux.Remote".severity = "Notice";
      "Net.PeerSelection.Actions".severity = "Debug";
      "Net.PeerSelection.Counters".severity = "Debug";
      "Net.PeerSelection.Initiator".severity = "Notice";
      "Net.PeerSelection.Responder".severity = "Notice";
      "Net.PeerSelection.Selection".severity = "Debug";
      "Net.Peers.Ledger".severity = "Debug";
      "Net.Peers.List".severity = "Notice";
      "Net.Peers.LocalRoot".severity = "Debug";
      "Net.Peers.PublicRoot".severity = "Debug";
      "Net.Server.Local".severity = "Debug";
      "Net.Server.Remote".severity = "Debug";
      "Net.Subscription.DNS".severity = "Debug";
      "Net.Subscription.IP".severity = "Debug";
      "NodeState".severity = "Notice";
      "Resources".severity = "Debug";
      "Shutdown".severity = "Notice";
      "Startup".severity = "Notice";
      "Startup.DiffusionInit".severity = "Debug";
      "StateQueryServer".severity = "Notice";
      "TxSubmission.Local".severity = "Notice";
      "TxSubmission.LocalServer".severity = "Notice";
      "TxSubmission.MonitorClient".severity = "Notice";
      "TxSubmission.Remote".severity = "Notice";
      "TxSubmission.TxInbound".severity = "Debug";
      "TxSubmission.TxOutbound".severity = "Notice";
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
