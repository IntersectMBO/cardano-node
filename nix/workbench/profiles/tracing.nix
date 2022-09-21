{ nodeSpec
, tracer
}:
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
        ] ++ (if !tracer then [] else
          [
            "Forwarder"
          ]);
      };
    BlockFetch.severity = "Info";
    ChainSync.severity = "Info";
    "Forge.Loop.BlockContext".severity = "Info";
    "Forge.Loop.LedgerView".severity = "Info";
    "Forge.Loop.LedgerState".severity = "Info";
  };
}
