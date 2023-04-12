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
}
