{ profile, nodeSpec }:
{
  UseTraceDispatcher   = true;

  TraceOptions  = {
    ""                            = { severity = "Notice";
                                      backends = [
                                        "Stdout MachineFormat"
                                        "EKGBackend"
                                      ] ++ (if !profile.node.tracer then [] else
                                      [
                                        "Forwarder"
                                      ]);
                                    };
    "AcceptPolicy"             = { severity = "Info"; };
    "BlockFetchClient"         = { severity = "Info"; detail = "DMinimal"; };
    "BlockFetchClient.CompletedBlockFetch" = { maxFrequency = 2.0; };
    "BlockFetchServer"         = { severity = "Info"; };
    "ChainDB"                  = { severity = "Info"; };
    "ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate"  = { maxFrequency = 2.0; };
    "ChainDB.AddBlockEvent.AddedBlockToQueue"                  = { maxFrequency = 2.0; };
    "ChainDB.AddBlockEvent.AddedBlockToVolatileDB"             = { maxFrequency = 2.0; };
    "ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB"  = { maxFrequency = 2.0; };
    "ChainSyncClient"          = { severity = "Info"; detail = "DMinimal"; };
    "ChainSyncServerHeader"    = { severity = "Info"; };
    "ChainSyncServerBlock"     = { severity = "Info"; };
    "DNSResolver"              = { severity = "Info"; };
    "DNSSubscription"          = { severity = "Info"; };
    "DiffusionInit"            = { severity = "Info"; };
    "ErrorPolicy"              = { severity = "Info"; };
    "Forge"                    = { severity = "Info"; };
    "IpSubscription"           = { severity = "Info"; };
    "LocalErrorPolicy"         = { severity = "Info"; };
    "Mempool"                  = { severity = "Info"; };
    "Resources"                = { severity = "Info"; };
    "TxSubmission2"            = { detail = "DMinimal"; };
  };
}
