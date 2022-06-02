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
    "Node.AcceptPolicy"             = { severity = "Info"; };
    "Node.BlockFetchClient"         = { severity = "Info"; detail = "DMinimal"; };
    "Node.BlockFetchServer"         = { severity = "Info"; };
    "Node.ChainDB"                  = { severity = "Info"; };
    "Node.ChainSyncClient"          = { severity = "Info"; detail = "DMinimal"; };
    "Node.ChainSyncServerHeader"    = { severity = "Info"; };
    "Node.ChainSyncServerBlock"     = { severity = "Info"; };
    "Node.DNSResolver"              = { severity = "Info"; };
    "Node.DNSSubscription"          = { severity = "Info"; };
    "Node.DiffusionInit"            = { severity = "Info"; };
    "Node.ErrorPolicy"              = { severity = "Info"; };
    "Node.Forge"                    = { severity = "Info"; };
    "Node.IpSubscription"           = { severity = "Info"; };
    "Node.LocalErrorPolicy"         = { severity = "Info"; };
    "Node.Mempool"                  = { severity = "Info"; };
    "Node.Resources"                = { severity = "Info"; };

    "Node.TxSubmission2"            = { detail = "DMinimal"; };

    ## Commented out because the legacy doesn't limit this message:
    "Node.BlockFetchClient.CompletedBlockFetch"                     = { maxFrequency = 2.0; };
    "Node.ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate"  = { maxFrequency = 2.0; };
    "Node.ChainDB.AddBlockEvent.AddedBlockToQueue"                  = { maxFrequency = 2.0; };
    "Node.ChainDB.AddBlockEvent.AddedBlockToVolatileDB"             = { maxFrequency = 2.0; };
    "Node.ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB"  = { maxFrequency = 2.0; };
  };
}
