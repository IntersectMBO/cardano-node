{ profile, nodeSpec }:
{
  UseTraceDispatcher   = true;

  TraceOptions  = {
    ""                            = { severity = "Debug";
                                      backends = [
                                        "Stdout MachineFormat"
                                        "EKGBackend"
                                      ] ++ (if !profile.node.tracer then [] else
                                      [
                                        "Forwarder"
                                      ]);
                                    };
    "Node.AcceptPolicy"             = { severity = "Info"; };
    "Node.ChainDB"                  = { severity = "Info"; };
    "Node.DNSResolver"              = { severity = "Info"; };
    "Node.DNSSubscription"          = { severity = "Info"; };
    "Node.DiffusionInit"            = { severity = "Info"; };
    "Node.ErrorPolicy"              = { severity = "Info"; };
    "Node.Forge"                    = { severity = "Info"; };
    "Node.IpSubscription"           = { severity = "Info"; };
    "Node.LocalErrorPolicy"         = { severity = "Info"; };
    "Node.Mempool"                  = { severity = "Info"; };
    "Node.Resources"                = { severity = "Info"; };

    "Node.BlockFetch.NodeToNode"    = { severity = "Silence"; };
    "Node.BlockFetchDecision"       = { severity = "Silence"; };
    "Node.BlockFetchSerialised"     = { severity = "Silence"; };
    "Node.ChainSyncNode.NodeToNode" = { severity = "Silence"; };
    "Node.ChainSyncSerialised"      = { severity = "Silence"; };
    "Node.LocalHandshake"           = { severity = "Silence"; };
    "Node.Mux"                      = { severity = "Silence"; };
    "Node.MuxLocal"                 = { severity = "Silence"; };
    "Node.TxOutbound"               = { severity = "Silence"; };
    "Node.TxSubmission2"            = { severity = "Silence"; };

    "Node.BlockFetchClient"         = { detail = "DMinimal"; };
    "Node.TxSubmission2"            = { detail = "DMinimal"; };
    ## Commented out because the legacy doesn't limit this message:
    # "Node.BlockFetchClient.CompletedBlockFetch"                     = { maxFrequency = 2.0; };
    "Node.ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate"  = { maxFrequency = 2.0; };
    "Node.ChainDB.AddBlockEvent.AddedBlockToQueue"                  = { maxFrequency = 2.0; };
    "Node.ChainDB.AddBlockEvent.AddedBlockToVolatileDB"             = { maxFrequency = 2.0; };
    "Node.ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB"  = { maxFrequency = 2.0; };
  };

  TraceOptionForwarder = {
    # mode = "Responder";
    mode = "Initiator";
  };
}
