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
  };
}
