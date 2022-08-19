{ nodeSpec
, tracer
}:
{
  UseTraceDispatcher   = true;

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
  };
}
