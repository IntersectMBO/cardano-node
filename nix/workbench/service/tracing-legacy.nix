{ nodeSpec
, ...
}:
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
}
