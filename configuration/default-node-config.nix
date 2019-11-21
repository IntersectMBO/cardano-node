{
  minSeverity = "Debug";
  rotation = {
    rpLogLimitBytes = 5000000;
    rpKeepFilesNum = 10;
    rpMaxAgeHours = 24;
  };
  setupBackends = [
    "KatipBK"
  ];
  defaultBackends = [
    "KatipBK"
  ];
  setupScribes = [
    { scKind = "StdoutSK";
      scName = "stdout";
      scFormat = "ScText";
      scRotation = null;
    }
  ];
  defaultScribes = [
    [ "StdoutSK"
      "stdout"
    ]
  ];
  options = {
    cfokey.value = "Release-1.0.0";
    mapSubtrace = {
      "#ekgview" = {
        contents = [
          [ { tag = "Contains";
              contents = "cardano.epoch-validation.benchmark"; }
            [ { tag = "Contains";
                contents = ".monoclock.basic."; }
            ]
          ]
          [ { tag = "Contains";
              contents = "cardano.epoch-validation.benchmark";
            }
            [ { tag = "Contains";
                contents = "diff.RTS.cpuNs.timed."; }
            ]
          ]
          [ { tag = "StartsWith";
              contents = "#ekgview.#aggregation.cardano.epoch-validation.benchmark";
            }
            [ { tag = "Contains";
                contents = "diff.RTS.gcNum.timed."; }
            ]
          ]
        ];
        subtrace = "FilterTrace";
      };
      "cardano.epoch-validation.utxo-stats"  = { subtrace = "NoTrace"; };
      "cardano.#messagecounters.aggregation" = { subtrace = "NoTrace"; };
      "cardano.#messagecounters.ekgview"     = { subtrace = "NoTrace"; };
      "cardano.#messagecounters.switchboard" = { subtrace = "NoTrace"; };
      "cardano.#messagecounters.katip"       = { subtrace = "NoTrace"; };
      "cardano.#messagecounters.monitoring"  = { subtrace = "NoTrace"; };
    };
    mapBackends = {
      "cardano.node.metrics.ChainDB" = [
        "EKGViewBK"
        { kind = "UserDefinedBK"; name = "LiveViewBackend"; }
      ];
      "cardano.node.metrics" = [
        { kind = "UserDefinedBK"; name = "LiveViewBackend"; }
      ];
      "cardano.node.BlockFetchDecision" = [
        { kind = "UserDefinedBK"; name = "LiveViewBackend"; }
      ];
      "cardano.node.peers.BlockFetchDecision" = [
        { kind = "UserDefinedBK"; name = "LiveViewBackend"; }
      ];
    };
  };
  NodeId = 0;
  Protocol = "RealPBFT";
  NumCoreNodes = 1;
  RequiresNetworkMagic = "RequiresMagic";
  PBftSignatureThreshold = 0.7;
  TurnOnLogging = true;
  ViewMode = "SimpleView";
  TurnOnLogMetrics = false;
  ResponseTimeout = 30000000;
  PollDelay = 1800000000;
  Servers = [
    "0.pool.ntp.org"
    "2.pool.ntp.org"
    "3.pool.ntp.org"
  ];
  ApplicationName = "cardano-sl";
  ApplicationVersion = 1;
  LastKnownBlockVersion-Major = 0;
  LastKnownBlockVersion-Minor = 2;
  LastKnownBlockVersion-Alt = 0;
  MemPoolLimitTx = 200;
  AssetLockedSrcAddress = [];
  CacheParameter = 500;
  MessageCacheTimeout = 30;
  NetworkDiameter = 18;
  RecoveryHeadersMessage = 2200;
  StreamWindow = 2048;
  NonCriticalCQBootstrap = 0.95;
  NonCriticalCQ = 0.8;
  CriticalCQBootstrap = 0.8888;
  CriticalCQ = 0.654321;
  CriticalForkThreshold = 3;
  FixedTimeCQ = 3600;
  SlotLength = 20000;
  NetworkConnectionTimeout = 15000;
  HandshakeTimeout = 30000;
  CA-Organization = "Input Output HK";
  CA-CommonName = "Cardano SL Self-Signed Root CA";
  CA-ExpiryDays = 3600;
  CA-AltDNS = [];
  Server-Organization = "Input Output HK";
  Server-CommonName = "Cardano SL Server";
  Server-ExpiryDays = 3600;
  Server-AltDNS = [
    "localhost"
    "localhost.localdomain"
    "127.0.0.1"
    "::1"
  ];
  Wallet-Organization = "Input Output HK";
  Wallet-CommonName = "Daedalus Wallet";
  Wallet-ExpiryDays = 3600;
  Wallet-AltDNS = [];
  Enabled = false;
  Rate = 0;
  Period = "";
  Burst = 0;
}
