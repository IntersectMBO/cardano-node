{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { asserts = false; ipv6 = false; tracetcpinfo = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "network-mux"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "duncan@well-typed.com, marcin.szamotulski@iohk.io, marc.fontaine@iohk.io, karl.knutsson@iohk.io, alex@well-typed.com, neil.davies@pnsol.com";
      author = "Duncan Coutts, Marc Fontaine, Karl Knutsson, Marcin Szamotulski, Alexander Vieth, Neil Davies";
      homepage = "";
      url = "";
      synopsis = "Multiplexing library";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."statistics-linreg" or (errorHandler.buildDepError "statistics-linreg"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ];
        buildable = true;
        modules = ([
          "Network/Mux"
          "Network/Mux/Channel"
          "Network/Mux/Codec"
          "Network/Mux/Compat"
          "Network/Mux/Egress"
          "Network/Mux/Ingress"
          "Network/Mux/Time"
          "Network/Mux/Timeout"
          "Network/Mux/Types"
          "Network/Mux/Trace"
          "Network/Mux/Bearer/AttenuatedChannel"
          "Network/Mux/Bearer/Pipe"
          "Network/Mux/Bearer/Queues"
          "Network/Mux/Bearer/Socket"
          "Network/Mux/DeltaQ/TraceStats"
          "Network/Mux/DeltaQ/TraceStatsSupport"
          "Network/Mux/DeltaQ/TraceTransformer"
          "Network/Mux/DeltaQ/TraceTypes"
          "Network/Mux/TCPInfo"
          "Control/Concurrent/JobPool"
          ] ++ (pkgs.lib).optional (system.isLinux) "Network/Mux/TCPInfo/Linux") ++ (pkgs.lib).optional (system.isWindows) "Network/Mux/Bearer/NamedPipe";
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "mux-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            ] ++ (if system.isWindows
            then [
              (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
              (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
              ]
            else [
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ]);
          buildable = true;
          modules = [ "Test/Mux/ReqResp" ];
          hsSourceDirs = [ "demo" "test" ];
          mainPath = [ "mux-demo.hs" ] ++ [ "" ];
          };
        "cardano-ping" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."tdigest" or (errorHandler.buildDepError "tdigest"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if system.isWindows then false else true;
          modules = [ "Linger" ];
          hsSourceDirs = [ "demo" ];
          mainPath = [ "cardano-ping.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
          buildable = true;
          modules = [ "Test/Mux" "Test/Mux/ReqResp" "Test/Mux/Timeout" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "10";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "10";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/network-mux; echo source root reset to $sourceRoot";
    }