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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "ouroboros-network-framework";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
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
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        modules = [
          "Ouroboros/Network/Linger"
          "Data/Cache"
          "Data/Wedge"
          "Ouroboros/Network/CodecCBORTerm"
          "Ouroboros/Network/Channel"
          "Ouroboros/Network/Driver"
          "Ouroboros/Network/Driver/Simple"
          "Ouroboros/Network/Driver/Limits"
          "Ouroboros/Network/ErrorPolicy"
          "Ouroboros/Network/IOManager"
          "Ouroboros/Network/Mux"
          "Ouroboros/Network/MuxMode"
          "Ouroboros/Network/Util/ShowProxy"
          "Ouroboros/Network/Protocol/Handshake"
          "Ouroboros/Network/Protocol/Handshake/Type"
          "Ouroboros/Network/Protocol/Handshake/Codec"
          "Ouroboros/Network/Protocol/Handshake/Client"
          "Ouroboros/Network/Protocol/Handshake/Server"
          "Ouroboros/Network/Protocol/Handshake/Version"
          "Ouroboros/Network/Protocol/Handshake/Unversioned"
          "Ouroboros/Network/Protocol/Limits"
          "Ouroboros/Network/ConnectionId"
          "Ouroboros/Network/ConnectionHandler"
          "Ouroboros/Network/ConnectionManager/Types"
          "Ouroboros/Network/ConnectionManager/Core"
          "Ouroboros/Network/InboundGovernor"
          "Ouroboros/Network/InboundGovernor/Event"
          "Ouroboros/Network/InboundGovernor/State"
          "Ouroboros/Network/InboundGovernor/ControlChannel"
          "Ouroboros/Network/RethrowPolicy"
          "Ouroboros/Network/Server/ConnectionTable"
          "Ouroboros/Network/Server/Socket"
          "Ouroboros/Network/Server/RateLimiting"
          "Ouroboros/Network/Server2"
          "Ouroboros/Network/Snocket"
          "Ouroboros/Network/Socket"
          "Ouroboros/Network/Subscription"
          "Ouroboros/Network/Subscription/Client"
          "Ouroboros/Network/Subscription/Dns"
          "Ouroboros/Network/Subscription/Ip"
          "Ouroboros/Network/Subscription/PeerState"
          "Ouroboros/Network/Subscription/Subscriber"
          "Ouroboros/Network/Subscription/Worker"
          "Simulation/Network/Snocket"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "demo-ping-pong" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          hsSourceDirs = [ "demo" "test" ];
          mainPath = [
            "ping-pong.hs"
            ] ++ (pkgs.lib).optional (system.isWindows) "";
          };
        "demo-connection-manager" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ];
          buildable = true;
          hsSourceDirs = [ "demo" "test" ];
          mainPath = [ "connection-manager.hs" ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network-testing" or (errorHandler.buildDepError "ouroboros-network-testing"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          modules = [
            "Test/Ouroboros/Network/ConnectionManager"
            "Test/Ouroboros/Network/Driver"
            "Test/Ouroboros/Network/Orphans"
            "Test/Ouroboros/Network/Server2"
            "Test/Ouroboros/Network/Socket"
            "Test/Ouroboros/Network/Subscription"
            "Test/Ouroboros/Network/RateLimiting"
            "Test/Simulation/Network/Snocket"
            ];
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
    postUnpack = "sourceRoot+=/ouroboros-network-framework; echo source root reset to $sourceRoot";
    }