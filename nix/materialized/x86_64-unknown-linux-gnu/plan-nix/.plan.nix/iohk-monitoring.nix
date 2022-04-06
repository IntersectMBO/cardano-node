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
    flags = { disable-observables = false; performance-test-queue = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "iohk-monitoring"; version = "0.1.11.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "logging, benchmarking and monitoring framework";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "src/Cardano/BM/Counters/os-support-darwin.h"
        "src/Cardano/BM/Counters/os-support-win.h"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async-timer" or (errorHandler.buildDepError "async-timer"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
          (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        modules = ((([
          "Paths_iohk_monitoring"
          "Cardano/BM/Configuration"
          "Cardano/BM/Configuration/Model"
          "Cardano/BM/Configuration/Static"
          "Cardano/BM/Counters"
          "Cardano/BM/Counters/Common"
          "Cardano/BM/Counters/Dummy"
          "Cardano/BM/Stats"
          "Cardano/BM/Stats/Resources"
          "Cardano/BM/Data/Aggregated"
          "Cardano/BM/Data/AggregatedKind"
          "Cardano/BM/Data/Backend"
          "Cardano/BM/Data/BackendKind"
          "Cardano/BM/Data/Configuration"
          "Cardano/BM/Data/Counter"
          "Cardano/BM/Data/LogItem"
          "Cardano/BM/Data/MonitoringEval"
          "Cardano/BM/Data/Observable"
          "Cardano/BM/Data/Output"
          "Cardano/BM/Data/Rotation"
          "Cardano/BM/Data/Severity"
          "Cardano/BM/Data/SubTrace"
          "Cardano/BM/Data/Trace"
          "Cardano/BM/Data/Tracer"
          "Cardano/BM/Data/Transformers"
          "Cardano/BM/Internal/ElidingTracer"
          "Cardano/BM/Tracing"
          "Cardano/BM/Backend/Log"
          "Cardano/BM/Backend/LogBuffer"
          "Cardano/BM/Backend/ProcessQueue"
          "Cardano/BM/Backend/Switchboard"
          "Cardano/BM/Plugin"
          "Cardano/BM/Rotator"
          "Cardano/BM/Setup"
          "Cardano/BM/Trace"
          "Cardano/BM/Tracer"
          "Cardano/BM/IOManager"
          "Cardano/BM/Snocket"
          ] ++ (pkgs.lib).optionals (!flags.disable-observables) [
          "Cardano/BM/Observer/Monadic"
          "Cardano/BM/Observer/STM"
          ]) ++ (pkgs.lib).optional (system.isLinux) "Cardano/BM/Counters/Linux") ++ (pkgs.lib).optional (system.isWindows) "Cardano/BM/Counters/Windows") ++ (pkgs.lib).optional (system.isOsx) "Cardano/BM/Counters/Darwin";
        cSources = (pkgs.lib).optional (system.isWindows) "src/Cardano/BM/Counters/os-support-win.c" ++ (pkgs.lib).optional (system.isOsx) "src/Cardano/BM/Counters/os-support-darwin.c";
        hsSourceDirs = [ "src" ];
        includeDirs = (pkgs.lib).optional (system.isWindows) "src/Cardano/BM/Counters/" ++ (pkgs.lib).optional (system.isOsx) "src/Cardano/BM/Counters/";
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            ];
          buildable = true;
          modules = [
            "Cardano/BM/Test/Trace"
            "Cardano/BM/Test/STM"
            "Cardano/BM/Test/Configuration"
            "Cardano/BM/Test/LogItem"
            "Cardano/BM/Test/Mock"
            "Cardano/BM/Test/Rotator"
            "Cardano/BM/Test/Routing"
            "Cardano/BM/Test/Structured"
            "Cardano/BM/Test/Tracer"
            "Cardano/BM/Test/Aggregated"
            "Cardano/BM/Arbitrary"
            "Cardano/BM/Arbitrary/Aggregated"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Test.lhs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "8";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "8";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/iohk-monitoring; echo source root reset to $sourceRoot";
    }