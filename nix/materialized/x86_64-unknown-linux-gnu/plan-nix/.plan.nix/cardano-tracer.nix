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
      specVersion = "2.4";
      identifier = { name = "cardano-tracer"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "2022 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A service for logging and monitoring over Cardano nodes.";
      description = "See README for more info";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" "CHANGELOG.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."async-extras" or (errorHandler.buildDepError "async-extras"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."snap-blaze" or (errorHandler.buildDepError "snap-blaze"))
          (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
          (hsPkgs."snap-server" or (errorHandler.buildDepError "snap-server"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
          (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (system.isLinux) (hsPkgs."libsystemd-journal" or (errorHandler.buildDepError "libsystemd-journal"));
        buildable = true;
        modules = [
          "Paths_cardano_tracer"
          "Cardano/Tracer/Acceptors/Client"
          "Cardano/Tracer/Acceptors/Run"
          "Cardano/Tracer/Acceptors/Server"
          "Cardano/Tracer/Acceptors/Utils"
          "Cardano/Tracer/Handlers/Logs/File"
          "Cardano/Tracer/Handlers/Logs/Journal"
          "Cardano/Tracer/Handlers/Logs/Rotator"
          "Cardano/Tracer/Handlers/Logs/TraceObjects"
          "Cardano/Tracer/Handlers/Logs/Utils"
          "Cardano/Tracer/Handlers/Metrics/Monitoring"
          "Cardano/Tracer/Handlers/Metrics/Prometheus"
          "Cardano/Tracer/Handlers/Metrics/Servers"
          "Cardano/Tracer/CLI"
          "Cardano/Tracer/Configuration"
          "Cardano/Tracer/Run"
          "Cardano/Tracer/Types"
          "Cardano/Tracer/Utils"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "demo-forwarder-lib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."cardano-tracer" or (errorHandler.buildDepError "cardano-tracer"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
            ];
          buildable = true;
          modules = [ "Cardano/Tracer/Test/Forwarder" ];
          hsSourceDirs = [ "test" ];
          };
        "demo-acceptor-lib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async-extras" or (errorHandler.buildDepError "async-extras"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-tracer" or (errorHandler.buildDepError "cardano-tracer"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
            ];
          buildable = true;
          modules = [ "Cardano/Tracer/Test/Acceptor" ];
          hsSourceDirs = [ "test" ];
          };
        };
      exes = {
        "cardano-tracer" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-tracer" or (errorHandler.buildDepError "cardano-tracer"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          modules = [ "Paths_cardano_tracer" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "cardano-tracer.hs" ];
          };
        "demo-forwarder" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-tracer".components.sublibs.demo-forwarder-lib or (errorHandler.buildDepError "cardano-tracer:demo-forwarder-lib"))
            ];
          buildable = true;
          hsSourceDirs = [ "demo/ssh" ];
          mainPath = [ "forwarder.hs" ];
          };
        "demo-acceptor" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-tracer".components.sublibs.demo-acceptor-lib or (errorHandler.buildDepError "cardano-tracer:demo-acceptor-lib"))
            ];
          buildable = true;
          hsSourceDirs = [ "demo" ];
          mainPath = [ "acceptor.hs" ];
          };
        };
      tests = {
        "cardano-tracer-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-tracer" or (errorHandler.buildDepError "cardano-tracer"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
            ];
          buildable = true;
          modules = [
            "Cardano/Tracer/Test/Forwarder"
            "Cardano/Tracer/Test/DataPoint/Tests"
            "Cardano/Tracer/Test/Logs/Tests"
            "Cardano/Tracer/Test/Restart/Tests"
            "Cardano/Tracer/Test/Queue/Tests"
            "Cardano/Tracer/Test/Utils"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "cardano-tracer-test.hs" ];
          };
        };
      benchmarks = {
        "cardano-tracer-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-tracer" or (errorHandler.buildDepError "cardano-tracer"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            ];
          buildable = true;
          hsSourceDirs = [ "bench" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../cardano-tracer; }