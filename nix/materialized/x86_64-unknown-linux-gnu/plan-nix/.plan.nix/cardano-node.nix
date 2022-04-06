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
    flags = { unexpected_thunks = false; systemd = true; };
    package = {
      specVersion = "3.0";
      identifier = { name = "cardano-node"; version = "1.33.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The cardano full node";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-git-rev" or (errorHandler.buildDepError "cardano-git-rev"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
          (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."lobemo-backend-aggregation" or (errorHandler.buildDepError "lobemo-backend-aggregation"))
          (hsPkgs."lobemo-backend-ekg" or (errorHandler.buildDepError "lobemo-backend-ekg"))
          (hsPkgs."lobemo-backend-monitoring" or (errorHandler.buildDepError "lobemo-backend-monitoring"))
          (hsPkgs."lobemo-backend-trace-forwarder" or (errorHandler.buildDepError "lobemo-backend-trace-forwarder"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
          (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
          (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
          (hsPkgs."trace-resources" or (errorHandler.buildDepError "trace-resources"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))) ++ (pkgs.lib).optionals (system.isLinux && flags.systemd) [
          (hsPkgs."lobemo-scribe-systemd" or (errorHandler.buildDepError "lobemo-scribe-systemd"))
          (hsPkgs."systemd" or (errorHandler.buildDepError "systemd"))
          ];
        buildable = true;
        modules = [
          "Paths_cardano_node"
          "Cardano/Node/Configuration/Logging"
          "Cardano/Node/Configuration/NodeAddress"
          "Cardano/Node/Configuration/POM"
          "Cardano/Node/Configuration/Socket"
          "Cardano/Node/Configuration/Topology"
          "Cardano/Node/Configuration/TopologyP2P"
          "Cardano/Node/Handlers/Shutdown"
          "Cardano/Node/Handlers/TopLevel"
          "Cardano/Node/Orphans"
          "Cardano/Node/Protocol"
          "Cardano/Node/Protocol/Alonzo"
          "Cardano/Node/Protocol/Byron"
          "Cardano/Node/Protocol/Cardano"
          "Cardano/Node/Protocol/Shelley"
          "Cardano/Node/Protocol/Types"
          "Cardano/Node/Parsers"
          "Cardano/Node/Queries"
          "Cardano/Node/Run"
          "Cardano/Node/STM"
          "Cardano/Node/Startup"
          "Cardano/Node/TraceConstraints"
          "Cardano/Node/Tracing"
          "Cardano/Node/Types"
          "Cardano/Node/Tracing/API"
          "Cardano/Node/Tracing/Compat"
          "Cardano/Node/Tracing/Documentation"
          "Cardano/Node/Tracing/Era/Byron"
          "Cardano/Node/Tracing/Era/HardFork"
          "Cardano/Node/Tracing/Era/Shelley"
          "Cardano/Node/Tracing/Peers"
          "Cardano/Node/Tracing/StateRep"
          "Cardano/Node/Tracing/Tracers"
          "Cardano/Node/Tracing/Tracers/BlockReplayProgress"
          "Cardano/Node/Tracing/Tracers/ChainDB"
          "Cardano/Node/Tracing/Tracers/Consensus"
          "Cardano/Node/Tracing/Tracers/Diffusion"
          "Cardano/Node/Tracing/Tracers/KESInfo"
          "Cardano/Node/Tracing/Tracers/StartLeadershipCheck"
          "Cardano/Node/Tracing/Tracers/ForgingThreadStats"
          "Cardano/Node/Tracing/Tracers/Resources"
          "Cardano/Node/Tracing/Tracers/Peer"
          "Cardano/Node/Tracing/Tracers/Startup"
          "Cardano/Node/Tracing/Tracers/Shutdown"
          "Cardano/Node/Tracing/Tracers/P2P"
          "Cardano/Node/Tracing/Tracers/NonP2P"
          "Cardano/Node/Tracing/Tracers/NodeToClient"
          "Cardano/Node/Tracing/Tracers/NodeToNode"
          "Cardano/Node/Tracing/Formatting"
          "Cardano/Node/Tracing/Render"
          "Cardano/Tracing/Config"
          "Cardano/Tracing/Metrics"
          "Cardano/Tracing/Peer"
          "Cardano/Tracing/Render"
          "Cardano/Tracing/Startup"
          "Cardano/Tracing/Shutdown"
          "Cardano/Tracing/Tracers"
          "Cardano/Tracing/OrphanInstances/Byron"
          "Cardano/Tracing/OrphanInstances/Common"
          "Cardano/Tracing/OrphanInstances/Consensus"
          "Cardano/Tracing/OrphanInstances/HardFork"
          "Cardano/Tracing/OrphanInstances/Network"
          "Cardano/Tracing/OrphanInstances/Shelley"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "cardano-node" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-git-rev" or (errorHandler.buildDepError "cardano-git-rev"))
            (hsPkgs."cardano-node" or (errorHandler.buildDepError "cardano-node"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [ "Paths_cardano_node" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "cardano-node.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "cardano-node-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-node" or (errorHandler.buildDepError "cardano-node"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-corpus" or (errorHandler.buildDepError "hedgehog-corpus"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          modules = [
            "Test/Cardano/Node/FilePermissions"
            "Test/Cardano/Node/Gen"
            "Test/Cardano/Node/Json"
            "Test/Cardano/Node/POM"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "cardano-node-test.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../cardano-node; }