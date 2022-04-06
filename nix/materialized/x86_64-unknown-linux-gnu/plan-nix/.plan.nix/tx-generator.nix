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
      identifier = { name = "tx-generator"; version = "2.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The transaction generator for cardano node";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-cli" or (errorHandler.buildDepError "cardano-cli"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-node" or (errorHandler.buildDepError "cardano-node"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
          (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."generic-monoid" or (errorHandler.buildDepError "generic-monoid"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."ixset-typed" or (errorHandler.buildDepError "ixset-typed"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [
          "Paths_tx_generator"
          "Cardano/Benchmarking/Command"
          "Cardano/Benchmarking/Compiler"
          "Cardano/Benchmarking/FundSet"
          "Cardano/Benchmarking/GeneratorTx"
          "Cardano/Benchmarking/GeneratorTx/Error"
          "Cardano/Benchmarking/GeneratorTx/LocalProtocolDefinition"
          "Cardano/Benchmarking/GeneratorTx/Genesis"
          "Cardano/Benchmarking/GeneratorTx/NodeToNode"
          "Cardano/Benchmarking/GeneratorTx/Tx"
          "Cardano/Benchmarking/GeneratorTx/SizedMetadata"
          "Cardano/Benchmarking/GeneratorTx/Tx/Byron"
          "Cardano/Benchmarking/GeneratorTx/Submission"
          "Cardano/Benchmarking/GeneratorTx/SubmissionClient"
          "Cardano/Benchmarking/NixOptions"
          "Cardano/Benchmarking/OuroborosImports"
          "Cardano/Benchmarking/Script"
          "Cardano/Benchmarking/Script/Action"
          "Cardano/Benchmarking/Script/Aeson"
          "Cardano/Benchmarking/Script/Core"
          "Cardano/Benchmarking/Script/Env"
          "Cardano/Benchmarking/Script/Example"
          "Cardano/Benchmarking/Script/Setters"
          "Cardano/Benchmarking/Script/Store"
          "Cardano/Benchmarking/Script/Types"
          "Cardano/Benchmarking/Tracer"
          "Cardano/Benchmarking/Types"
          "Cardano/Benchmarking/Wallet"
          "Cardano/Benchmarking/ListBufferedSelector"
          "Cardano/Benchmarking/PlutusExample"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "tx-generator" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tx-generator" or (errorHandler.buildDepError "tx-generator"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "tx-generator.hs" ];
          };
        };
      tests = {
        "tx-generator-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tx-generator" or (errorHandler.buildDepError "tx-generator"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../bench/tx-generator; }