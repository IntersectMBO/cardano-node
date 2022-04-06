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
      specVersion = "3.0";
      identifier = { name = "cardano-node-chairman"; version = "1.33.0"; };
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
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      exes = {
        "cardano-node-chairman" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-git-rev" or (errorHandler.buildDepError "cardano-git-rev"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-node" or (errorHandler.buildDepError "cardano-node"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          modules = [
            "Cardano/Chairman"
            "Cardano/Chairman/Commands"
            "Cardano/Chairman/Commands/Version"
            "Cardano/Chairman/Commands/Run"
            "Paths_cardano_node_chairman"
            ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "cardano-node-chairman.hs" ];
          };
        };
      tests = {
        "chairman-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-testnet" or (errorHandler.buildDepError "cardano-testnet"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cardano-node.components.exes.cardano-node or (pkgs.buildPackages.cardano-node or (errorHandler.buildToolDepError "cardano-node:cardano-node")))
            (hsPkgs.buildPackages.cardano-cli.components.exes.cardano-cli or (pkgs.buildPackages.cardano-cli or (errorHandler.buildToolDepError "cardano-cli:cardano-cli")))
            (hsPkgs.buildPackages.cardano-node-chairman.components.exes.cardano-node-chairman or (pkgs.buildPackages.cardano-node-chairman or (errorHandler.buildToolDepError "cardano-node-chairman:cardano-node-chairman")))
            ];
          buildable = true;
          modules = [
            "Spec/Chairman/Chairman"
            "Spec/Chairman/Byron"
            "Spec/Chairman/Cardano"
            "Spec/Chairman/Shelley"
            "Spec/Network"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../cardano-node-chairman; }