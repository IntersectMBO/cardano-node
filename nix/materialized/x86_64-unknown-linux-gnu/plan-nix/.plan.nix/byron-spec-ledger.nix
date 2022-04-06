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
      specVersion = "2.2";
      identifier = { name = "byron-spec-ledger"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Executable specification of Cardano ledger";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" "src/goblin_genomes/*.genome" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."goblins" or (errorHandler.buildDepError "goblins"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          ];
        buildable = true;
        modules = [
          "Hedgehog/Gen/Double"
          "Byron/Spec/Ledger/Core"
          "Byron/Spec/Ledger/Core/Generators"
          "Byron/Spec/Ledger/Core/Omniscient"
          "Byron/Spec/Ledger/Delegation"
          "Byron/Spec/Ledger/Delegation/Test"
          "Byron/Spec/Ledger/GlobalParams"
          "Byron/Spec/Ledger/Update"
          "Byron/Spec/Ledger/Update/Generators"
          "Byron/Spec/Ledger/Update/Test"
          "Byron/Spec/Ledger/UTxO"
          "Byron/Spec/Ledger/UTxO/Generators"
          "Byron/Spec/Ledger/Util"
          "Byron/Spec/Ledger/STS/UTXO"
          "Byron/Spec/Ledger/STS/UTXOW"
          "Byron/Spec/Ledger/STS/UTXOWS"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "byron-spec-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            ];
          buildable = true;
          modules = [
            "Test/Byron/Spec/Ledger/Core/Generators/Properties"
            "Test/Byron/Spec/Ledger/Delegation/Examples"
            "Test/Byron/Spec/Ledger/Delegation/Properties"
            "Test/Byron/Spec/Ledger/AbstractSize/Properties"
            "Test/Byron/Spec/Ledger/Update/Examples"
            "Test/Byron/Spec/Ledger/Update/Properties"
            "Test/Byron/Spec/Ledger/Relation/Properties"
            "Test/Byron/Spec/Ledger/UTxO/Properties"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "5";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "5";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/eras/byron/ledger/executable-spec; echo source root reset to $sourceRoot";
    }