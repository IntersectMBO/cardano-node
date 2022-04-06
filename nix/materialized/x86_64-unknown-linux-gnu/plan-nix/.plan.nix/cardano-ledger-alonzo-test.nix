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
      identifier = {
        name = "cardano-ledger-alonzo-test";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Tests for Cardano ledger introducing Plutus Core";
      description = "This package builds upon the Mary ledger with support for extended UTxO\nvia Plutus Core.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cddl-files/alonzo.cddl"
        "cddl-files/real/crypto.cddl"
        "cddl-files/mock/extras.cddl"
        "golden/*.cbor"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Test/Cardano/Ledger/Alonzo/EraMapping"
          "Test/Cardano/Ledger/Alonzo/Examples/Consensus"
          "Test/Cardano/Ledger/Alonzo/Serialisation/Generators"
          "Test/Cardano/Ledger/Alonzo/AlonzoEraGen"
          "Test/Cardano/Ledger/Alonzo/Scripts"
          "Test/Cardano/Ledger/Alonzo/PlutusScripts"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cardano-ledger-alonzo-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Test/Cardano/Ledger/Alonzo/Examples"
            "Test/Cardano/Ledger/Alonzo/Golden"
            "Test/Cardano/Ledger/Alonzo/PropertyTests"
            "Test/Cardano/Ledger/Alonzo/Serialisation/Canonical"
            "Test/Cardano/Ledger/Alonzo/Serialisation/CDDL"
            "Test/Cardano/Ledger/Alonzo/Serialisation/Tripping"
            "Test/Cardano/Ledger/Alonzo/Translation"
            "Test/Cardano/Ledger/Alonzo/Trials"
            "Test/Cardano/Ledger/Alonzo/TxInfo"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
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
    postUnpack = "sourceRoot+=/eras/alonzo/test-suite; echo source root reset to $sourceRoot";
    }