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
      identifier = {
        name = "cardano-ledger-shelley-test";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Test helpers from cardano-ledger-shelley exposed to other packages";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cddl-files/shelley.cddl"
        "cddl-files/real/crypto.cddl"
        "cddl-files/mock/extras.cddl"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Test/Cardano/Crypto/VRF/Fake"
          "Test/Cardano/Ledger/TerseTools"
          "Test/Cardano/Ledger/Shelley/Address/Bootstrap"
          "Test/Cardano/Ledger/Shelley/Address/CompactAddr"
          "Test/Cardano/Ledger/Shelley/BenchmarkFunctions"
          "Test/Cardano/Ledger/Shelley/ByronTranslation"
          "Test/Cardano/Ledger/Shelley/ConcreteCryptoTypes"
          "Test/Cardano/Ledger/Shelley/Examples/Cast"
          "Test/Cardano/Ledger/Shelley/Examples/Consensus"
          "Test/Cardano/Ledger/Shelley/Examples/Federation"
          "Test/Cardano/Ledger/Shelley/Generator/Block"
          "Test/Cardano/Ledger/Shelley/Generator/Constants"
          "Test/Cardano/Ledger/Shelley/Generator/Core"
          "Test/Cardano/Ledger/Shelley/Generator/Delegation"
          "Test/Cardano/Ledger/Shelley/Generator/Metadata"
          "Test/Cardano/Ledger/Shelley/Generator/Presets"
          "Test/Cardano/Ledger/Shelley/Generator/Trace/Chain"
          "Test/Cardano/Ledger/Shelley/Generator/Trace/DCert"
          "Test/Cardano/Ledger/Shelley/Generator/Trace/Ledger"
          "Test/Cardano/Ledger/Shelley/Generator/Update"
          "Test/Cardano/Ledger/Shelley/Generator/Utxo"
          "Test/Cardano/Ledger/Shelley/Generator/EraGen"
          "Test/Cardano/Ledger/Shelley/Generator/ScriptClass"
          "Test/Cardano/Ledger/Shelley/Generator/ShelleyEraGen"
          "Test/Cardano/Ledger/Shelley/LaxBlock"
          "Test/Cardano/Ledger/Shelley/Orphans"
          "Test/Cardano/Ledger/Shelley/PropertyTests"
          "Test/Cardano/Ledger/Shelley/Rules/Chain"
          "Test/Cardano/Ledger/Shelley/Rules/ClassifyTraces"
          "Test/Cardano/Ledger/Shelley/Rules/TestChain"
          "Test/Cardano/Ledger/Shelley/Rules/TestDeleg"
          "Test/Cardano/Ledger/Shelley/Rules/TestPool"
          "Test/Cardano/Ledger/Shelley/Rules/TestPoolreap"
          "Test/Cardano/Ledger/Shelley/Serialisation/CDDLUtils"
          "Test/Cardano/Ledger/Shelley/Serialisation/Generators"
          "Test/Cardano/Ledger/Shelley/Serialisation/EraIndepGenerators"
          "Test/Cardano/Ledger/Shelley/Serialisation/Generators/Bootstrap"
          "Test/Cardano/Ledger/Shelley/Serialisation/Generators/Genesis"
          "Test/Cardano/Ledger/Shelley/Serialisation/GoldenUtils"
          "Test/Cardano/Ledger/Shelley/ShelleyTranslation"
          "Test/Cardano/Ledger/Shelley/Shrinkers"
          "Test/Cardano/Ledger/Shelley/Utils"
          "Test/TestScenario"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cardano-ledger-shelley-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Test/Cardano/Ledger/Shelley/Examples"
            "Test/Cardano/Ledger/Shelley/Examples/Combinators"
            "Test/Cardano/Ledger/Shelley/Examples/EmptyBlock"
            "Test/Cardano/Ledger/Shelley/Examples/Init"
            "Test/Cardano/Ledger/Shelley/Examples/GenesisDelegation"
            "Test/Cardano/Ledger/Shelley/Examples/NetworkID"
            "Test/Cardano/Ledger/Shelley/Examples/Mir"
            "Test/Cardano/Ledger/Shelley/Examples/MirTransfer"
            "Test/Cardano/Ledger/Shelley/Examples/PoolLifetime"
            "Test/Cardano/Ledger/Shelley/Examples/PoolReReg"
            "Test/Cardano/Ledger/Shelley/Examples/TwoPools"
            "Test/Cardano/Ledger/Shelley/Examples/Updates"
            "Test/Cardano/Ledger/Shelley/Fees"
            "Test/Cardano/Ledger/Shelley/MultiSigExamples"
            "Test/Cardano/Ledger/Shelley/Pretty"
            "Test/Cardano/Ledger/Shelley/Rewards"
            "Test/Cardano/Ledger/Shelley/SafeHash"
            "Test/Cardano/Ledger/Shelley/Serialisation"
            "Test/Cardano/Ledger/Shelley/Serialisation/CDDL"
            "Test/Cardano/Ledger/Shelley/Serialisation/Golden/Address"
            "Test/Cardano/Ledger/Shelley/Serialisation/Golden/Encoding"
            "Test/Cardano/Ledger/Shelley/Serialisation/Golden/Genesis"
            "Test/Cardano/Ledger/Shelley/Serialisation/Tripping/CBOR"
            "Test/Cardano/Ledger/Shelley/Serialisation/Tripping/JSON"
            "Test/Cardano/Ledger/Shelley/RulesTests"
            "Test/Cardano/Ledger/Shelley/UnitTests"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      benchmarks = {
        "mainbench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Bench/Control/Iterate/SetAlgebra/Bimap"
            "BenchUTxOAggregate"
            "BenchValidation"
            "Cardano/Ledger/Shelley/Bench/Gen"
            "Cardano/Ledger/Shelley/Bench/Rewards"
            ];
          hsSourceDirs = [ "bench" ];
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
    postUnpack = "sourceRoot+=/eras/shelley/test-suite; echo source root reset to $sourceRoot";
    }