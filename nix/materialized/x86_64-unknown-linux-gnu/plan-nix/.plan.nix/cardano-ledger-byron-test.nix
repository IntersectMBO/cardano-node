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
      identifier = { name = "cardano-ledger-byron-test"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-ledger exposed to other packages";
      description = "Test helpers from cardano-ledger exposed to other packages";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-binary-test" or (errorHandler.buildDepError "cardano-binary-test"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
          (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."generic-monoid" or (errorHandler.buildDepError "generic-monoid"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Test/Cardano/Chain/Block/CBOR"
          "Test/Cardano/Chain/Block/Gen"
          "Test/Cardano/Chain/Block/Model"
          "Test/Cardano/Chain/Block/Model/Examples"
          "Test/Cardano/Chain/Block/Validation"
          "Test/Cardano/Chain/Block/ValidationMode"
          "Test/Cardano/Chain/Byron/API"
          "Test/Cardano/Chain/Buildable"
          "Test/Cardano/Chain/Common/Address"
          "Test/Cardano/Chain/Common/CBOR"
          "Test/Cardano/Chain/Common/Compact"
          "Test/Cardano/Chain/Common/Example"
          "Test/Cardano/Chain/Common/Gen"
          "Test/Cardano/Chain/Common/Lovelace"
          "Test/Cardano/Chain/Config"
          "Test/Cardano/Chain/Delegation/CBOR"
          "Test/Cardano/Chain/Delegation/Certificate"
          "Test/Cardano/Chain/Delegation/Example"
          "Test/Cardano/Chain/Delegation/Gen"
          "Test/Cardano/Chain/Delegation/Model"
          "Test/Cardano/Chain/Elaboration/Block"
          "Test/Cardano/Chain/Elaboration/Delegation"
          "Test/Cardano/Chain/Elaboration/Keys"
          "Test/Cardano/Chain/Elaboration/Update"
          "Test/Cardano/Chain/Elaboration/UTxO"
          "Test/Cardano/Chain/Epoch/File"
          "Test/Cardano/Chain/Genesis/CBOR"
          "Test/Cardano/Chain/Genesis/Dummy"
          "Test/Cardano/Chain/Genesis/Example"
          "Test/Cardano/Chain/Genesis/Gen"
          "Test/Cardano/Chain/Genesis/Json"
          "Test/Cardano/Chain/MempoolPayload/CBOR"
          "Test/Cardano/Chain/MempoolPayload/Example"
          "Test/Cardano/Chain/MempoolPayload/Gen"
          "Test/Cardano/Chain/Ssc/CBOR"
          "Test/Cardano/Chain/Slotting/CBOR"
          "Test/Cardano/Chain/Slotting/Example"
          "Test/Cardano/Chain/Slotting/Gen"
          "Test/Cardano/Chain/Slotting/Properties"
          "Test/Cardano/Chain/UTxO/CBOR"
          "Test/Cardano/Chain/UTxO/Compact"
          "Test/Cardano/Chain/UTxO/Example"
          "Test/Cardano/Chain/UTxO/Gen"
          "Test/Cardano/Chain/UTxO/Model"
          "Test/Cardano/Chain/UTxO/ValidationMode"
          "Test/Cardano/Chain/Update/CBOR"
          "Test/Cardano/Chain/Update/Example"
          "Test/Cardano/Chain/Update/Gen"
          "Test/Cardano/Chain/Update/Properties"
          "Test/Cardano/Mirror"
          "Test/Options"
          ];
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
    postUnpack = "sourceRoot+=/eras/byron/ledger/impl/test; echo source root reset to $sourceRoot";
    }