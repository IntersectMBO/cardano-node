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
    flags = { test-normal-form = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-ledger-byron"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The blockchain layer of Cardano during the Byron era";
      description = "The blockchain layer of Cardano during the Byron era";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."streaming-binary" or (errorHandler.buildDepError "streaming-binary"))
          (hsPkgs."streaming-bytestring" or (errorHandler.buildDepError "streaming-bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Cardano/Chain/Block/Block"
          "Cardano/Chain/Block/Body"
          "Cardano/Chain/Block/Boundary"
          "Cardano/Chain/Block/Header"
          "Cardano/Chain/Block/Proof"
          "Cardano/Chain/Block/Validation"
          "Cardano/Chain/Block/ValidationMode"
          "Cardano/Chain/Byron/API/Common"
          "Cardano/Chain/Byron/API/Mempool"
          "Cardano/Chain/Byron/API/Protocol"
          "Cardano/Chain/Byron/API/Validation"
          "Cardano/Chain/Common/AddrAttributes"
          "Cardano/Chain/Common/AddrSpendingData"
          "Cardano/Chain/Common/Address"
          "Cardano/Chain/Common/AddressHash"
          "Cardano/Chain/Common/Attributes"
          "Cardano/Chain/Common/BlockCount"
          "Cardano/Chain/Common/CBOR"
          "Cardano/Chain/Common/ChainDifficulty"
          "Cardano/Chain/Common/Compact"
          "Cardano/Chain/Common/KeyHash"
          "Cardano/Chain/Common/Lovelace"
          "Cardano/Chain/Common/LovelacePortion"
          "Cardano/Chain/Common/Merkle"
          "Cardano/Chain/Common/NetworkMagic"
          "Cardano/Chain/Common/TxFeePolicy"
          "Cardano/Chain/Common/TxSizeLinear"
          "Cardano/Chain/Delegation/Certificate"
          "Cardano/Chain/Delegation/Map"
          "Cardano/Chain/Delegation/Payload"
          "Cardano/Chain/Genesis/AvvmBalances"
          "Cardano/Chain/Genesis/Config"
          "Cardano/Chain/Genesis/Data"
          "Cardano/Chain/Genesis/Delegation"
          "Cardano/Chain/Genesis/Generate"
          "Cardano/Chain/Genesis/Hash"
          "Cardano/Chain/Genesis/Initializer"
          "Cardano/Chain/Genesis/KeyHashes"
          "Cardano/Chain/Genesis/NonAvvmBalances"
          "Cardano/Chain/Genesis/Spec"
          "Cardano/Chain/Slotting/EpochAndSlotCount"
          "Cardano/Chain/Slotting/EpochNumber"
          "Cardano/Chain/Slotting/EpochSlots"
          "Cardano/Chain/Slotting/SlotCount"
          "Cardano/Chain/Slotting/SlotNumber"
          "Cardano/Chain/UTxO/Compact"
          "Cardano/Chain/UTxO/GenesisUTxO"
          "Cardano/Chain/UTxO/Tx"
          "Cardano/Chain/UTxO/TxAux"
          "Cardano/Chain/UTxO/TxPayload"
          "Cardano/Chain/UTxO/UTxOConfiguration"
          "Cardano/Chain/UTxO/TxProof"
          "Cardano/Chain/UTxO/TxWitness"
          "Cardano/Chain/UTxO/ValidationMode"
          "Cardano/Chain/Update/ApplicationName"
          "Cardano/Chain/Update/InstallerHash"
          "Cardano/Chain/Update/Payload"
          "Cardano/Chain/Update/Proof"
          "Cardano/Chain/Update/ProtocolParameters"
          "Cardano/Chain/Update/ProtocolParametersUpdate"
          "Cardano/Chain/Update/ProtocolVersion"
          "Cardano/Chain/Update/SoftforkRule"
          "Cardano/Chain/Update/SoftwareVersion"
          "Cardano/Chain/Update/SystemTag"
          "Cardano/Chain/Update/Validation/Interface/ProtocolVersionBump"
          "Cardano/Chain/Block"
          "Cardano/Chain/Byron/API"
          "Cardano/Chain/Common"
          "Cardano/Chain/Constants"
          "Cardano/Chain/Delegation"
          "Cardano/Chain/Delegation/Validation/Activation"
          "Cardano/Chain/Delegation/Validation/Interface"
          "Cardano/Chain/Delegation/Validation/Scheduling"
          "Cardano/Chain/Epoch/File"
          "Cardano/Chain/Epoch/Validation"
          "Cardano/Chain/Genesis"
          "Cardano/Chain/MempoolPayload"
          "Cardano/Chain/ProtocolConstants"
          "Cardano/Chain/Slotting"
          "Cardano/Chain/Ssc"
          "Cardano/Chain/UTxO"
          "Cardano/Chain/UTxO/UTxO"
          "Cardano/Chain/UTxO/Validation"
          "Cardano/Chain/Update"
          "Cardano/Chain/Update/Proposal"
          "Cardano/Chain/Update/Validation/Endorsement"
          "Cardano/Chain/Update/Validation/Interface"
          "Cardano/Chain/Update/Validation/Registration"
          "Cardano/Chain/Update/Validation/Voting"
          "Cardano/Chain/Update/Vote"
          "Cardano/Chain/ValidationMode"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cardano-ledger-byron-test" = {
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
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
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
            "Test/Cardano/Chain/Block/Size"
            "Test/Cardano/Chain/Block/Validation"
            "Test/Cardano/Chain/Block/ValidationMode"
            "Test/Cardano/Chain/Byron/API"
            "Test/Cardano/Chain/Buildable"
            "Test/Cardano/Chain/Common/Address"
            "Test/Cardano/Chain/Common/Attributes"
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
          hsSourceDirs = [ "test" ];
          mainPath = [ "test.hs" ];
          };
        "epoch-validation-normal-form-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-ledger" or (errorHandler.buildDepError "cardano-ledger"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = if !flags.test-normal-form then false else true;
          modules = [
            "Test/Cardano/Chain/Block/Validation"
            "Test/Cardano/Chain/Config"
            "Test/Cardano/Mirror"
            "Test/Options"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "NormalFormTest.hs" ];
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
    postUnpack = "sourceRoot+=/eras/byron/ledger/impl; echo source root reset to $sourceRoot";
    }