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
      identifier = { name = "cardano-crypto-wrapper"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cryptographic primitives used in the Cardano project";
      description = "Cryptographic primitives used in the Cardano project";
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
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."base64-bytestring-type" or (errorHandler.buildDepError "base64-bytestring-type"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Cardano/Crypto/Signing/Tag"
          "Cardano/Crypto/Signing/KeyGen"
          "Cardano/Crypto/Signing/VerificationKey"
          "Cardano/Crypto/Signing/SigningKey"
          "Cardano/Crypto/Signing/Signature"
          "Cardano/Crypto/Signing/Redeem/Compact"
          "Cardano/Crypto/Signing/Redeem/KeyGen"
          "Cardano/Crypto/Signing/Redeem/SigningKey"
          "Cardano/Crypto/Signing/Redeem/Signature"
          "Cardano/Crypto/Signing/Redeem/VerificationKey"
          "Cardano/Crypto/Signing/Safe/KeyGen"
          "Cardano/Crypto/Signing/Safe/PassPhrase"
          "Cardano/Crypto/Signing/Safe/SafeSigner"
          "Cardano/Crypto"
          "Cardano/Crypto/Hashing"
          "Cardano/Crypto/Orphans"
          "Cardano/Crypto/ProtocolMagic"
          "Cardano/Crypto/Random"
          "Cardano/Crypto/Signing"
          "Cardano/Crypto/Signing/Redeem"
          "Cardano/Crypto/Signing/Safe"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-binary-test" or (errorHandler.buildDepError "cardano-binary-test"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Test/Cardano/Crypto/CBOR"
            "Test/Cardano/Crypto/Dummy"
            "Test/Cardano/Crypto/Example"
            "Test/Cardano/Crypto/Gen"
            "Test/Cardano/Crypto/Hashing"
            "Test/Cardano/Crypto/Json"
            "Test/Cardano/Crypto/Keys"
            "Test/Cardano/Crypto/Limits"
            "Test/Cardano/Crypto/Orphans"
            "Test/Cardano/Crypto/Random"
            "Test/Cardano/Crypto/Signing/Redeem"
            "Test/Cardano/Crypto/Signing/Redeem/Compact"
            "Test/Cardano/Crypto/Signing/Safe"
            "Test/Cardano/Crypto/Signing/Signing"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "test.hs" ];
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
    postUnpack = "sourceRoot+=/eras/byron/crypto; echo source root reset to $sourceRoot";
    }