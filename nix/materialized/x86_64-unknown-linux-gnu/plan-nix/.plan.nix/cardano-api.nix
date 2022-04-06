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
      identifier = { name = "cardano-api"; version = "1.33.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The cardano api";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
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
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ];
        buildable = true;
        modules = [
          "Cardano/Api/Address"
          "Cardano/Api/Block"
          "Cardano/Api/Certificate"
          "Cardano/Api/Eras"
          "Cardano/Api/Error"
          "Cardano/Api/Fees"
          "Cardano/Api/GenesisParameters"
          "Cardano/Api/Hash"
          "Cardano/Api/HasTypeProxy"
          "Cardano/Api/IPC"
          "Cardano/Api/IPC/Monad"
          "Cardano/Api/Json"
          "Cardano/Api/Key"
          "Cardano/Api/KeysByron"
          "Cardano/Api/KeysPraos"
          "Cardano/Api/KeysShelley"
          "Cardano/Api/LedgerEvent"
          "Cardano/Api/LedgerState"
          "Cardano/Api/Modes"
          "Cardano/Api/NetworkId"
          "Cardano/Api/OperationalCertificate"
          "Cardano/Api/ProtocolParameters"
          "Cardano/Api/Query"
          "Cardano/Api/Script"
          "Cardano/Api/ScriptData"
          "Cardano/Api/SerialiseBech32"
          "Cardano/Api/SerialiseCBOR"
          "Cardano/Api/SerialiseJSON"
          "Cardano/Api/SerialiseLedgerCddl"
          "Cardano/Api/SerialiseRaw"
          "Cardano/Api/SerialiseTextEnvelope"
          "Cardano/Api/SerialiseUsing"
          "Cardano/Api/Shelley/Genesis"
          "Cardano/Api/SpecialByron"
          "Cardano/Api/StakePoolMetadata"
          "Cardano/Api/Tx"
          "Cardano/Api/TxBody"
          "Cardano/Api/InMode"
          "Cardano/Api/TxMetadata"
          "Cardano/Api/TxSubmit/ErrorRender"
          "Cardano/Api/TxSubmit/Types"
          "Cardano/Api/Utils"
          "Cardano/Api/Value"
          "Cardano/Api/ValueParser"
          "Cardano/Api"
          "Cardano/Api/Byron"
          "Cardano/Api/ChainSync/Client"
          "Cardano/Api/ChainSync/ClientPipelined"
          "Cardano/Api/Crypto/Ed25519Bip32"
          "Cardano/Api/Protocol/Types"
          "Cardano/Api/Shelley"
          "Cardano/Api/Orphans"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "gen" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Gen/Cardano/Api"
            "Gen/Cardano/Api/Metadata"
            "Gen/Cardano/Api/Typed"
            "Gen/Cardano/Crypto/Seed"
            "Gen/Hedgehog/Roundtrip/Bech32"
            "Gen/Hedgehog/Roundtrip/CBOR"
            ];
          hsSourceDirs = [ "gen" ];
          };
        };
      tests = {
        "cardano-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-api".components.sublibs.gen or (errorHandler.buildDepError "cardano-api:gen"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-th" or (errorHandler.buildDepError "tasty-th"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          modules = [
            "Test/Cardano/Api/Crypto"
            "Test/Cardano/Api/Genesis"
            "Test/Cardano/Api/Json"
            "Test/Cardano/Api/KeysByron"
            "Test/Cardano/Api/Ledger"
            "Test/Cardano/Api/Metadata"
            "Test/Cardano/Api/Typed/Address"
            "Test/Cardano/Api/Typed/Bech32"
            "Test/Cardano/Api/Typed/CBOR"
            "Test/Cardano/Api/Typed/Envelope"
            "Test/Cardano/Api/Typed/JSON"
            "Test/Cardano/Api/Typed/Ord"
            "Test/Cardano/Api/Typed/Orphans"
            "Test/Cardano/Api/Typed/RawBytes"
            "Test/Cardano/Api/Typed/Script"
            "Test/Cardano/Api/Typed/Value"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "cardano-api-test.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../cardano-api; }