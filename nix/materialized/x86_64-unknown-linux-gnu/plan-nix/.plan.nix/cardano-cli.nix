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
    flags = { unexpected_thunks = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "cardano-cli"; version = "1.33.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The Cardano command-line interface.";
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
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-git-rev" or (errorHandler.buildDepError "cardano-git-rev"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        modules = [
          "Paths_cardano_cli"
          "Cardano/CLI/Helpers"
          "Cardano/CLI/Parsers"
          "Cardano/CLI/Render"
          "Cardano/CLI/Run"
          "Cardano/CLI/Run/Friendly"
          "Cardano/CLI/Types"
          "Cardano/CLI/Environment"
          "Cardano/CLI/Byron/Commands"
          "Cardano/CLI/Byron/Parsers"
          "Cardano/CLI/Byron/Run"
          "Cardano/CLI/Byron/Delegation"
          "Cardano/CLI/Byron/Genesis"
          "Cardano/CLI/Byron/Key"
          "Cardano/CLI/Byron/Legacy"
          "Cardano/CLI/Byron/Tx"
          "Cardano/CLI/Byron/Query"
          "Cardano/CLI/Byron/UpdateProposal"
          "Cardano/CLI/Byron/Vote"
          "Cardano/CLI/Shelley/Commands"
          "Cardano/CLI/Shelley/Key"
          "Cardano/CLI/Shelley/Orphans"
          "Cardano/CLI/Shelley/Output"
          "Cardano/CLI/Shelley/Parsers"
          "Cardano/CLI/Shelley/Run"
          "Cardano/CLI/Shelley/Run/Address"
          "Cardano/CLI/Shelley/Run/Address/Info"
          "Cardano/CLI/Shelley/Run/Genesis"
          "Cardano/CLI/Shelley/Run/Governance"
          "Cardano/CLI/Shelley/Run/Key"
          "Cardano/CLI/Shelley/Run/Node"
          "Cardano/CLI/Shelley/Run/Pool"
          "Cardano/CLI/Shelley/Run/Query"
          "Cardano/CLI/Shelley/Run/StakeAddress"
          "Cardano/CLI/Shelley/Run/TextView"
          "Cardano/CLI/Shelley/Run/Transaction"
          "Cardano/CLI/Shelley/Script"
          "Cardano/CLI/TopHandler"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "cardano-cli" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-cli" or (errorHandler.buildDepError "cardano-cli"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
            (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [
            "cardano-cli.hs"
            ] ++ (pkgs.lib).optional (!system.isWindows) "";
          };
        };
      tests = {
        "cardano-cli-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-api".components.sublibs.gen or (errorHandler.buildDepError "cardano-api:gen"))
            (hsPkgs."cardano-cli" or (errorHandler.buildDepError "cardano-cli"))
            (hsPkgs."cardano-node" or (errorHandler.buildDepError "cardano-node"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          modules = [
            "Test/Config/Mainnet"
            "Test/Cli/FilePermissions"
            "Test/Cli/ITN"
            "Test/Cli/JSON"
            "Test/Cli/MultiAssetParsing"
            "Test/Cli/Pioneers/Exercise1"
            "Test/Cli/Pioneers/Exercise2"
            "Test/Cli/Pioneers/Exercise3"
            "Test/Cli/Pioneers/Exercise4"
            "Test/Cli/Pioneers/Exercise5"
            "Test/Cli/Pioneers/Exercise6"
            "Test/Cli/Shelley/Run/Query"
            "Test/OptParse"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "cardano-cli-test.hs" ];
          };
        "cardano-cli-golden" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-cli" or (errorHandler.buildDepError "cardano-cli"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cardano-cli.components.exes.cardano-cli or (pkgs.buildPackages.cardano-cli or (errorHandler.buildToolDepError "cardano-cli:cardano-cli")))
            ];
          buildable = true;
          modules = [
            "Test/Golden/Byron/SigningKeys"
            "Test/Golden/Byron/Tx"
            "Test/Golden/Byron/TxBody"
            "Test/Golden/Byron/UpdateProposal"
            "Test/Golden/Byron/Vote"
            "Test/Golden/Byron/Witness"
            "Test/Golden/Shelley"
            "Test/Golden/Shelley/Address/Build"
            "Test/Golden/Shelley/Address/Info"
            "Test/Golden/Shelley/Address/KeyGen"
            "Test/Golden/Shelley/Genesis/Create"
            "Test/Golden/Shelley/Genesis/InitialTxIn"
            "Test/Golden/Shelley/Genesis/KeyGenDelegate"
            "Test/Golden/Shelley/Genesis/KeyGenGenesis"
            "Test/Golden/Shelley/Genesis/KeyGenUtxo"
            "Test/Golden/Shelley/Genesis/KeyHash"
            "Test/Golden/Shelley/Key/ConvertCardanoAddressKey"
            "Test/Golden/Shelley/Metadata/StakePoolMetadata"
            "Test/Golden/Shelley/MultiSig/Address"
            "Test/Golden/Shelley/Node/IssueOpCert"
            "Test/Golden/Shelley/Node/KeyGen"
            "Test/Golden/Shelley/Node/KeyGenKes"
            "Test/Golden/Shelley/Node/KeyGenVrf"
            "Test/Golden/Shelley/StakeAddress/Build"
            "Test/Golden/Shelley/StakeAddress/DeregistrationCertificate"
            "Test/Golden/Shelley/StakeAddress/KeyGen"
            "Test/Golden/Shelley/StakeAddress/RegistrationCertificate"
            "Test/Golden/Shelley/StakePool/RegistrationCertificate"
            "Test/Golden/Shelley/TextEnvelope/Certificates/GenesisKeyDelegationCertificate"
            "Test/Golden/Shelley/TextEnvelope/Certificates/MIRCertificate"
            "Test/Golden/Shelley/TextEnvelope/Certificates/OperationalCertificate"
            "Test/Golden/Shelley/TextEnvelope/Certificates/StakeAddressCertificates"
            "Test/Golden/Shelley/TextEnvelope/Certificates/StakePoolCertificates"
            "Test/Golden/Shelley/TextEnvelope/Keys/ExtendedPaymentKeys"
            "Test/Golden/Shelley/TextEnvelope/Keys/GenesisDelegateKeys"
            "Test/Golden/Shelley/TextEnvelope/Keys/GenesisKeys"
            "Test/Golden/Shelley/TextEnvelope/Keys/GenesisUTxOKeys"
            "Test/Golden/Shelley/TextEnvelope/Keys/KESKeys"
            "Test/Golden/Shelley/TextEnvelope/Keys/PaymentKeys"
            "Test/Golden/Shelley/TextEnvelope/Keys/StakeKeys"
            "Test/Golden/Shelley/TextEnvelope/Keys/VRFKeys"
            "Test/Golden/Shelley/TextEnvelope/Tx/Tx"
            "Test/Golden/Shelley/TextEnvelope/Tx/TxBody"
            "Test/Golden/Shelley/TextEnvelope/Tx/Witness"
            "Test/Golden/Shelley/TextView/DecodeCbor"
            "Test/Golden/Shelley/Transaction/Assemble"
            "Test/Golden/Shelley/Transaction/Build"
            "Test/Golden/Shelley/Transaction/CalculateMinFee"
            "Test/Golden/Shelley/Transaction/CreateWitness"
            "Test/Golden/Shelley/Transaction/Sign"
            "Test/Golden/TxView"
            "Test/Golden/Version"
            "Test/OptParse"
            "Test/Utilities"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "cardano-cli-golden.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../cardano-cli; }