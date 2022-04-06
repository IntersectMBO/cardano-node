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
      identifier = { name = "cardano-ledger-shelley"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Shelley Ledger Executable Model";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          ];
        buildable = true;
        modules = [
          "Cardano/Ledger/Chain"
          "Cardano/Ledger/Shelley"
          "Cardano/Ledger/Shelley/Constraints"
          "Cardano/Ledger/Shelley/Address/Bootstrap"
          "Cardano/Ledger/Shelley/API"
          "Cardano/Ledger/Shelley/API/ByronTranslation"
          "Cardano/Ledger/Shelley/API/Genesis"
          "Cardano/Ledger/Shelley/API/Validation"
          "Cardano/Ledger/Shelley/API/Wallet"
          "Cardano/Ledger/Shelley/API/Mempool"
          "Cardano/Ledger/Shelley/API/Types"
          "Cardano/Ledger/Shelley/BlockChain"
          "Cardano/Ledger/Shelley/CompactAddr"
          "Cardano/Ledger/Shelley/Delegation/Certificates"
          "Cardano/Ledger/Shelley/Delegation/PoolParams"
          "Cardano/Ledger/Shelley/EpochBoundary"
          "Cardano/Ledger/Shelley/Genesis"
          "Cardano/Ledger/Shelley/HardForks"
          "Cardano/Ledger/Shelley/LedgerState"
          "Cardano/Ledger/Shelley/Metadata"
          "Cardano/Ledger/Shelley/Orphans"
          "Cardano/Ledger/Shelley/PoolRank"
          "Cardano/Ledger/Shelley/PParams"
          "Cardano/Ledger/Shelley/Rewards"
          "Cardano/Ledger/Shelley/RewardProvenance"
          "Cardano/Ledger/Shelley/RewardUpdate"
          "Cardano/Ledger/Shelley/Scripts"
          "Cardano/Ledger/Shelley/SoftForks"
          "Cardano/Ledger/Shelley/StabilityWindow"
          "Cardano/Ledger/Shelley/Rules/Bbody"
          "Cardano/Ledger/Shelley/Rules/Deleg"
          "Cardano/Ledger/Shelley/Rules/Delegs"
          "Cardano/Ledger/Shelley/Rules/Delpl"
          "Cardano/Ledger/Shelley/Rules/Epoch"
          "Cardano/Ledger/Shelley/Rules/EraMapping"
          "Cardano/Ledger/Shelley/Rules/Ledger"
          "Cardano/Ledger/Shelley/Rules/Ledgers"
          "Cardano/Ledger/Shelley/Rules/Mir"
          "Cardano/Ledger/Shelley/Rules/NewEpoch"
          "Cardano/Ledger/Shelley/Rules/Newpp"
          "Cardano/Ledger/Shelley/Rules/Pool"
          "Cardano/Ledger/Shelley/Rules/PoolReap"
          "Cardano/Ledger/Shelley/Rules/Ppup"
          "Cardano/Ledger/Shelley/Rules/Rupd"
          "Cardano/Ledger/Shelley/Rules/Snap"
          "Cardano/Ledger/Shelley/Rules/Tick"
          "Cardano/Ledger/Shelley/Rules/Updn"
          "Cardano/Ledger/Shelley/Rules/Upec"
          "Cardano/Ledger/Shelley/Rules/Utxo"
          "Cardano/Ledger/Shelley/Rules/Utxow"
          "Cardano/Ledger/Shelley/Tx"
          "Cardano/Ledger/Shelley/TxBody"
          "Cardano/Ledger/Shelley/UTxO"
          ];
        hsSourceDirs = [ "src" ];
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
    postUnpack = "sourceRoot+=/eras/shelley/impl; echo source root reset to $sourceRoot";
    }