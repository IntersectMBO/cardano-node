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
      specVersion = "2.4";
      identifier = { name = "cardano-ledger-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Core components of Cardano ledgers from the Shelley release on.";
      description = "Cardano ledgers from the Shelley release onwards share a core basis rooted in\nthe Shelley ledger specification. This package abstracts a number of components\nwhich we expect to be shared amongst all future ledgers implemented around this base.";
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
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."compact-map" or (errorHandler.buildDepError "compact-map"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."partial-order" or (errorHandler.buildDepError "partial-order"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          ];
        buildable = true;
        modules = [
          "Cardano/Ledger/Address"
          "Cardano/Ledger/CompactAddress"
          "Cardano/Ledger/AuxiliaryData"
          "Cardano/Ledger/BaseTypes"
          "Cardano/Ledger/BHeaderView"
          "Cardano/Ledger/Block"
          "Cardano/Ledger/Coin"
          "Cardano/Ledger/Compactible"
          "Cardano/Ledger/Core"
          "Cardano/Ledger/Credential"
          "Cardano/Ledger/Crypto"
          "Cardano/Ledger/Era"
          "Cardano/Ledger/Hashes"
          "Cardano/Ledger/Keys"
          "Cardano/Ledger/PoolDistr"
          "Cardano/Ledger/Rules/ValidationMode"
          "Cardano/Ledger/SafeHash"
          "Cardano/Ledger/Serialization"
          "Cardano/Ledger/Slot"
          "Cardano/Ledger/TxIn"
          "Cardano/Ledger/UnifiedMap"
          "Cardano/Ledger/Val"
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
    postUnpack = "sourceRoot+=/libs/cardano-ledger-core; echo source root reset to $sourceRoot";
    }