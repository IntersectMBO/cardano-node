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
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-prelude"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A Prelude replacement for the Cardano project";
      description = "A Prelude replacement for the Cardano project";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "ChangeLog.md"
        "README.md"
        "cbits/hashset.h"
        "cbits/worklist.h"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."ghc-heap" or (errorHandler.buildDepError "ghc-heap"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Cardano/Prelude/Base"
          "Cardano/Prelude/Error"
          "Cardano/Prelude/Formatting"
          "Cardano/Prelude/GHC/Heap"
          "Cardano/Prelude/GHC/Heap/NormalForm"
          "Cardano/Prelude/GHC/Heap/Size"
          "Cardano/Prelude/GHC/Heap/Tree"
          "Cardano/Prelude/HeapWords"
          "Cardano/Prelude/Json/Canonical"
          "Cardano/Prelude/Json/Parse"
          "Cardano/Prelude/Orphans"
          "Cardano/Prelude/Strict"
          "Cardano/Prelude"
          "Data/Semigroup/Action"
          ];
        cSources = [
          "cbits/hashset.c"
          "cbits/worklist.c"
          "cbits/closure_size.c"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "6";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "6";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/cardano-prelude; echo source root reset to $sourceRoot";
    }