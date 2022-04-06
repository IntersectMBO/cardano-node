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
    flags = { use-ghc-stub = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-tx-plugin"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "The Plutus Tx compiler and GHC plugin";
      description = "The Plutus Tx compiler and GHC plugin.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          ] ++ (if flags.use-ghc-stub
          then [
            (hsPkgs."plutus-ghc-stub" or (errorHandler.buildDepError "plutus-ghc-stub"))
            ]
          else [ (hsPkgs."ghc" or (errorHandler.buildDepError "ghc")) ]);
        buildable = true;
        modules = [
          "PlutusTx/Compiler/Binders"
          "PlutusTx/Compiler/Builtins"
          "PlutusTx/Compiler/Expr"
          "PlutusTx/Compiler/Kind"
          "PlutusTx/Compiler/Laziness"
          "PlutusTx/Compiler/Names"
          "PlutusTx/Compiler/Type"
          "PlutusTx/Compiler/Types"
          "PlutusTx/Compiler/Utils"
          "PlutusTx/PIRTypes"
          "PlutusTx/PLCTypes"
          "PlutusTx/Plugin"
          "PlutusTx/Compiler/Error"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "plutus-tx-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx".components.sublibs.plutus-tx-testlib or (errorHandler.buildDepError "plutus-tx:plutus-tx-testlib"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = if flags.use-ghc-stub then false else true;
          modules = [
            "Budget/Spec"
            "IsData/Spec"
            "Lift/Spec"
            "Optimization/Spec"
            "Plugin/Spec"
            "Plugin/Basic/Spec"
            "Plugin/Data/Spec"
            "Plugin/Errors/Spec"
            "Plugin/Functions/Spec"
            "Plugin/Laziness/Spec"
            "Plugin/NoTrace/Spec"
            "Plugin/Primitives/Spec"
            "Plugin/Profiling/Spec"
            "Plugin/Typeclasses/Spec"
            "Plugin/Typeclasses/Lib"
            "Plugin/Coverage/Spec"
            "Plugin/Strict/Spec"
            "Plugin/Lib"
            "StdLib/Spec"
            "TH/Spec"
            "TH/TestTH"
            "Lib"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        "size" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-tx".components.sublibs.plutus-tx-testlib or (errorHandler.buildDepError "plutus-tx:plutus-tx-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "test/size" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "11";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "11";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/plutus-tx-plugin; echo source root reset to $sourceRoot";
    }