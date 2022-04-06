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
      specVersion = "1.10";
      identifier = { name = "plutus-ghc-stub"; version = "8.6.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "glasgow-haskell-users@haskell.org";
      author = "The GHC Team";
      homepage = "http://www.haskell.org/ghc/";
      url = "";
      synopsis = "The GHC API";
      description = "Stub functionality for the Plutus plugin, for cross compilers that\ndon't have a GHC library installed, like GHCJS\nThis should contain all the types and functions that the Plutus\ncompiler uses.\nFor technical reasons (Cabal), we need to be able to compile the plutus-tx\ncompiler for the host platform, even if we are going to load the plugin\nfrom the build platform libraries.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
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
          (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        modules = [
          "StubTypes"
          "Plugins"
          "GhcPlugins"
          "FamInstEnv"
          "Panic"
          "LoadIface"
          "Finder"
          "OccName"
          "TcRnTypes"
          "CoreSyn"
          "Kind"
          "TysPrim"
          "PrimOp"
          "Class"
          "FV"
          "MkId"
          "PrelNames"
          "TcRnMonad"
          ];
        hsSourceDirs = [ "src" ];
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
    postUnpack = "sourceRoot+=/stubs/plutus-ghc-stub; echo source root reset to $sourceRoot";
    }