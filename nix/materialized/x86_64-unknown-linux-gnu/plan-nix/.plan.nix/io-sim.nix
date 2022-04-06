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
    flags = { asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "io-sim"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2020 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Duncan Coutts, Marcin Szamotulski, Alexander Vieth";
      homepage = "";
      url = "";
      synopsis = "A pure simulator for monadic concurrency with STM";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
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
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          ];
        buildable = true;
        modules = [
          "Control/Monad/IOSim/CommonTypes"
          "Control/Monad/IOSim/Internal"
          "Control/Monad/IOSim/InternalTypes"
          "Control/Monad/IOSim/STM"
          "Control/Monad/IOSimPOR/Internal"
          "Control/Monad/IOSimPOR/Types"
          "Control/Monad/IOSimPOR/QuickCheckUtils"
          "Control/Monad/IOSimPOR/Timeout"
          "Data/List/Trace"
          "Control/Monad/IOSim"
          "Control/Monad/IOSim/Types"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          modules = [ "Test/IOSim" "Test/STM" "Test/Control/Monad/IOSimPOR" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "10";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "10";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/io-sim; echo source root reset to $sourceRoot";
    }