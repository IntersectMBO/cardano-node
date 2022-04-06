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
      identifier = { name = "tracer-transformers"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Neil Davies, Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "tracer transformers and examples showing their use";
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
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"));
        buildable = true;
        modules = [
          "Control/Tracer/Transformers"
          "Control/Tracer/Transformers/ObserveOutcome"
          "Control/Tracer/Transformers/WithThreadAndTime"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "tracer-transfomers-example1" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "example1" ];
          mainPath = [ "Main.hs" ];
          };
        "tracer-transfomers-example2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "example2" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "8";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "8";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/tracer-transformers; echo source root reset to $sourceRoot";
    }