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
      identifier = { name = "contra-tracer"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Neil Davies, Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "A simple interface for logging, tracing or monitoring.";
      description = "";
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
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"));
        buildable = true;
        modules = [ "Control/Tracer" "Control/Tracer/Observe" ];
        hsSourceDirs = [ "src" ];
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
    postUnpack = "sourceRoot+=/contra-tracer; echo source root reset to $sourceRoot";
    }