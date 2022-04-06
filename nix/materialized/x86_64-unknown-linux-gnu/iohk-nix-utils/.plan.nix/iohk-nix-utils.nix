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
      identifier = { name = "iohk-nix-utils"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "devops@iohk.io";
      author = "IOHK Devops";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
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
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."turtle" or (errorHandler.buildDepError "turtle"))
          ];
        buildable = true;
        modules = [ "Build" "BuildArgs" "CommonBuild" ];
        hsSourceDirs = [ "lib" ];
        };
      exes = {
        "set-git-rev" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            ];
          buildable = true;
          mainPath = [
            "set-git-rev.hs"
            ] ++ (pkgs.lib).optional (system.isOsx) "";
          };
        "rewrite-libs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."turtle" or (errorHandler.buildDepError "turtle"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          buildable = true;
          mainPath = [ "rewrite-libs.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }