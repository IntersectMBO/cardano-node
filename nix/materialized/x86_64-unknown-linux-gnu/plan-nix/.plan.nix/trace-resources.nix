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
      identifier = { name = "trace-resources"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "Juergen Nicklisch-Franken";
      homepage = "";
      url = "";
      synopsis = "Package for tracing resources for linux, mac and windows";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "CHANGELOG.md"
        "README.md"
        "include/os-support-darwin.h"
        "include/os-support-win.h"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        modules = (([
          "Cardano/Logging/Resources"
          "Cardano/Logging/Resources/Types"
          ] ++ (pkgs.lib).optional (system.isLinux) "Cardano/Logging/Resources/Linux") ++ (pkgs.lib).optional (system.isWindows) "Cardano/Logging/Resources/Windows") ++ (pkgs.lib).optional (system.isOsx) "Cardano/Logging/Resources/Darwin";
        cSources = (pkgs.lib).optional (system.isWindows) "cbits/os-support-win.c" ++ (pkgs.lib).optional (system.isOsx) "cbits/os-support-darwin.c";
        hsSourceDirs = [ "src" ];
        includeDirs = (pkgs.lib).optional (system.isWindows) "include/" ++ (pkgs.lib).optional (system.isOsx) "include/";
        };
      tests = {
        "trace-resources-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."trace-resources" or (errorHandler.buildDepError "trace-resources"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "trace-resources-test.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../trace-resources; }