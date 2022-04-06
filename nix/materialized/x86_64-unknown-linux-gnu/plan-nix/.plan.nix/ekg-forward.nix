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
      identifier = { name = "ekg-forward"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "Denis Shevchenko <denis.shevchenko@iohk.io>";
      author = "Denis Shevchenko";
      homepage = "https://github.com/input-output-hk/ekg-forward";
      url = "";
      synopsis = "See README for more info";
      description = "See README for more info";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" "CHANGELOG.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [
          "System/Metrics/Acceptor"
          "System/Metrics/Configuration"
          "System/Metrics/Forwarder"
          "System/Metrics/ReqResp"
          "System/Metrics/Network/Acceptor"
          "System/Metrics/Network/Forwarder"
          "System/Metrics/Store/Acceptor"
          "System/Metrics/Store/Forwarder"
          "System/Metrics/Protocol/Type"
          "System/Metrics/Protocol/Codec"
          "System/Metrics/Protocol/Acceptor"
          "System/Metrics/Protocol/Forwarder"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "demo-forwarder" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          hsSourceDirs = [ "demo" ];
          mainPath = [ "forwarder.hs" ];
          };
        "demo-acceptor" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          hsSourceDirs = [ "demo" ];
          mainPath = [ "acceptor.hs" ];
          };
        };
      tests = {
        "ekg-forward-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          modules = [ "Test/GetAllMetrics" "Test/GetMetrics" "Test/MkConfig" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "12";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "12";
      rev = "minimal";
      sha256 = "";
      };
    }