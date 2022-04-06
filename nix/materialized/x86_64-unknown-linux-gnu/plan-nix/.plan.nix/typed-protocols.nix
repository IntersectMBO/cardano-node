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
      identifier = { name = "typed-protocols"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "A framework for strongly typed protocols";
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
          ];
        buildable = true;
        modules = [
          "Network/TypedProtocol"
          "Network/TypedProtocol/Core"
          "Network/TypedProtocol/Codec"
          "Network/TypedProtocol/Pipelined"
          "Network/TypedProtocol/Driver"
          "Network/TypedProtocol/Proofs"
          ];
        hsSourceDirs = [ "src" ];
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
    postUnpack = "sourceRoot+=/typed-protocols; echo source root reset to $sourceRoot";
    }