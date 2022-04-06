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
      identifier = { name = "typed-protocols-examples"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Examples and tests for the typed-protocols framework";
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
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          ];
        buildable = true;
        modules = [
          "Network/TypedProtocol/Channel"
          "Network/TypedProtocol/Driver/Simple"
          "Network/TypedProtocol/PingPong/Type"
          "Network/TypedProtocol/PingPong/Client"
          "Network/TypedProtocol/PingPong/Server"
          "Network/TypedProtocol/PingPong/Codec"
          "Network/TypedProtocol/PingPong/Codec/CBOR"
          "Network/TypedProtocol/PingPong/Examples"
          "Network/TypedProtocol/ReqResp/Type"
          "Network/TypedProtocol/ReqResp/Client"
          "Network/TypedProtocol/ReqResp/Server"
          "Network/TypedProtocol/ReqResp/Codec"
          "Network/TypedProtocol/ReqResp/Codec/CBOR"
          "Network/TypedProtocol/ReqResp/Examples"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          modules = [
            "Network/TypedProtocol/PingPong/Tests"
            "Network/TypedProtocol/ReqResp/Tests"
            ];
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
    postUnpack = "sourceRoot+=/typed-protocols-examples; echo source root reset to $sourceRoot";
    }