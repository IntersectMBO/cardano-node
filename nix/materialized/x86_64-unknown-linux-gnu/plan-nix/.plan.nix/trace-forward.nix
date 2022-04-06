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
      identifier = { name = "trace-forward"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The forwarding protocols library for cardano node.";
      description = "The library providing typed protocols for forwarding different\ninformation from the cardano node to an external application.";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          ];
        buildable = true;
        modules = [
          "Trace/Forward/Protocol/DataPoint/Acceptor"
          "Trace/Forward/Protocol/DataPoint/Codec"
          "Trace/Forward/Protocol/DataPoint/Forwarder"
          "Trace/Forward/Protocol/DataPoint/Type"
          "Trace/Forward/Protocol/TraceObject/Acceptor"
          "Trace/Forward/Protocol/TraceObject/Codec"
          "Trace/Forward/Protocol/TraceObject/Forwarder"
          "Trace/Forward/Protocol/TraceObject/Type"
          "Trace/Forward/Run/DataPoint/Acceptor"
          "Trace/Forward/Run/DataPoint/Forwarder"
          "Trace/Forward/Run/TraceObject/Acceptor"
          "Trace/Forward/Run/TraceObject/Forwarder"
          "Trace/Forward/Configuration/DataPoint"
          "Trace/Forward/Configuration/TraceObject"
          "Trace/Forward/Utils/DataPoint"
          "Trace/Forward/Utils/TraceObject"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
            (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Test/Trace/Forward/Protocol/TraceObject/Codec"
            "Test/Trace/Forward/Protocol/TraceObject/Direct"
            "Test/Trace/Forward/Protocol/TraceObject/Examples"
            "Test/Trace/Forward/Protocol/TraceObject/Item"
            "Test/Trace/Forward/Protocol/TraceObject/Tests"
            "Test/Trace/Forward/Protocol/DataPoint/Codec"
            "Test/Trace/Forward/Protocol/DataPoint/Direct"
            "Test/Trace/Forward/Protocol/DataPoint/Examples"
            "Test/Trace/Forward/Protocol/DataPoint/Item"
            "Test/Trace/Forward/Protocol/DataPoint/Tests"
            "Test/Trace/Forward/Protocol/Common"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../trace-forward; }