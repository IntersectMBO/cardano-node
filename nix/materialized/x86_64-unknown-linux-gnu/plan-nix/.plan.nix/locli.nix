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
      identifier = { name = "locli"; version = "1.29"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Cardano log analysis CLI.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "NOTICE" ];
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."attoparsec-iso8601" or (errorHandler.buildDepError "attoparsec-iso8601"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-git-rev" or (errorHandler.buildDepError "cardano-git-rev"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."gnuplot" or (errorHandler.buildDepError "gnuplot"))
          (hsPkgs."Histogram" or (errorHandler.buildDepError "Histogram"))
          (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
          (hsPkgs."optparse-generic" or (errorHandler.buildDepError "optparse-generic"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."trace-resources" or (errorHandler.buildDepError "trace-resources"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Paths_locli"
          "Data/Accum"
          "Data/Distribution"
          "Cardano/Analysis/TopHandler"
          "Cardano/Analysis/Run"
          "Cardano/Analysis/API"
          "Cardano/Analysis/BlockProp"
          "Cardano/Analysis/Chain"
          "Cardano/Analysis/ChainFilter"
          "Cardano/Analysis/Driver"
          "Cardano/Analysis/MachTimeline"
          "Cardano/Analysis/Version"
          "Cardano/Unlog/Commands"
          "Cardano/Unlog/LogObject"
          "Cardano/Unlog/Parsers"
          "Cardano/Unlog/Render"
          "Cardano/Unlog/Resources"
          "Cardano/Unlog/Run"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "locli" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
            (hsPkgs."locli" or (errorHandler.buildDepError "locli"))
            (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "locli.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../bench/locli; }