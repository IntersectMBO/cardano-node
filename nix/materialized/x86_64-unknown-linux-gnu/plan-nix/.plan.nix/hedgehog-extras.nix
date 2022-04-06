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
      identifier = { name = "hedgehog-extras"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Supplemental library for hedgehog";
      description = "Supplemental library for hedgehog.";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"));
        buildable = true;
        modules = [
          "Hedgehog/Extras"
          "Hedgehog/Extras/Aeson"
          "Hedgehog/Extras/Internal/Cli"
          "Hedgehog/Extras/Internal/Plan"
          "Hedgehog/Extras/Internal/Test/Integration"
          "Hedgehog/Extras/Stock"
          "Hedgehog/Extras/Stock/Aeson"
          "Hedgehog/Extras/Stock/CallStack"
          "Hedgehog/Extras/Stock/IO/File"
          "Hedgehog/Extras/Stock/IO/Network/NamedPipe"
          "Hedgehog/Extras/Stock/IO/Network/Socket"
          "Hedgehog/Extras/Stock/IO/Network/Sprocket"
          "Hedgehog/Extras/Stock/IO/Process"
          "Hedgehog/Extras/Stock/Monad"
          "Hedgehog/Extras/Stock/OS"
          "Hedgehog/Extras/Stock/String"
          "Hedgehog/Extras/Stock/Time"
          "Hedgehog/Extras/Test"
          "Hedgehog/Extras/Test/Base"
          "Hedgehog/Extras/Test/Concurrent"
          "Hedgehog/Extras/Test/File"
          "Hedgehog/Extras/Test/MonadAssertion"
          "Hedgehog/Extras/Test/Network"
          "Hedgehog/Extras/Test/Process"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "2";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "2";
      rev = "minimal";
      sha256 = "";
      };
    }