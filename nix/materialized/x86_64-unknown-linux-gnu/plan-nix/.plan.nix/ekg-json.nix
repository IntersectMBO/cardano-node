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
      identifier = { name = "ekg-json"; version = "0.1.0.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Johan Tibell <johan.tibell@gmail.com>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>";
      author = "Johan Tibell";
      homepage = "https://github.com/tibbe/ekg-json";
      url = "";
      synopsis = "JSON encoding of ekg metrics";
      description = "Encodes ekg metrics as JSON, using the same encoding as used by the\nekg package, thus allowing ekg metrics to be served by other HTTP\nservers than the one used by the ekg package.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGES.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [ "System/Metrics/Json" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "1";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "1";
      rev = "minimal";
      sha256 = "";
      };
    }