{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "ekg-prometheus-adapter"; version = "0.2.0.1"; };
      license = "MIT";
      copyright = "2016 Alfredo Di Napoli";
      maintainer = "alfredo.dinapoli@gmail.com";
      author = "Alfredo Di Napoli";
      homepage = "https://github.com/CodiePP/ekg-prometheus-adapter";
      url = "";
      synopsis = "Easily expose your EKG metrics to Prometheus";
      description = "Forked from original implementation by Alfredo Di Napoli on https://github.com/adinapoli/ekg-prometheus-adapter";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.prometheus)
          (hsPkgs.ekg-core)
          (hsPkgs.unordered-containers)
          (hsPkgs.containers)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.microlens-th)
          ];
        };
      tests = {
        "tests" = {
          depends = [ (hsPkgs.base) (hsPkgs.ekg-prometheus-adapter) ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/CodiePP/ekg-prometheus-adapter";
      rev = "1a258b6df7d9807d4c4ff3e99722223d31a2c320";
      sha256 = "0jzr1afb4vanhcc2gzlybzr0jnh66cap8kh00fkd4c22882jqkh8";
      });
    }