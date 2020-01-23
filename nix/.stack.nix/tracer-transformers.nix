{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "tracer-transformers"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Neil Davies, Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "tracer transformers and examples showing their use";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.contra-tracer)
          (hsPkgs.time)
          (hsPkgs.safe-exceptions)
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs.contravariant);
        };
      exes = {
        "tracer-transfomers-example1" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.contra-tracer)
            (hsPkgs.time)
            (hsPkgs.tracer-transformers)
            ];
          };
        "tracer-transfomers-example2" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.contra-tracer)
            (hsPkgs.text)
            (hsPkgs.tracer-transformers)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "9d37e5733b095a0c18acb419f0e934af533573d8";
      sha256 = "0givfrh0qkpxk8pwc4aikn9dx6k3z2ajivcsprfgbcja1z0f8y1n";
      });
    postUnpack = "sourceRoot+=/tracer-transformers; echo source root reset to \$sourceRoot";
    }