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
      rev = "54a25a80ff8f7e299b328c7a05e31d99e22c76be";
      sha256 = "07z971p1v7z5r3yip8966rzz4qnnvyc1dzv8afhj8naj3ynxfyby";
      });
    postUnpack = "sourceRoot+=/tracer-transformers; echo source root reset to \$sourceRoot";
    }