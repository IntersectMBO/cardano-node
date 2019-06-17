{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "io-sim-classes"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Type classes for concurrency with STM, ST and timing";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.mtl)
          (hsPkgs.stm)
          (hsPkgs.time)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "c848f9cb6b964f70c5c5248c596a071d2ff57132";
      sha256 = "1fcg38nrv2mcra2xhkyzkqagc5w3in7ingv3984bnmh1j3z3bvpm";
      });
    postUnpack = "sourceRoot+=/io-sim-classes; echo source root reset to \$sourceRoot";
    }