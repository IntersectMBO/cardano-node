{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { checktvarinvariant = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "io-sim-classes"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
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
      rev = "602856e08e0ebe9d63751185d39580d3b22989c1";
      sha256 = "1frmr2szqipfxnny8d1h18564dwssach5liy9i4vcg1dpjzfdph6";
      });
    postUnpack = "sourceRoot+=/io-sim-classes; echo source root reset to \$sourceRoot";
    }