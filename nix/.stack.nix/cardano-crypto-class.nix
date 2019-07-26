{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-crypto-class"; version = "2.0.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Type classes abstracting over cryptography primitives for Cardano";
      description = "Type classes abstracting over cryptography primitives for Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cryptonite)
          (hsPkgs.memory)
          (hsPkgs.reflection)
          (hsPkgs.vector)
          ];
        };
      tests = {
        "test-crypto" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cborg)
            (hsPkgs.cryptonite)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "d015414ab8a139173162e0cbce719f834d8b6e6c";
      sha256 = "105x0ragbd6v307375jv5bip6khmhmr9iidq0998xh9c2sc70i7a";
      });
    postUnpack = "sourceRoot+=/cardano-crypto-class; echo source root reset to \$sourceRoot";
    }