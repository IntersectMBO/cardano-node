{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "typed-protocols"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "A framework for strongly typed protocols";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.io-sim-classes)
          (hsPkgs.bytestring)
          (hsPkgs.contra-tracer)
          (hsPkgs.time)
          ];
        };
      tests = {
        "test-protocols" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.contra-tracer)
            (hsPkgs.io-sim-classes)
            (hsPkgs.io-sim)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.time)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "7ebe200d16cf18da8c586bdb64d2ca121ab9d37e";
      sha256 = "1a6zy2rb7m6mb5jd0295ddn3k2v858pv769fkdzs2cchg707k8bh";
      });
    postUnpack = "sourceRoot+=/typed-protocols; echo source root reset to \$sourceRoot";
    }