{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "typed-protocols-cbor"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cborg)
          (hsPkgs.serialise)
          (hsPkgs.io-sim-classes)
          (hsPkgs.typed-protocols)
          ];
        };
      tests = {
        "test-typed-protocols-cbor" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.serialise)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.io-sim-classes)
            (hsPkgs.typed-protocols)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "74a6928ff518dafe4adcd83987b402dc13ba99a3";
      sha256 = "0jjxm4i9swyz2v3z1q2hm16zv64af9gi0b0wqaf66iiz1m2zrxbv";
      });
    postUnpack = "sourceRoot+=/typed-protocols-cbor; echo source root reset to \$sourceRoot";
    }