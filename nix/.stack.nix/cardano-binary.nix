{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-binary"; version = "1.5.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Binary serialization for Cardano";
      description = "This package includes the binary serialization format for Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.digest)
          (hsPkgs.formatting)
          (hsPkgs.recursion-schemes)
          (hsPkgs.safe-exceptions)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-prelude-test)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.pretty-show)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.tagged)
            (hsPkgs.text)
            (hsPkgs.vector)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "eed4fc484366214831576eef0c7fe90d1d08c78b";
      sha256 = "0h9vbs2bsx6pvb300vl66znwpkqn169pmvsl4pv8mn0sz8iw4pk6";
      });
    postUnpack = "sourceRoot+=/binary; echo source root reset to \$sourceRoot";
    }