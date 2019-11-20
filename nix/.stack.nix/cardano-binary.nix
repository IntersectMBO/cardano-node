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
      rev = "057d8a9a3e751f88fc7677df5713c5ecc47a37c4";
      sha256 = "1fr74v2p7d2r0rsvmb10zxl77v6vg5b3qbhwxd9pgflp4g7lwzkr";
      });
    postUnpack = "sourceRoot+=/binary; echo source root reset to \$sourceRoot";
    }