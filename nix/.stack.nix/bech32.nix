{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { werror = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bech32"; version = "1.0.0"; };
      license = "Apache-2.0";
      copyright = "2017 Marko Bencun, 2019 IOHK";
      maintainer = "operations@iohk.io, erikd@mega-nerd.com";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/bech32";
      url = "";
      synopsis = "Implementation of the Bech32 cryptocurrency address format (BIP 0173).";
      description = "Implementation of the Bech32 cryptocurrency address format documented in the\nBIP (Bitcoin Improvement Proposal) 0173.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.extra)
          (hsPkgs.text)
          ];
        };
      tests = {
        "bech32-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bech32)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.extra)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
            (hsPkgs.text)
            (hsPkgs.vector)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/bech32";
      rev = "6e6b5f2ff8a61265ff0fa12b5c01614f9c747d92";
      sha256 = "0h40w417jgc3pn3a5f8bd8bd92rgy43g8ab6pdspkfv11il155ig";
      });
    }