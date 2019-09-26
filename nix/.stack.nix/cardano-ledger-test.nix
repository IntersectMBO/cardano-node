{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-ledger-test"; version = "1.3.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-ledger exposed to other packages";
      description = "Test helpers from cardano-ledger exposed to other packages";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-binary-test)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-crypto-test)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-prelude-test)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.hedgehog)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.tasty)
          (hsPkgs.tasty-hedgehog)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "344aa563be42f6a2682f5b7ff1e4978e4d75359a";
      sha256 = "15zp3sanqjgcsldzwhkfqlakhsmfz5jk27x9nldhw5blr4hd53dx";
      });
    postUnpack = "sourceRoot+=/cardano-ledger/test; echo source root reset to \$sourceRoot";
    }