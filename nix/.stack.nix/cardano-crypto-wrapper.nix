{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-crypto-wrapper"; version = "1.3.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cryptographic primitives used in the Cardano project";
      description = "Cryptographic primitives used in the Cardano project";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.base64-bytestring)
          (hsPkgs.base64-bytestring-type)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cryptonite)
          (hsPkgs.cryptonite-openssl)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.text)
          ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-binary-test)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-prelude-test)
            (hsPkgs.cryptonite)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.memory)
            (hsPkgs.text)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "a773ba71eb9a6fb91dda2cdf6c41b2280f2acf7d";
      sha256 = "0gbh82r9zd7k61hbj96vmh7jd2lrj4xg0hz1058qs7yraf5pjif2";
      });
    postUnpack = "sourceRoot+=/crypto; echo source root reset to \$sourceRoot";
    }