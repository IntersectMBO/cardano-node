{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; test-normal-form = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-ledger"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The blockchain layer of Cardano";
      description = "The blockchain layer of Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.base58-bytestring)
          (hsPkgs.base64-bytestring-type)
          (hsPkgs.bimap)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-shell)
          (hsPkgs.containers)
          (hsPkgs.concurrency)
          (hsPkgs.cryptonite)
          (hsPkgs.Cabal)
          (hsPkgs.deepseq)
          (hsPkgs.digest)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.megaparsec)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.resourcet)
          (hsPkgs.streaming)
          (hsPkgs.streaming-binary)
          (hsPkgs.streaming-bytestring)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          ];
        };
      tests = {
        "cardano-ledger-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.base16-bytestring)
            (hsPkgs.bimap)
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
            (hsPkgs.cs-blockchain)
            (hsPkgs.cs-ledger)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.generic-monoid)
            (hsPkgs.hedgehog)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.resourcet)
            (hsPkgs.small-steps)
            (hsPkgs.streaming)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.vector)
            ];
          };
        "epoch-validation-normal-form-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-crypto-test)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-prelude-test)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.optparse-applicative)
            (hsPkgs.resourcet)
            (hsPkgs.silently)
            (hsPkgs.streaming)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hedgehog)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "98d818fb3d4e74dd8d9003e14d97733114d8ddc9";
      sha256 = "0vvznrkp26fd1r2nhrhw90ccf1z09s72jh4l170540d9l40y9ysq";
      });
    postUnpack = "sourceRoot+=/cardano-ledger; echo source root reset to \$sourceRoot";
    }