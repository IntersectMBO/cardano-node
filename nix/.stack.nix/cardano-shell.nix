{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-shell"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/cardano-shell#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/cardano-shell#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.Cabal)
          (hsPkgs.cardano-prelude)
          (hsPkgs.concurrency)
          (hsPkgs.containers)
          (hsPkgs.contravariant)
          (hsPkgs.dhall)
          (hsPkgs.directory)
          (hsPkgs.formatting)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.optparse-applicative)
          (hsPkgs.process)
          (hsPkgs.QuickCheck)
          (hsPkgs.safe-exceptions)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.transformers)
          ];
        };
      exes = {
        "cardano-shell-exe" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-shell)
            (hsPkgs.cardano-prelude)
            (hsPkgs.optparse-applicative)
            (hsPkgs.safe-exceptions)
            (hsPkgs.stm)
            (hsPkgs.iohk-monitoring)
            ];
          };
        "node-ipc" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-shell)
            (hsPkgs.cardano-prelude)
            (hsPkgs.optparse-applicative)
            (hsPkgs.safe-exceptions)
            ];
          };
        "cardano-launcher" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-shell)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-sl-x509)
            (hsPkgs.async)
            (hsPkgs.process)
            (hsPkgs.turtle)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.safe-exceptions)
            (hsPkgs.optparse-applicative)
            ];
          };
        };
      tests = {
        "cardano-shell-test" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.cardano-shell)
            (hsPkgs.cardano-prelude)
            (hsPkgs.dhall)
            (hsPkgs.safe-exceptions)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-state-machine)
            (hsPkgs.tree-diff)
            (hsPkgs.pretty-show)
            (hsPkgs.hspec)
            (hsPkgs.hspec-contrib)
            (hsPkgs.concurrency)
            (hsPkgs.dejafu)
            (hsPkgs.hunit-dejafu)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-shell";
      rev = "92e6b42167d1399971df788b691423222722c2f6";
      sha256 = "059m0hd1hjf96g9l1qzh7imawvgwc813l0xggwlb5n47cab2dvhd";
      });
    }