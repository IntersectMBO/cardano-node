{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-shell"; version = "0.1.0.0"; };
      license = "Apache-2.0";
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
          (hsPkgs.async)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.generic-monoid)
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs.Win32);
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
        "daedalus-ipc" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-shell)
            (hsPkgs.cardano-prelude)
            (hsPkgs.optparse-applicative)
            (hsPkgs.safe-exceptions)
            (hsPkgs.iohk-monitoring)
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
            (hsPkgs.process)
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
          build-tools = [
            (hsPkgs.buildPackages.cardano-shell or (pkgs.buildPackages.cardano-shell))
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-shell";
      rev = "2044172cd8856e927860e9b5c731a4343462206e";
      sha256 = "1ij6cpj7lg26ka90dg5q5n0976kqy2np912zdk4w4hwl9vqy4z4j";
      });
    }