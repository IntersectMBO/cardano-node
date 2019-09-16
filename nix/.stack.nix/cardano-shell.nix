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
          (hsPkgs.directory)
          (hsPkgs.formatting)
          (hsPkgs.process)
          (hsPkgs.QuickCheck)
          (hsPkgs.safe-exceptions)
          (hsPkgs.async)
          (hsPkgs.text)
          (hsPkgs.transformers)
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs.Win32);
        };
      exes = {
        "node-ipc" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-shell)
            (hsPkgs.cardano-prelude)
            ];
          };
        "daedalus-ipc" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-shell)
            (hsPkgs.cardano-prelude)
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
            (hsPkgs.process)
            (hsPkgs.yaml)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-state-machine)
            (hsPkgs.tree-diff)
            (hsPkgs.hspec)
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
      rev = "18906dd8c109380c4b1fa20ed35100e48135aac3";
      sha256 = "1w2szfbkachpna80lark1qf6h0rcsnrdq5958h5m4bvylhn6n6fq";
      });
    }