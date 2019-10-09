{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-launcher"; version = "0.1.0.0"; };
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
          (hsPkgs.Cabal)
          (hsPkgs.cardano-prelude)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.process)
          (hsPkgs.QuickCheck)
          (hsPkgs.text)
          (hsPkgs.turtle)
          (hsPkgs.yaml)
          (hsPkgs.time-units)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs.Win32);
        };
      exes = {
        "cardano-launcher" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-launcher)
            (hsPkgs.cardano-sl-x509)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.safe-exceptions)
            ];
          };
        "mock-daedalus-frontend" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-prelude)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.safe-exceptions)
            ];
          };
        "mock-installer" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-prelude)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.safe-exceptions)
            ];
          };
        };
      tests = {
        "cardano-launcher-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-launcher)
            (hsPkgs.cardano-prelude)
            (hsPkgs.directory)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-state-machine)
            (hsPkgs.tree-diff)
            (hsPkgs.hspec)
            (hsPkgs.yaml)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
            (hsPkgs.temporary)
            (hsPkgs.filepath)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-shell";
      rev = "6d1e522d0268a855fdd31d8fbbec1a88fad11a15";
      sha256 = "031ay9h4imgcyj1fp48sb82cxjp2f5q68m4rjrk0rmdym8x38rsc";
      });
    postUnpack = "sourceRoot+=/cardano-launcher; echo source root reset to \$sourceRoot";
    }