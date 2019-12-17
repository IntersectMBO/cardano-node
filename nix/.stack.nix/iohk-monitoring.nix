{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { disable-observables = false; performance-test-queue = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "iohk-monitoring"; version = "0.1.10.1"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "";
      author = "Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "logging, benchmarking and monitoring framework";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.contra-tracer)
          (hsPkgs.aeson)
          (hsPkgs.array)
          (hsPkgs.async)
          (hsPkgs.async-timer)
          (hsPkgs.attoparsec)
          (hsPkgs.auto-update)
          (hsPkgs.base64-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.clock)
          (hsPkgs.containers)
          (hsPkgs.contravariant)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.katip)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.safe)
          (hsPkgs.safe-exceptions)
          (hsPkgs.scientific)
          (hsPkgs.stm)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.tracer-transformers)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.yaml)
          (hsPkgs.libyaml)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.contra-tracer)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.aeson)
            (hsPkgs.array)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.clock)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.mtl)
            (hsPkgs.process)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.semigroups)
            (hsPkgs.split)
            (hsPkgs.stm)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.time-units)
            (hsPkgs.tracer-transformers)
            (hsPkgs.transformers)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
            (hsPkgs.void)
            (hsPkgs.yaml)
            (hsPkgs.libyaml)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "54a25a80ff8f7e299b328c7a05e31d99e22c76be";
      sha256 = "07z971p1v7z5r3yip8966rzz4qnnvyc1dzv8afhj8naj3ynxfyby";
      });
    postUnpack = "sourceRoot+=/iohk-monitoring; echo source root reset to \$sourceRoot";
    }