{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { ipv6 = false; cddl = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-network"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "A networking layer for the Ouroboros blockchain protocol";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.typed-protocols)
          (hsPkgs.io-sim-classes)
          (hsPkgs.contra-tracer)
          (hsPkgs.array)
          (hsPkgs.async)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.fingertree)
          (hsPkgs.network)
          (hsPkgs.process)
          (hsPkgs.serialise)
          (hsPkgs.stm)
          (hsPkgs.time)
          (hsPkgs.hashable)
          (hsPkgs.text)
          ];
        };
      exes = {
        "demo-chain-sync" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.directory)
            (hsPkgs.ouroboros-network)
            (hsPkgs.typed-protocols)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.network)
            (hsPkgs.random)
            (hsPkgs.serialise)
            (hsPkgs.splitmix)
            (hsPkgs.stm)
            (hsPkgs.QuickCheck)
            ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.typed-protocols)
            (hsPkgs.io-sim-classes)
            (hsPkgs.io-sim)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.contra-tracer)
            (hsPkgs.array)
            (hsPkgs.async)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.fingertree)
            (hsPkgs.hashable)
            (hsPkgs.mtl)
            (hsPkgs.network)
            (hsPkgs.pipes)
            (hsPkgs.process)
            (hsPkgs.QuickCheck)
            (hsPkgs.splitmix)
            (hsPkgs.serialise)
            (hsPkgs.stm)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.tasty-hunit)
            (hsPkgs.text)
            (hsPkgs.time)
            ];
          };
        "cddl" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.fingertree)
            (hsPkgs.hashable)
            (hsPkgs.process-extras)
            (hsPkgs.serialise)
            (hsPkgs.text)
            (hsPkgs.io-sim-classes)
            (hsPkgs.typed-protocols)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "8f999f3a0e7765a673654486c79951a32e4a5c37";
      sha256 = "14yqhqzlf5yffks9az15v4ba5rdpq64dlbnz1n4dlpss0r10md5v";
      });
    postUnpack = "sourceRoot+=/ouroboros-network; echo source root reset to \$sourceRoot";
    }