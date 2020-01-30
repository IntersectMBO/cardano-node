{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { asserts = false; ipv6 = false; cddl = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
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
          (hsPkgs.network-mux)
          (hsPkgs.typed-protocols)
          (hsPkgs.typed-protocols-cbor)
          (hsPkgs.io-sim-classes)
          (hsPkgs.contra-tracer)
          (hsPkgs.async)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-slotting)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.dns)
          (hsPkgs.mtl)
          (hsPkgs.fingertree)
          (hsPkgs.iproute)
          (hsPkgs.network)
          (hsPkgs.psqueues)
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
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.directory)
            (hsPkgs.network-mux)
            (hsPkgs.network)
            (hsPkgs.ouroboros-network)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.serialise)
            (hsPkgs.splitmix)
            (hsPkgs.stm)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.typed-protocols)
            ];
          };
        };
      tests = {
        "test-network" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.array)
            (hsPkgs.async)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-slotting)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.directory)
            (hsPkgs.dns)
            (hsPkgs.fingertree)
            (hsPkgs.hashable)
            (hsPkgs.io-sim)
            (hsPkgs.io-sim-classes)
            (hsPkgs.iproute)
            (hsPkgs.mtl)
            (hsPkgs.network-mux)
            (hsPkgs.network)
            (hsPkgs.psqueues)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.pipes)
            (hsPkgs.process)
            (hsPkgs.QuickCheck)
            (hsPkgs.serialise)
            (hsPkgs.splitmix)
            (hsPkgs.stm)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.tasty)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.typed-protocols)
            ];
          };
        "cddl" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-slotting)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.fingertree)
            (hsPkgs.hashable)
            (hsPkgs.io-sim)
            (hsPkgs.io-sim-classes)
            (hsPkgs.network-mux)
            (hsPkgs.pipes)
            (hsPkgs.process-extras)
            (hsPkgs.QuickCheck)
            (hsPkgs.serialise)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.text)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.typed-protocols)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "99c5747db01e2eeea25478424d65d90e785544bd";
      sha256 = "0vj5465lyl0zy6csg2j6338bzp07b4875f04s0417k5igq6bf4b5";
      });
    postUnpack = "sourceRoot+=/ouroboros-network; echo source root reset to \$sourceRoot";
    }