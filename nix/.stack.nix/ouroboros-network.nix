{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { ipv6 = false; cddl = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
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
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.dns)
          (hsPkgs.fingertree)
          (hsPkgs.iproute)
          (hsPkgs.network)
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
            (hsPkgs.network-mux)
            (hsPkgs.ouroboros-network)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-cbor)
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
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.io-sim-classes)
            (hsPkgs.io-sim)
            (hsPkgs.network-mux)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.contra-tracer)
            (hsPkgs.array)
            (hsPkgs.async)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.dns)
            (hsPkgs.fingertree)
            (hsPkgs.hashable)
            (hsPkgs.iproute)
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
            (hsPkgs.typed-protocols-cbor)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "af06ad8d87336f416d1bf81a89f043e921ef68d4";
      sha256 = "0w788k5vmb414lkqgywj9sbkd00qjcc8xmvhh3jf1w9k1j1n8rcw";
      });
    postUnpack = "sourceRoot+=/ouroboros-network; echo source root reset to \$sourceRoot";
    }