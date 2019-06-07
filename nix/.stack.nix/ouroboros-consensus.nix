{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-consensus"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Consensus layer for the Ouroboros blockchain protocol";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.ouroboros-network)
          (hsPkgs.typed-protocols)
          (hsPkgs.io-sim-classes)
          (hsPkgs.contra-tracer)
          (hsPkgs.cardano-ledger-test)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bimap)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.fingertree)
          (hsPkgs.formatting)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.pipes)
          (hsPkgs.reflection)
          (hsPkgs.serialise)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.vector)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      exes = {
        "demo-playground" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.contra-tracer)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.io-sim-classes)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-prelude)
            (hsPkgs.aeson)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serialise)
            (hsPkgs.stm)
            (hsPkgs.string-conv)
            (hsPkgs.text)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        };
      tests = {
        "test-consensus" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.typed-protocols)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.io-sim-classes)
            (hsPkgs.io-sim)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.cryptonite)
            (hsPkgs.fgl)
            (hsPkgs.fingertree)
            (hsPkgs.graphviz)
            (hsPkgs.mtl)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.serialise)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.text)
            (hsPkgs.time)
            ];
          };
        "test-crypto" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.bytestring)
            (hsPkgs.QuickCheck)
            (hsPkgs.serialise)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.time)
            ];
          };
        "test-storage" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.io-sim-classes)
            (hsPkgs.io-sim)
            (hsPkgs.bifunctors)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cereal)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.fingertree)
            (hsPkgs.generics-sop)
            (hsPkgs.mtl)
            (hsPkgs.pretty-show)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-state-machine)
            (hsPkgs.random)
            (hsPkgs.serialise)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.temporary)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.tree-diff)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "1c291da70decc003b474b192e335e86e80c252b3";
      sha256 = "0s7nrq5drqb4rdl8kwdgi6niizy43ic4rgjik7fh0y7xwngryva2";
      });
    postUnpack = "sourceRoot+=/ouroboros-consensus; echo source root reset to \$sourceRoot";
    }