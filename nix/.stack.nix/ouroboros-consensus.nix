{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-consensus"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
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
          (hsPkgs.network-mux)
          (hsPkgs.typed-protocols)
          (hsPkgs.typed-protocols-cbor)
          (hsPkgs.io-sim-classes)
          (hsPkgs.contra-tracer)
          (hsPkgs.bifunctors)
          (hsPkgs.bimap)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-class)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-slotting)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.deepseq)
          (hsPkgs.digest)
          (hsPkgs.directory)
          (hsPkgs.filelock)
          (hsPkgs.filepath)
          (hsPkgs.fingertree)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.network)
          (hsPkgs.psqueues)
          (hsPkgs.serialise)
          (hsPkgs.stm)
          (hsPkgs.streaming)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.vector)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) (hsPkgs.unix-bytestring) ]);
        };
      exes = {
        "byron-db-converter" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.directory)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.optparse-generic)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.path)
            (hsPkgs.path-io)
            (hsPkgs.resourcet)
            (hsPkgs.streaming)
            (hsPkgs.text)
            (hsPkgs.time)
            ];
          };
        "analyse-db" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.ouroboros-network)
            ];
          };
        };
      tests = {
        "test-consensus" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.base16-bytestring)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-crypto-test)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-prelude)
            (hsPkgs.typed-protocols)
            (hsPkgs.network-mux)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.io-sim-classes)
            (hsPkgs.io-sim)
            (hsPkgs.binary-search)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.cryptonite)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.fgl)
            (hsPkgs.filepath)
            (hsPkgs.fingertree)
            (hsPkgs.generics-sop)
            (hsPkgs.graphviz)
            (hsPkgs.hedgehog-quickcheck)
            (hsPkgs.mtl)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-state-machine)
            (hsPkgs.random)
            (hsPkgs.serialise)
            (hsPkgs.tasty)
            (hsPkgs.tasty-golden)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.tree-diff)
            ];
          };
        "test-storage" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-prelude)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.io-sim-classes)
            (hsPkgs.io-sim)
            (hsPkgs.base16-bytestring)
            (hsPkgs.bifunctors)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.cereal)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.fingertree)
            (hsPkgs.generics-sop)
            (hsPkgs.hashable)
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
            (hsPkgs.text)
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
      rev = "9bea6b22df7169afc4688fb328079c25ca9f8f79";
      sha256 = "1d3m4f386ccwjck8sx3bn7jv8v152f9p2xh26jadzh983m0irawy";
      });
    postUnpack = "sourceRoot+=/ouroboros-consensus; echo source root reset to \$sourceRoot";
    }