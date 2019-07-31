{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-node"; version = "3.0.1.87"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The cardano full node";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cborg)
          (hsPkgs.formatting)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-ledger-test)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-shell)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.ouroboros-network)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      exes = {
        "cardano-node" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-shell)
            (hsPkgs.contra-tracer)
            (hsPkgs.io-sim-classes)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.aeson)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.directory)
            (hsPkgs.file-embed)
            (hsPkgs.formatting)
            (hsPkgs.iproute)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.serialise)
            (hsPkgs.safe-exceptions)
            (hsPkgs.stm)
            (hsPkgs.string-conv)
            (hsPkgs.template-haskell)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.typed-protocols)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) (hsPkgs.brick) (hsPkgs.vty) ]);
          };
        "wallet-client" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-shell)
            (hsPkgs.contra-tracer)
            (hsPkgs.io-sim-classes)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.bytestring)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serialise)
            (hsPkgs.text)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        "genesis-tool" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            (hsPkgs.contra-tracer)
            (hsPkgs.filepath)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.pvss)
            (hsPkgs.aeson)
            (hsPkgs.aeson-pretty)
            (hsPkgs.async)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serialise)
            (hsPkgs.stm)
            (hsPkgs.string-conv)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.typed-protocols)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        "chairman" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.contra-tracer)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-shell)
            (hsPkgs.io-sim-classes)
            (hsPkgs.network-mux)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.ouroboros-network)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serialise)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        };
      tests = {
        "cardano-node-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././.; }