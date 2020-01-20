{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-node"; version = "1.4.0"; };
      license = "Apache-2.0";
      copyright = "";
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
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-config)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-crypto-class)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-prelude-test)
          (hsPkgs.cardano-shell)
          (hsPkgs.contra-tracer)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.directory)
          (hsPkgs.file-embed)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.hostname)
          (hsPkgs.io-sim-classes)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.iproute)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.network)
          (hsPkgs.network-mux)
          (hsPkgs.optparse-applicative)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.ouroboros-network)
          (hsPkgs.process)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serialise)
          (hsPkgs.stm)
          (hsPkgs.string-conv)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.tracer-transformers)
          (hsPkgs.transformers)
          (hsPkgs.transformers-except)
          (hsPkgs.tracer-transformers)
          (hsPkgs.typed-protocols)
          (hsPkgs.typed-protocols-cbor)
          (hsPkgs.utf8-string)
          (hsPkgs.vector)
          (hsPkgs.yaml)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) (hsPkgs.brick) (hsPkgs.vty) ]);
        };
      exes = {
        "cardano-node" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-config)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-shell)
            (hsPkgs.optparse-applicative)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        "wallet-client" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-config)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-shell)
            (hsPkgs.contra-tracer)
            (hsPkgs.io-sim-classes)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.optparse-applicative)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.text)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-cbor)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        "cardano-cli" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-node)
            (hsPkgs.optparse-applicative)
            (hsPkgs.transformers-except)
            ];
          };
        "chairman" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-config)
            (hsPkgs.cardano-prelude)
            (hsPkgs.contra-tracer)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            (hsPkgs.io-sim-classes)
            (hsPkgs.network-mux)
            (hsPkgs.optparse-applicative)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.ouroboros-network)
            (hsPkgs.text)
            (hsPkgs.transformers-except)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-cbor)
            ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
          };
        };
      tests = {
        "cardano-node-test" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-prelude) ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-node; }