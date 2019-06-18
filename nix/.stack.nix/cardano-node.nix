{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-node"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
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
          (hsPkgs.cardano-ledger)
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
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-shell)
            (hsPkgs.contra-tracer)
            (hsPkgs.io-sim-classes)
            (hsPkgs.ouroboros-network)
            (hsPkgs.ouroboros-consensus)
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