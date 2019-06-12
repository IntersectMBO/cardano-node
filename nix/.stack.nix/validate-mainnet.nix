{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "validate-mainnet"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Run validation code over blocks from mainnet";
      buildType = "Simple";
      };
    components = {
      exes = {
        "validate-mainnet" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-ledger-test)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-shell)
            (hsPkgs.formatting)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.text)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././validate-mainnet; }