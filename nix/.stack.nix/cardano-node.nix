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
          ];
        };
      exes = {
        "cardano-node" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            ];
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