{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-config"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-prelude-test)
          (hsPkgs.cardano-shell)
          (hsPkgs.cborg)
          (hsPkgs.contra-tracer)
          (hsPkgs.generic-monoid)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.lobemo-backend-aggregation)
          (hsPkgs.lobemo-backend-editor)
          (hsPkgs.lobemo-backend-ekg)
          (hsPkgs.lobemo-backend-monitoring)
          (hsPkgs.lobemo-scribe-systemd)
          (hsPkgs.network)
          (hsPkgs.optparse-applicative)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.ouroboros-network)
          (hsPkgs.iproute)
          (hsPkgs.safe-exceptions)
          (hsPkgs.scientific)
          (hsPkgs.string-conv)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.transformers-except)
          (hsPkgs.yaml)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-config; }