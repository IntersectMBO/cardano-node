{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-x509"; version = "3.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/x509/README.md";
      url = "";
      synopsis = "Tool-suite for generating x509 certificates specialized for RSA with SHA-256";
      description = "See README";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.asn1-encoding)
          (hsPkgs.asn1-types)
          (hsPkgs.base64-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default-class)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.hourglass)
          (hsPkgs.ip)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.x509)
          (hsPkgs.x509-store)
          (hsPkgs.x509-validation)
          (hsPkgs.yaml)
          ];
        };
      tests = {
        "cardano-sl-x509-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.QuickCheck)
            (hsPkgs.cardano-sl-x509)
            (hsPkgs.universum)
            (hsPkgs.hedgehog)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl-x509";
      rev = "e8bfc1294e088f90e5ae0b4aedbc82ee46ac5ee4";
      sha256 = "11qi871sb52vcbxibsjbc3s112pl26ljjvv13q53lvd1ydcgbc3n";
      });
    }