{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "cs-ledger"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-chain";
      url = "";
      synopsis = "Executable specification of Cardano ledger";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bimap)
          (hsPkgs.containers)
          (hsPkgs.filepath)
          (hsPkgs.file-embed)
          (hsPkgs.goblins)
          (hsPkgs.hashable)
          (hsPkgs.hedgehog)
          (hsPkgs.lens)
          (hsPkgs.template-haskell)
          (hsPkgs.Unique)
          (hsPkgs.cardano-binary)
          (hsPkgs.small-steps)
          ];
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.doctest)
            (hsPkgs.containers)
            (hsPkgs.hedgehog)
            (hsPkgs.lens)
            (hsPkgs.memory)
            (hsPkgs.text)
            (hsPkgs.small-steps)
            (hsPkgs.cs-ledger)
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover or (pkgs.buildPackages.doctest-discover))
            ];
          };
        "ledger-rules-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bimap)
            (hsPkgs.containers)
            (hsPkgs.lens)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.Unique)
            (hsPkgs.cs-ledger)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "a0607b2c60a196f78be36bd3dba74f3ae5054f29";
      sha256 = "1c2i15yrph7s4k8rg8hmjp916baxvq255d7gaqqy2b1kq3y9jizv";
      });
    postUnpack = "sourceRoot+=/byron/ledger/executable-spec; echo source root reset to \$sourceRoot";
    }