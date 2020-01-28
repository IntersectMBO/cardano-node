{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-slotting"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "IOHK";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Key slotting types for cardano libraries";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-binary)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.serialise)
          (hsPkgs.transformers)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "eed4fc484366214831576eef0c7fe90d1d08c78b";
      sha256 = "0h9vbs2bsx6pvb300vl66znwpkqn169pmvsl4pv8mn0sz8iw4pk6";
      });
    postUnpack = "sourceRoot+=/slotting; echo source root reset to \$sourceRoot";
    }