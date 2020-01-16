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
      rev = "0fcb3a306e96ce36fca75d62792c55ab1de871ea";
      sha256 = "0fbcf3iim4fh8m647ilw02ybbxiqna3zpny0w7rlqjgd4skfqvjr";
      });
    postUnpack = "sourceRoot+=/slotting; echo source root reset to \$sourceRoot";
    }