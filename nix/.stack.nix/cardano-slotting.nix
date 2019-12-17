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
      rev = "80deee31f8d9422b8e090e55b17e1a714153180b";
      sha256 = "0jzv074hc8kq0r0k47bw8hvziyi16mfyv5gcyngd1d0mbrc0j2k1";
      });
    postUnpack = "sourceRoot+=/slotting; echo source root reset to \$sourceRoot";
    }