{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "lobemo-backend-editor"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand";
      homepage = "https://github.com/input-output-hk/iohk-monitoring-framework";
      url = "";
      synopsis = "provides a backend implementation to interactively manage configuration";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.safe)
          (hsPkgs.safe-exceptions)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.threepenny-gui)
          (hsPkgs.unordered-containers)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "4956b32f039579a0e7e4fd10793f65b4c77d9044";
      sha256 = "03lyb2m4i6p7rpjqarnhsx21nx48fwk6rzsrx15k6274a4bv0pix";
      });
    postUnpack = "sourceRoot+=/plugins/backend-editor; echo source root reset to \$sourceRoot";
    }