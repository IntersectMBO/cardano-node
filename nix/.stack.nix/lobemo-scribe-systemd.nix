{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "lobemo-scribe-systemd"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand";
      homepage = "https://github.com/input-output-hk/iohk-monitoring-framework";
      url = "";
      synopsis = "provides a backend for logging to systemd/journal";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs.base)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.katip)
          (hsPkgs.text)
          (hsPkgs.template-haskell)
          (hsPkgs.unordered-containers)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ])) ++ (pkgs.lib).optionals (system.isLinux) [
          (hsPkgs.hsyslog)
          (hsPkgs.libsystemd-journal)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "c24126944d78e3e015aff21ae78e81408256556f";
      sha256 = "0n8a0lzbb8hk6vlnaf96a0k1yxl8whybwbfl6ciz5pdqdbvpyrid";
      });
    postUnpack = "sourceRoot+=/plugins/scribe-systemd; echo source root reset to \$sourceRoot";
    }