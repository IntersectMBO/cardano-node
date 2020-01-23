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
      rev = "096646a3064c6edf2d1e74f9eb5f491ee396ea9d";
      sha256 = "0lfiffm5xc8g48dqlppa150kdiy9n7m1ssy1gs1kjjgz8qxi0isl";
      });
    postUnpack = "sourceRoot+=/plugins/scribe-systemd; echo source root reset to \$sourceRoot";
    }