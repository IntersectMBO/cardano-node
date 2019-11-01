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
      rev = "bea0e079fc32ed316ce352d17d14199a680e3f6c";
      sha256 = "1hcshwmfsm34l1j65xcrk5ab6qh93yihf7163n85wva5z71l1rgj";
      });
    postUnpack = "sourceRoot+=/plugins/scribe-systemd; echo source root reset to \$sourceRoot";
    }