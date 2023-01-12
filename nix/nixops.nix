self: super:

let plugins = [ "nixops-aws"
                "nixops-libvirtd"
              ];
    sourcePaths = import ./sources.nix { pkgs = super; };
in
{
  nixops = (import (sourcePaths.nixops-core + "/release.nix") {
    nixpkgs = sourcePaths.nixpkgs-nixops;
    nixpkgsConfig.permittedInsecurePackages =
      [
        "libvirt-5.9.0"
      ];
    pluginsSources = sourcePaths;
    p = self.lib.attrVals plugins;
    withManual = false;
  }).build.${self.stdenv.system};
}
