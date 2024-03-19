{ lib, pkgs, ... }:

#
# Configuration for a workbench profile.
#
# The overall structure matches what we currently have in profile.json,
# but so far it only includes what is needed for the genesis creation.
#

with lib;

let format = pkgs.formats.json { }; in

{
  imports = [
    ./genesis.nix
  ];

  options = {
    # The following containts only what is required for genesis.

    composition = mkOption {
      type = types.submodule { freeformType = format.type; };
      default = { };
    };

    derived = mkOption {
      type = types.submodule { freeformType = format.type; };
      default = { };
    };

    node-specs = mkOption {
      type = types.submodule { freeformType = format.type; };
      default = { };
    };
  };
}
