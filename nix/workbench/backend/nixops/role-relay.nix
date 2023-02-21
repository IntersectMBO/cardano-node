##
## The "relay" role:
##
##  - exactly a usual node
##
pkgs: with pkgs; {...}: {
  imports = [
    cardano-ops.modules.base-service
  ];
}
