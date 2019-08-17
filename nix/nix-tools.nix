{ ... }@args:

let
  commonLib = import ../lib.nix;

in commonLib.nix-tools.default-nix ./pkgs.nix args
