{ ... }@args:

let
  commonLib = import ../lib.nix args;

in commonLib.nix-tools.default-nix ./pkgs.nix args
