let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix { };
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
  niv = (import sources.niv {}).niv;
in lib // iohkNix.cardanoLib // {
  inherit iohkNix pkgs;
  inherit (iohkNix) nix-tools;
  inherit niv;
}
