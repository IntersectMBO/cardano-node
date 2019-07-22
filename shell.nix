{ withHoogle ? true
}:
let
  default = import ./default.nix {};
in
default.nix-tools._raw.shellFor {
  packages    = ps: with ps; [ cardano-node ];
  withHoogle  = withHoogle;
  buildInputs =
  (with default.nix-tools._raw; [
    cabal-install.components.exes.cabal
    ghcid.components.exes.ghcid
  ]) ++
  (with default.nix-tools._raw._config._module.args.pkgs; [
    tmux
  ]);
}
