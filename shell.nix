{ withHoogle ? true
, profiling ? false
, localLib ? import ./lib.nix { inherit profiling; }
}:
let
  pkgs = localLib.iohkNix.pkgs;
  default = import ./default.nix {};
  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      localLib.niv
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };
  haskell-nix-src = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
  haskell-nix = (import (haskell-nix-src + "/nixpkgs") (import haskell-nix-src)).haskell-nix;
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
    (haskell-nix.hackage-package {
      name = "eventlog2html";
      version = "0.6.0";
    }).components.exes.eventlog2html
    tmux
  ]);
} // { inherit devops; }
