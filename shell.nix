{ withHoogle ? true
, localLib ? import ./lib.nix
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
    screen
    tmux
  ]);
} // { inherit devops; }
