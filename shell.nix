{ commonLib ? import ./lib.nix {}
, withHoogle ? false
}:
let
  pkgs = commonLib.pkgs;
  default = import ./. { inherit withHoogle; };
  shell = default.shell;
  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      commonLib.niv
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
# TODO: switch back to default.nix shell when it works
in default.shell // { inherit devops; }
