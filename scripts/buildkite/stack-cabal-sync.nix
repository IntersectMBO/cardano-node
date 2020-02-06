{ system ? builtins.currentSystem }:

let
  pkgs = import ../../nix { inherit system; };
  repo-tool-src = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-repo-tool";
    rev = "94087feff428ebc3f2c2ae8bbb2c958b22ac51dd";
    sha256 = "09gydz3ic1rjljpajbs6zgdr27a3gsp43c034724zks6karybav8";
  };
  cardano-repo-tool = import repo-tool-src { inherit system; };
in pkgs.runCommand "stack-cabal-sync-shell" {
  buildInputs = [ cardano-repo-tool.cardano-repo-tool ];
  shellHook = ''
    for EXE in cardano-repo-tool; do
      source <($EXE --bash-completion-script `type -p $EXE`)
    done
  '';
} ""
