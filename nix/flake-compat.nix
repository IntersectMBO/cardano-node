{...}@args:
let
  src = args.src or ../.;
  lock = builtins.fromJSON (builtins.readFile (src + "/flake.lock"));
  flake-compate-input = lock.nodes.root.inputs.flake-compat;
  nixpkgs-input = lock.nodes.haskellNix.inputs.${builtins.elemAt lock.nodes.root.inputs.nixpkgs 1};
  fetchTree = builtins.fetchTree or (info:
    builtins.fetchTarball {
      url = "https://api.github.com/repos/${info.owner}/${info.repo}/tarball/${info.rev}";
      sha256 = info.narHash;
    });
  flake-compat = import (fetchTree lock.nodes.${flake-compate-input}.locked);
  pkgs = import (fetchTree lock.nodes.${nixpkgs-input}.locked) { };
in
flake-compat {
  inherit src pkgs;
  override-inputs = {
    customConfig = args;
  };
}
