{...}@args:
let
  src = args.src or ../.;
  lock = builtins.fromJSON (builtins.readFile (src + "/flake.lock"));
  flake-compate-input = lock.nodes.root.inputs.flake-compat;
  nixpkgs-input = lock.nodes.haskellNix.inputs.${builtins.elemAt lock.nodes.root.inputs.nixpkgs 1};
  flake-compat = import (builtins.fetchTarball {
    url = "https://api.github.com/repos/input-output-hk/flake-compat/tarball/${lock.nodes.${flake-compate-input}.locked.rev}";
    sha256 = lock.nodes.${flake-compate-input}.locked.narHash;
  });
  pkgs = import
    (builtins.fetchTarball {
      url = "https://api.github.com/repos/NixOS/nixpkgs/tarball/${lock.nodes.${nixpkgs-input}.locked.rev}";
      sha256 = lock.nodes.${nixpkgs-input}.locked.narHash;
    })
    { };
in
flake-compat {
  inherit src pkgs;
  override-inputs = {
    customConfig = args;
  };
}
