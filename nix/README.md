# Nix dependencies

The nix build use the new flake format to manage dependencies. A flake-compatible nix command is provided from within `nix-shell`. To add flake support to your native nix setup please see https://nixos.wiki/wiki/Flakes.

Cardano-node nix build depends primarily on [haskell.nix](https://github.com/input-output-hk/haskell.nix) and secondarily, for some utilities, on [iohk-nix](https://github.com/input-output-hk/iohk-nix/).

Both can be updated from within a cardano-node `nix-shell` with:

```
nix flake update --update-input haskellNix
nix flake update --update-input iohkNix
```

Or from outside the `nix-shell` with the scripts:

```
./nix/update-haskellNix.sh
./nix/update-iohkNix.sh
```
