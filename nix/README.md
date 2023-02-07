# Nix dependencies

The nix build use the new flake format to manage dependencies. To add flake support to your native nix setup please see https://nixos.wiki/wiki/Flakes.

Cardano-node nix build depends primarily on [haskell.nix](https://github.com/input-output-hk/haskell.nix) and secondarily, for some utilities, on [iohk-nix](https://github.com/input-output-hk/iohk-nix/).

Both can be updated with:

```
nix flake lock --update-input haskellNix
nix flake lock --update-input iohkNix
```
