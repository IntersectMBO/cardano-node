# Nix dependencies

The nix build uses the new flake format to manage dependencies. To add flake support to your native nix setup please see https://nixos.wiki/wiki/Flakes.

The cardano-node nix build depends primarily on [haskell.nix](https://github.com/input-output-hk/haskell.nix) and secondarily, for some utilities, on [iohk-nix](https://github.com/input-output-hk/iohk-nix/).

Both can be updated with:

```
nix flake update haskellNix
nix flake update iohkNix
```

# Building Cardano Node with Nix

See: https://github.com/input-output-hk/cardano-node-wiki/wiki/building-the-node-using-nix
