# cardano-cli


A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

The general synopsis is as follows:
 ```
   Usage: cardano-cli (Genesis related CMDs | Key related CMDs | Delegation related CMDs | Transaction related CMDs | Local node related CMDs)
```

See `../README.md` for full usage instructions.

## How to build

### Cabal

Use [Cabal - Version 3.0](https://www.haskell.org/cabal/) to build this project:

```
$ cd cardano-cli
$ cabal build
```
