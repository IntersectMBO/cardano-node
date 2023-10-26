# Scripts Overview

The `scripts` directory consists of the following directories:
- [benchmarking](#benchmarking)
- [lite](#lite)
- [shelley-from-scratch](#shelley-from-scratch)
- [byron-to-alonzo](#byron-to-alonzo)
- [plutus](#plutus)

#### lite
Contains scripts that can start various clusters and intended to be as simple as possible. Note that using the shelley only era testnet clusters breaks compatibility with some cli commands.

#### shelley-from-scratch
Contains a script that creates all the necessary keys etc to create a shelley cluster from scratch.

#### byron-to-alonzo
Contains a script that creates all the necessary keys and configuration files to create an alonzo cluster from scratch.

It can either be used to start cluster in Byron and then gradually transition to Alonzo, or can jumpstart straight into selected era, eg.:

```bash
./scripts/byron-to-alonzo/mkfiles.sh alonzo
```

will start the cluster in Alonzo era from epoch 0.

#### plutus

Contains scripts to test submission of transactions containing (simple) Plutus scripts. Obviously, only works against a test cluster running in Alonzo era.

Example:

```bash
$ scripts/plutus/example-txin-locking-plutus-script.sh guessinggame
```
will post several transactions validating against the simple guessing Game plutus contract.
