# Scripts Overview

The `scripts` directory consists of the following directories:
- [benchmarking](#benchmarking)
- [buildkite](#buildkite)
- [byron-shelley-allegra-mary](#byron-shelley-allegra-mary)
- [lite](#lite)
- [shelley-from-scratch](#shelley-from-scratch)
- [byron-to-alonzo](#byron-to-alonzo)
- [plutus](#plutus)

#### benchmarking
Contains all the scripts relevant to benchmarking `cardano-node`. See the benchmarking [README](benchmarking/README.md).

#### buildkite
Contains scripts relevant to IOHK's CI.

#### byron-shelley-allegra-mary
Contains a script that sets up a cluster beginning in the Byron era and can transition to the Shelley era. You can also start a cluster in the Shelley, Allegra or Mary era by supplying an argument to `mk-files.sh`.
E.g
```bash
./scripts/byron-to-mary/mk-files.sh shelley # Starts nodes in Shelley era
./scripts/byron-to-mary/mk-files.sh allegra # Starts nodes in Allegra era
./scripts/byron-to-mary/mk-files.sh mary    # Starts nodes in Mary era
```
#### lite
Contains scripts that can start various clusters and intended to be as simple as possible. Note that using the shelley only era testnet clusters breaks compatibility with some cli commands.

#### shelley-from-scratch
Contains a script that creates all the necessary keys etc to create a shelley cluster from scratch.

#### byron-to-alonzo
Contains a script that creates all the necessary keys and configuration files to create an alonzo cluster from scratch.
This script is similar to [byron-shelley-allegra-mary](#byron-shelley-allegra-mary) script: It can either be used to start cluster in Byron and then gradually transition to Alonzo, or can jumpstart straight into selected era, eg.:
```
./scripts/byron-to-alonzo/mkfiles.sh alonzo
```
will start the cluster in Alonzo era from epoch 0.

#### plutus

Contains scripts to test submission of transactions containing (simple) Plutus scripts. Obviously, only works against a test cluster running in Alonzo era.

Example:
```
$ scripts/plutus/example-txin-locking-plutus-script.sh guessinggame
```
will post several transactions validating against the simple guessing Game plutus contract.
