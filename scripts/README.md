# Scripts Overview

The `scripts` directory consists of the following directories:
- [benchmarking](#benchmarking)
- [buildkite](#buildkite)
- [byron-to-shelley](#byron-to-shelley)
- [lite](#lite)
- [shelley-from-scratch](#shelley-from-scratch)
- [byron-to-alonzo](#byron-to-alonzo)
- [plutus](#plutus)

#### buildkite
Contains scripts relevant to IOHK's CI.

### Clusters

Generally clusters sub-folders contain a `mkfiles.sh` script which creates a top level
`examples` subdirectory with keys and configuration to start a cluster. The script prints
to stdout instructions for starting nodes, transitioning protocols and general use. There are
also some scripts with examples of various kinds of transactions.

#### byron-to-shelley

Contains a script that sets up a config files and keys needed to start a cluster. The cluster starts in the Byron era and can transition to the Shelley era.

You can also start a cluster in the Shelley, Allegra or Mary era by supplying an argument to `mk-files.sh`.
E.g
```bash
./scripts/byron-to-mary/mk-files.sh shelley # Starts nodes in Shelley era
./scripts/byron-to-mary/mk-files.sh allegra # Starts nodes in Allegra era
./scripts/byron-to-mary/mk-files.sh mary    # Starts nodes in Mary era
```

#### lite
Contains scripts that can start various clusters and intended to be as simple as possible.
This does not follow the pattern of using a `mkfiles.sh` script as described above.
Note that using the shelley only era testnet clusters breaks compatibility with some cli commands.

#### shelley-from-scratch
Contains a `mkfiles.sh` script that creates all the necessary keys etc to create a shelley cluster from scratch.

#### byron-to-alonzo

Contains a `mkfiles.sh` script that creates all the necessary keys and configuration files to create an alonzo cluster from scratch.

It can either be used to start cluster in Byron and then gradually transition to Alonzo, or can jumpstart straight into selected era, eg.:
```
./scripts/byron-to-alonzo/mkfiles.sh alonzo
```
will start the cluster in Alonzo era from epoch 0.

The folder also contains examples of using transactions, namely a very simple example
in `simple-tx.sh` and examples of minting and burning ada in `mint.sh` and `burn.sh`.

#### plutus

Contains scripts to test submission of transactions containing (simple) Plutus scripts. Obviously, only works against a test cluster running in Alonzo era. Note: this does not follow the
pattern of using a `mkfiles.sh` script as described above.

Example:
```
$ scripts/plutus/example-txin-locking-plutus-script.sh guessinggame
```
will post several transactions validating against the simple guessing Game plutus contract.
