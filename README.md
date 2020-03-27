<p align="center">
  <big><strong>Cardano Node</strong></big>
</p>

![alt text](https://github.com/input-output-hk/cardano-node/blob/media/Cardano-ADA.png?raw=true)

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-node/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-node.svg?style=for-the-badge" /></a>
  <a href="https://buildkite.com/input-output-hk/cardano-node"><img src="https://img.shields.io/buildkite/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a/master?label=BUILD&style=for-the-badge"/></a>
</p>

# cardano-node

Integration of the [ledger](https://github.com/input-output-hk/cardano-ledger),
[consensus](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus),
[networking](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network)
and [node shell](https://github.com/input-output-hk/cardano-shell)
repositories.
[Logging](https://github.com/input-output-hk/iohk-monitoring-framework) is
provided as
a [feature](https://github.com/input-output-hk/cardano-shell/blob/master/app/Cardano/Shell/Features/Logging.hs)
by the node shell to the other packages.

## How to build

### Stack
Use [Haskell Stack - Version 2.1.3](https://haskellstack.org/) to build this project:

```
$ cd cardano-node
$ stack build
```

## Cabal

Use [Cabal - Version 3.0](https://www.haskell.org/cabal/) to build this project:

```
$ cd cardano-node
$ cabal build
```

## Windows Executable

### Download
You can download [here](https://hydra.iohk.io/job/Cardano/cardano-node/cardano-node-win64/latest-finished).

### Instructions

The download includes cardano-node.exe and a .dll. To run the node with cardano-node run you need to reference a few files and directories as arguments. These can be copied from the cardano-node repo into the executables directory. The command to run the node on mainnet looks like this:

```
cardano-node.exe run --topology ./mainnet-topology.json --database-path ./state --genesis-file ./mainnet-genesis.json --genesis-hash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb --port 3001 --config ./configuration-mainnet.yaml --socket-path ./socket/mainnet-socket
```

# `cardano-node`

This refers to the client that is used for running a node.

The general synopsis is as follows:
```

Usage: cardano-node --topology FILEPATH --database-path FILEPATH
                    --genesis-file FILEPATH [--delegation-certificate FILEPATH]
                    [--signing-key FILEPATH] --socket-path FILEPATH
                    [--host-addr HOST-NAME] --port PORT
                    --config NODE-CONFIGURATION [--help] [--help-tracing]
                    [--help-advanced]
  Start node of the Cardano blockchain.
```
`--topology` - Filepath to a topology file describing which peers the node should connect to.

`--database-path` - Path to the blockchain database.

`--genesis-file` - Path to the genesis file of the chain you are connecting to.

`--delegation-certificate` - Optional path to the delegation certificate. The delegation certificate allows the delegator (the issuer of said certificate) to give his/her own block signing rights to somebody else (the delegatee). The delegatee can then sign blocks on behalf of the delegator.

`--signing-key` - Optional path to the signing key.

`--socket-path` - Path to the socket file.

`--host-addr` - Optionally specify your node's IPv4 or IPv6 address.

`--port` - Specify which port to assign to the node.

`--config` - Specify the filepath to the config `.yaml` file. This file is responsible for all the other node's required settings. See examples in `configuration` (e.g. `log-config-0.yaml`).


## Configuration `.yaml` files

The `--config` flag points to a `.yaml` file that is responsible to configuring the logging & other important settings for the node.
Some of the more important settings are as follows:

`NodeId: 0`  -- Used in mock protocols only to differentiate nodes.

`Protocol: RealPBFT` -- Protocol the node will execute

`RequiresNetworkMagic`: RequiresNoMagic -- Used to distinguish between mainnet (`RequiresNoMagic`) and testnets (`RequiresMagic`)

`ViewMode: SimpleView` -- Choose between SimpleView or LiveView


## Logging

Logs are output to the `log/` dir.


## Scripts

Please see `scripts/README.md` for information on the various scripts.

# `cardano-cli`

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

The general synopsis is as follows:
 ```
   Usage: cardano-cli (Genesis related CMDs | Key related CMDs | Delegation related CMDs | Transaction related CMDs | Local node related CMDs)
```

NOTE: the exact invocation command depends on the environment.  If you have only
built `cardano-cli`, without installing it, then you have to prepend `cabal
run -- ` before `cardano-cli`.  We henceforth assume that the necessary
environment-specific adjustment has been made, so we only mention `cardano-cli`.

The subcommands are subdivided in groups, and their full list can be seen in the
output of `cardano-cli --help`.

All subcommands have help available:

```
$ cabal v2-run -- cardano-cli migrate-delegate-key-from --help
Usage: cardano-cli migrate-delegate-key-from (--byron-legacy | --bft | --praos |
                                               --mock-pbft | --real-pbft)
                                             --from FILEPATH
                                             (--byron-legacy | --bft | --praos |
                                               --mock-pbft | --real-pbft)
                                             --to FILEPATH
  Migrate a delegate key from an older version.

Available options:
  --byron-legacy           Byron/Ouroboros Classic suite of algorithms
  --bft                    BFT consensus
  --praos                  Praos consensus
  --mock-pbft              Permissive BFT consensus with a mock ledger
  --real-pbft              Permissive BFT consensus with a real ledger
  --from FILEPATH          Signing key file to migrate.
  --byron-legacy           Byron/Ouroboros Classic suite of algorithms
  --bft                    BFT consensus
  --praos                  Praos consensus
  --mock-pbft              Permissive BFT consensus with a mock ledger
  --real-pbft              Permissive BFT consensus with a real ledger
  --to FILEPATH            Non-existent file to write the signing key to.
  -h,--help                Show this help text
```

## Genesis operations

### Generation

The genesis generation operations will create a directory that contains:

  `genesis.json`
  :: The genesis JSON file itself.

  `avvm-seed.*.seed`
  :: Ada Voucher Vending Machine seeds (secret). Affected by `--avvm-entry-count` and `--avvm-entry-balance`.

  `delegate-keys.*.key`
  :: Delegate private keys. Affected by: `--n-delegate-addresses`.

  `delegation-cert.*.json`
  :: Delegation certificates. Affected by: `--n-delegate-addresses`.

  `genesis-keys.*.key`
  :: Genesis stake private keys. Affected by: `--n-delegate-addresses`, `--total-balance`.

  `poor-keys.*.key`
  :: Non-delegate private keys with genesis UTxO. Affected by: `--n-poor-addresses`, `--total-balance`.

More details on the Genesis `JSON` file can be found in `docs/GenesisData.md`

Genesis delegation and related concepts are described in detail in:

  https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec

The canned `scripts/genesis.sh` example provides a nice set of defaults and
illustrates available options.  Running it will produce a `./configuration/XXXXX` directory,
where `XXXXX` will be a 5-character prefix of the genesis hash.

## Key operations

Note that no key operation currently supports accepting password-protected keys.
The `keygen` subcommand, though, can generate such keys.

### Signing key generation & verification key extraction

Signing keys can be generated using the `keygen` subcommand, password protection being
controlled by the `--no-password` flag.

Extracting a verification key out of the signing key is performed by the `to-verification` subcommand.

### Delegate key migration

In order to continue using a delegate key from the Byron Legacy era in the new implementation,
it needs to be migrated over, which is done by the `migrate-delegate-key-from` subcommand:


```
$ cabal v2-run -- cardano-cli migrate-delegate-key-from
--byron-legacy --from key0.sk  --real-pbft --to key0.pbft
```

### Signing key queries

One can gather information about a signing key's properties through the `signing-key-public`
and `signing-key-address` subcommands (the latter requires the network magic):

```
$ cabal v2-run -- cardano-cli signing-key-public --real-pbft --secret key0.pbft

public key hash: a2b1af0df8ca764876a45608fae36cf04400ed9f413de2e37d92ce04
     public key: sc4pa1pAriXO7IzMpByKo4cG90HCFD465Iad284uDYz06dHCqBwMHRukReQ90+TA/vQpj4L1YNaLHI7DS0Z2Vg==

$ cabal v2-run -- cardano-cli signing-key-address --real-pbft --secret key0.pbft --testnet-magic 459045235

2cWKMJemoBakxhXgZSsMteLP9TUvz7owHyEYbUDwKRLsw2UGDrG93gPqmpv1D9ohWNddx
VerKey address with root e5a3807d99a1807c3f161a1558bcbc45de8392e049682df01809c488, attributes: AddrAttributes { derivation path: {} }
```

## Delegation

The `issue-delegation-certificate` subcommand enables generation of Byron genesis
delegation certificates, given the following inputs:

   - node configuration yaml file
   - starting epoch of delegation
   - genesis delegator signing key
   - delegate verification key

To check the generated delegation certificate, you can use the `check-delegation` subcommand,
which would verify:

   - certificate signature validity
   - correspondence of the expected issuer/delegate with those on the certificate.

The expected issuer and delegate are supplied through the `--issuer-key` and `--delegate-key`
options.

## Transactions

### Creation

Transactions can be created via the  `issue-genesis-utxo-expenditure` & `issue-utxo-expenditure` commands.

The easiest way to create a transaction is via the `scripts/issue-genesis-utxo-expenditure.sh` script as follows:

`./scripts/issue-genesis-utxo-expenditure.sh transaction_file`

This will run `scripts/genesis.sh` if you do not have a genesis file and will create a tx file with the name `transaction_file`. The script `scripts/issue-genesis-utxo-expenditure.sh` has defaults for all the requirements of the `issue-genesis-utxo-expenditure` command.

### Submission

The `submit-tx` subcommand provides the option of submitting a pre-signed
transaction, in its raw wire format (see GenTx for Byron transactions).

The canned `scripts/submit-tx.sh` script will submit the supplied transaction to a testnet
launched by `scripts/shelley-testnet-*.sh` family of scripts.

### Issuing UTxO expenditure (genesis and regular)

To make a transaction spending UTxO, you can either use the:

  - `issue-genesis-utxo-expenditure`, for genesis UTxO
  - `issue-utxo-expenditure`, for normal UTxO

subcommands directly, or, again use canned scripts that will make transactions tailored
for the aforementioned testnet cluster:

  - `scripts/issue-genesis-utxo-expenditure.sh`.
  - `scripts/issue-utxo-expenditure.sh`.

The script requires the target file name to write the transaction to, input TxId
(for normal UTxO), and optionally allows specifying the source txin output index,
source and target signing keys and lovelace value to send.

The target address defaults to the 1-st richman key (`configuration/delegate-keys.001.key`)
of the testnet, and lovelace amount is almost the entirety of its funds.

# Local node queries

You can query the tip of your local node via the `get-tip` command as follows

1. Open `tmux`
2. Run `cabal build cardano-node`
3. Run `./scripts/shelley-testnet-live.sh`
4. `cabal exec cardano-cli -- get-tip --config configuration/log-config-0.liveview.yaml --socket-path socket/0`

You will see output from stdout in this format:
```
Current tip:
Block hash: 4ab21a10e1b25e39
Slot: 6
Block number: 5
```

# Development

run *ghcid* with: `ghcid -c "cabal v2-repl exe:cardano-node --reorder-goals"`

# Debugging

### Pretty printing CBOR encoded files

It may be useful to print the on chain representations of blocks, delegation certificates, txs and update proposals. There are two commands that do this (for any cbor encoded file):

To pretty print as CBOR:
`cabal exec cardano-cli -- pretty-print-cbor --filepath CBOREncodedFile`

### Validate cbor files

You can validate Byron era blocks, delegation certificates, txs and update proposals with the `validate-cbor` command.

`cabal exec cardano-cli -- validate-cbor --byron-block --filepath CBOREncodedByronBlockFile`
