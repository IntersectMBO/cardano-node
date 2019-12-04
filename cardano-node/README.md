<p align="center">
  <big><strong>Cardano Node</strong></big>
</p>

![alt text](https://github.com/input-output-hk/cardano-node/blob/media/Cardano-ADA.png?raw=true)

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-node/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-node.svg?style=for-the-badge" /></a>
  <a href="https://buildkite.com/input-output-hk/cardano-node"><img src="https://img.shields.io/buildkite/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a/master?label=BUILD&style=for-the-badge"/></a>
</p>

<hr/>

- The cardano-node is the top level for the node and
  aggregates the other components from other packages: consensus, ledger and
  networking, with configuration, CLI, logging and monitoring.

- The node no longer incorporates wallet or explorer functionality. The wallet
  backend and explorer backend are separate components that run in separate
  external processes that communicate with the node via local IPC.

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

## Cardano Node CLI

This refers to the client that is used for running a node.

The general synopsis is as follows:
```

Usage: cardano-node --topology FILEPATH --database-path FILEPATH
                    --genesis-file FILEPATH [--delegation-certificate FILEPATH]
                    [--signing-key FILEPATH] --socket-dir FILEPATH
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

`--socket-dir` - Path to the socket directory.

`--host-addr` - Optionally specify your node's IPv4 or IPv6 address.

`--port` - Specify which port to assign to the node.

`--config` - Specify the filepath to the config `.yaml` file. This file is responsible for all the other node's required settings. See examples in `configuration` (e.g. `log-config-0.yaml`).


## Configuration `.yaml` files

The `--config` flag points to a `.yaml` file that is responsible to configuring the logging & other important settings for the node.
Some of the more important settings are as follows:

`NodeId: 0`  -- Soon to be removed from config file. Used to identify nodes in local clusters.

`Protocol: RealPBFT` -- Protocol the node will execute

`RequiresNetworkMagic`: RequiresNoMagic -- Used to distinguish between mainnet (`RequiresNoMagic`) and testnets (`RequiresMagic`)

`ViewMode: SimpleView` -- Choose between SimpleView or LiveView


## Logging

Logs are output to the `log/` dir.


## Connect to mainnet

Run `./scripts/mainnet.sh`

This script connects to several IOHK nodes on mainnet.



# Shelley Testnet

The `./scripts/shelley-testnet.sh` script starts up three nodes that are
connected via TCP sockets to each other and produce blocks according to the
algorithm selected (e.g. "BFT").  The blocks are shared among the nodes and
after verification integrated into a nodes ledger.  The user can submit
transactions to a node which includes them in its local mempool, and eventually
in the next block it will create.


```

 +---------+         +---------+
 |         | <-----> |         |
 | node 0  |         | node 1  |
 |         | <-+ +-> |         |
 +---------+   | |   +---------+
               v v

            +---------+
            |         |
            | node 2  |
            |         |
            +---------+


```

## Startup testnet

Add the next two lines to your $HOME/.tmux.conf file:
```
set-window-option -g mouse on
set -g default-terminal "tmux-256color"
```

1.) create a `tmux` session

`tmux new-session -s Demo`

2.) run the demo script in this new session

`./scripts/shelley-testnet.sh`

## Submit a tx to the testnet

Following the instructions above, the window of the terminal will be split into four panes.
Three of the panes showing the nodes running and a shell to enter commands for transaction submission, e.g.

```
./scripts/submit-tx.sh generated-tx-file
```
The `submit-tx.sh` script by default sends the transaction to node with node id 0.

See cardano-cli's Transactions section for instructions on how to generate a tx.

## Startup testnet with dns

You can run: 

`./scripts/shelley-testnet-dns.sh` 

instead of `shelley-testnet.sh`.  
It requires that the addresses `local.iohk.io` and `local6.iohk.io` resolve to `127.0.0.1` and `::1` respectively.  

You can use [unbound](https://github.com/NLnetLabs/unbound) dns server. You can use the following `/etc/unbound/unbound.conf` file:
```
server:
  verbosity: 1
  local-data: "local.iohk.io A 127.0.0.1"
  local-data: "local6.iohk.io AAAA ::1"
```

# `cardano-cli`

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

The general synopsis is as follows:
 ```
   Usage: cardano-cli (--byron-legacy | --real-pbft) (Genesis Related CMDs | Key Related CMDs | Delegations related CMDs | Tx related CMDs)
```

NOTE: the exact invocation command depends on the environment.  If you have only
built `cardano-cli`, without installing it, then you have to prepend `cabal
run -- ` before `cardano-cli`.  We henceforth assume that the necessary
environment-specific adjustment has been made, so we only mention `cardano-cli`.

The `--byron-legacy` and `--real-pbft` options immediately preceding the
subcommand have the role of choosing the set of Cardano algorithms for the purpose
of subcommand operations.

The subcommands are subdivided in groups, and their full list can be seen in the
output of `cardano-cli --help`.

All subcommands have help available:

```
$ cabal run -- cardano-cli --real-pbft migrate-delegate-key-from --help
Usage: cardano-cli --real-pbft migrate-delegate-key-from (--byron-legacy | --real-pbft)
                                                          --to FILEPATH --from FILEPATH
  Migrate a delegate key from an older system version.

Available options:
  --byron-legacy           Byron/Ouroboros Classic suite of algorithms
  --bft                    BFT consensus
  --praos                  Praos consensus
  --mock-pbft              Permissive BFT consensus with a mock ledger
  --real-pbft              Permissive BFT consensus with a real ledger
  --to FILEPATH            Non-existent file to write the signing key to.
  --from FILEPATH          Signing key file to migrate.
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

### Hashing

To underscore the identity of the genesis being employed by the node, the latter
requires hash of the genesis JSON file as a configuration parameter
(`--genesis-hash`).

This hash can be obtained by the means of the `print-genesis-hash` subcommand --
in a form expected by the node.

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
$ cabal run -- cardano-cli --real-pbft migrate-delegate-key-from byron-legacy \
                                           --from key0.sk --to key0.pbft
```

### Signing key queries

One can gather information about a signing key's properties through the `signing-key-public`
and `signing-key-address` subcommands (the latter requires the network magic):

```
$ cabal run -- cardano-cli --real-pbft signing-key-public \
                                           --secret key0.pbft
public key hash: a2b1af0df8ca764876a45608fae36cf04400ed9f413de2e37d92ce04
     public key: sc4pa1pAriXO7IzMpByKo4cG90HCFD465Iad284uDYz06dHCqBwMHRukReQ90+TA/vQpj4L1YNaLHI7DS0Z2Vg==

$ cabal run -- cardano-cli --real-pbft signing-key-address \
                                           --secret key0.pbft  \
                                           --testnet-magic 459045235
2cWKMJemoBakxhXgZSsMteLP9TUvz7owHyEYbUDwKRLsw2UGDrG93gPqmpv1D9ohWNddx
VerKey address with root e5a3807d99a1807c3f161a1558bcbc45de8392e049682df01809c488, attributes: AddrAttributes { derivation path: {} }
```

## Delegation

The `issue-delegation-certificate` subcommand enables generation of Byron genesis
delegation certificates, given the following inputs:

   - protocol magic
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

### Submission

The `submit-tx` subcommand provides the option of submitting a pre-signed
transaction, in its raw wire format (see GenTx for Byron transactions).

The canned `scripts/submit-tx.sh` script will submit the supplied transaction to a testnet
launched by `scripts/shelley-testnet*.sh` family of scripts.

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

# Development

run *ghcid* with: `ghcid -c "cabal repl exe:cardano-node --reorder-goals"`
