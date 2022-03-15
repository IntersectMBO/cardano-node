# Cardano Node CLI Reference

The command line interface (CLI) provides a collection of tools for generating keys, constructing transactions, creating certificates, and performing other important tasks. It is organized in a hierarchy of subcommands, and each level comes with its own built-in documentation of command syntax and options.

This section provides a reference of the core `cardano-cli` commands and their associated subcommands:

*cardano-cli*
The set of `cardano-cli` commands include:
* `address`: payment address commands
* `stake-address`: stake address commands
* `transaction`: transaction commands
* `node`: node operation commands
* `stake-pool`: stake pool commands
* `query`: node query commands. Commands in this group query the local node whose Unix domain socket is obtained from the CARDANO_NODE_SOCKET_PATH environment variable.
* `genesis`: genesis block commands
* `text-view`: commands for dealing with text view files that are stored on disk, such as transactions or addresses
* `governance`: governance commands

*cardano-cli address*
The `address` command contains the following subcommands:
* `key-gen`: creates a single address key pair
* `key-hash`: prints the hash of an address to stdout
* `build`: builds a payment address, with optional delegation to a stake address
* `build-script`: builds a token locking script
* `info`: prints details about the address

*cardano-cli stake-address*
The `stake-address` command contains the following subcommands:
* `key-gen`: creates a single address key pair
* `build`: builds a stake address
* `key-hash`: prints the hash of a stake verification key
* `registration-certificate`: creates a registration certificate
* `delegation-certificate`: creates a stake address delegation certificate
* `deregistration-certificate`: creates a de-registration certificate

*cardano-cli transaction*
The `transaction` command contains the following subcommands:
* `build-raw`: builds a low-level transaction (uses the `--cardano-mode`, `--byron-mode`, `--shelley-mode` flags)
* `build`: builds an automatically balanced transaction (automatically calculates fees)
* `sign`: signs the transaction
* `assemble`: combines and assembles the transaction witness(es) with a transaction body to create a transaction
* `witness`: witnesses a transaction
* `submit`: submits the transaction to the local node whose Unix domain socket is obtained from the CARANO_NODE_SOCKET_PATH environment variable (uses the `--cardano-mode`, `--byron-mode`, `--shelley-mode` flags)
* `calculate-min-fee`: calculates the minimum fee for the transaction
* `calculate-min-required-utxo`: calculates the minimum required ADA for a transaction output
* `hash-script-data`: calculates the hash of script data (datums)
* `txid`: retrieves the transaction ID
* `policyid`: retrieves the policy ID
* `view`: pretty prints a transaction

*cardano-cli node*
The `node` command contains the following subcommands:
* `key-gen`: creates a key pair for a node operator's offline key and a new certificate issue counter
* `key-gen-KES`: creates a key pair for a node KES operational key
* `key-gen-VRF`: creates a key pair for a node VRF operational key
* `key-hash-VRF`: creates a key hash for a node VRF operational key
* `new-counter`: keeps track of the number of KES evolutions for a given operational certificate hot key
* `issue-op-cert`: issues a node operational certificate

*cardano-cli stake-pool*
The `stake-pool` command contains the following subcommands:
* `registration-certificate`: creates a stake pool registration certificate
* `de-registration-certificate`: creates a stake pool de-registration certificate
* `id`: builds pool id from the offline key
* `metadata-hash`:  retrieves the metadata hash

*cardano-cli query*
The `query` command contains the following subcommands:
* `protocol-parameters` (advanced): retrieves the node's current pool parameters (a raw dump of `Ledger.ChainDepState`).
* `tip`: gets the node's current tip (slot number, hash, and block number)
* `stake-pools`: gets the node's current set of stake pool ids
* `utxo`: retrieves the node's current UTxO, filtered by address
* `ledger-state` (advanced):  dumps the current state of the node (a raw dump of `Ledger.NewEpochState`)* `stake-distribution`: gets the node's current set of stake pool ids
* `protocol-state` (advanced): dumps the node's current protocol state
* `stake-address-info`: gets the current delegations and reward accounts filtered by stake address.
* `stake-distribution`: gets the node's current aggregated stake distribution
* `stake-snapshot` (advanced): gets the stake snapshot information for a stake pool
* `pool-params` (advanced): gets the current and future parameters for a stake pool
* `leadership-schedule`: gets the slots in which the node is slot leader for the current or following epoch
* `kes-period-info` (advanced): returns diagnostic information about your operational certificate

*cardano-cli governance*
The `governance` command contains the following subcommands:
* `create-mir-certificate`: creates an MIR (move instantaneous rewards) certificate
* `create-update-proposal`: creates an update proposal
* `create-genesis-key-certificate`: retrieves the genesis key certificate

*cardano-cli genesis*
The `genesis` command contains the following subcommands:
* `key-gen-genesis`: creates a genesis key pair
* `key-gen-delegate`: creates a genesis delegate key pair
* `key-gen-utxo`: creates a genesis UTxO key pair
* `key-hash`: prints the identifier, or hash, of a public key
* `get-ver-key`: derives verification key from a signing key
* `initial-addr`: gets the address for an initial UTxO based on the verification key
* `initial-txin`: gets the transaction ID for an initial UTxO based on the verification key.
* `create`: creates a genesis file from a genesis template, as well as genesis keys, delegation keys, and spending keys.
* `create-staked`: creates a staked genesis file
* `hash`: retrieves the hash value

*cardano-cli text-view*
The `text-view` command contains the following subcommand:
* `decode-cbor`: prints a text view file as decoded CBOR.



## Advanced Commands

`query kes-period-info`: This command runs the following checks on your operational certificate and your operational certificate issue counter:
- Do the counters of the issue counter and operational certificate match?
- Do the counters match what is currently in the node state?
- Does the KES key period specified in your operational certificate fall within the current KES key period?
We essentially check the predicates of the OCERT rule in the cardano-ledger specification, and we print additional diagnostic information as follows:
```
✓ The operational certificate counter agrees with the node protocol state counter
✓ Operational certificate's kes period is within the correct KES period interval
{
    "qKesNodeStateOperationalCertificateNumber": 6,
    "qKesCurrentKesPeriod": 404,
    "qKesOnDiskOperationalCertificateNumber": 6,
    "qKesRemainingSlotsInKesPeriod": 3760228,
    "qKesMaxKESEvolutions": 62,
    "qKesKesKeyExpiry": "2022-03-20T21:44:51Z",
    "qKesEndKesInterval": 434,
    "qKesStartKesInterval": 372,
    "qKesSlotsPerKesPeriod": 129600
}
```

`query leadership-schedule`: This command can calculate the leadership slots in the current epoch or the following epoch for an SPO with output as follows:
```
     SlotNo                          UTC Time
-------------------------------------------------------------
     4073                   2021-12-29 17:26:54.998001755 UTC
     4126                   2021-12-29 17:27:00.298001755 UTC
     4206                   2021-12-29 17:27:08.298001755 UTC
     4256                   2021-12-29 17:27:13.298001755 UTC
     4309                   2021-12-29 17:27:18.598001755 UTC
     4376                   2021-12-29 17:27:25.298001755 UTC
     4423                   2021-12-29 17:27:29.998001755 UTC
     4433                   2021-12-29 17:27:30.998001755 UTC
```


`transaction build ... --calculate-plutus-script-cost`: Using the `--calculate-plutus-script-cost` flag with the `transaction build` command will calculate the cost of the Plutus script(s) within the transaction body and output it as JSON.
```
[
    {
        "executionUnits": {
            "memory": 1700,
            "steps": 476468
        },
        "lovelaceCost": 133,
        "scriptHash": "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
    }
]
```
