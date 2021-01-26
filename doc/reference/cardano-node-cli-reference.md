# Cardano Node CLI Reference

The command line interface (CLI) provides a collection of tools for key generation, transaction construction, certificate creation and other important tasks. It is organized in a hierarchy of subcommands based on the targeted era, and each level comes with its own built-in documentation of command syntax and options.

This section provides a reference of the core `cardano-cli` commands and their associated sub commands:

*cardano-cli*
The set of `cardano-cli` commands include:
* `address`: Payment address commands
* `stake-address`: Stake address commands
* `transaction`: Transaction commands
* `node`: Node operation commands
* `stake-pool`: Stake pool commands
* `query`: Node query commands. This queries the local node whose Unix domain socket is obtained from the CARDANO_NODE_SOCKET_PATH environment variable.
* `genesis`: Genesis block commands
* `text-view`: Commands for dealing with  text view files that are stored on disk such as transactions or addresses
* `governance`: Governance commands

*cardano-cli address*
The `address` command contains the following sub commands:
* `key-gen`: Creates a single address key pair
* `key-hash`: Prints the hash of an address to stdout
* `build`: Builds a payment address, with optional delegation to a stake address
* `build-script`: Builds a script address.
* `info`: Prints details about the address

*cardano-cli stake-address*
The `stake-address` command contains the following sub commands:
* `key-gen`: Creates a single address key pair
* `build`: Builds a stake address
* `key-hash`: Print the hash of a stake address key
* `registration-certificate`: Creates a registration certificate
* `delegation-certificate`: Creates a stake address delegation certificate
* `deregistration-certificate`: Creates a de-registration certificate

*cardano-cli transaction*
The `transaction` command contains the following sub commands:
* `build-raw`: Builds a low-level transaction
* `sign`: Signs the transaction
* `witness`: Witnesses a transaction
* `assemble`: Assemble a tx body and witness(es) to form a transaction
* `submit`: Submits the transaction to the local node whose Unix domain socket is obtained from the CARANO_NODE_SOCKET_PATH environment variable.
* `policyid`: Calculate the PolicyId from the monetary policy script.
* `calculate-min-fee`: Calculates the minimum fee for the transaction
* `txid`: Print a transaction identifier

*cardano-cli node*
The `node` command contains the following sub commands:
* `key-gen`: Creates a key pair for a node operator’s offline key and a new certificate issue counter
* `key-gen-KES`: Creates a key pair for a node KES operational key
* `key-gen-VRF`: Creates a key pair for a node VRF operational key
* `new-counter`: Create a new certificate issue counter
* `issue-op-cert`: Issues a node operational certificate

*cardano-cli stake-pool*
The `stake-pool` command contains the following sub commands:
* `registration-certificate`: Creates a stake pool registration certificate
* `de-registration-certificate`: Creates a stake pool de-registration certificate
* `id`: Builds pool id from the offline key
* `metadata-hash`: Print the hash of pool metadata

*cardano-cli query*
The `query` command contains the following sub commands:
* `protocol-parameters`(advanced): retrieves the node’s current pool parameters
* `tip`: Gets the node’s current tip (slot number, hash, and block number)
* `utxo`: Get the node's current UTxO with the option of filtering by address(es)
* `ledger-state` (advanced):  dumps the current state of the node (a raw dump of `Ledger.NewEpochState`)
* `stake-address-info`: Get the current delegations and reward accounts filtered by stake address.
* `stake-distribution`: Get the node's current aggregated stake distribution

*cardano-cli governance*
The `governance` command contains the following sub commands:
* `create-mir-certificate`: Creates an MIR (move instantaneous rewards) certificate
* `create-update-proposal`: Creates an update proposal
* `create-genesis-key-delegation-certificate`: Create a genesis key delegation certificate

*cardano-cli genesis*
The `genesis` command contains the following sub commands:
* `key-gen-genesis`: Creates a Shgenesis key pair
* `key-gen-delegate`: Creates a genesis delegate key pair
* `key-gen-utxo`: Creates a genesis UTxO key pair
* `key-hash`: Prints the identifier, or hash, of a public key
* `get-ver-key`: Derives verification key from a signing key
* `initial-addr`: Gets the address for an initial UTxO based on the verification key
* `initial-txin`: Gets the transaction ID for an initial UTxO based on the verification key.
* `create`: Creates a genesis file from a genesis template, as well as genesis keys, delegation keys, and spending keys.
* `create-staked`: Create a staked genesis file from a genesis template and genesis/delegation/spending keys.
* `hash`: Compute the hash of a genesis file

*cardano-cli text-view*
The `text-view` command contains the following sub command:
* `decode-cbor`: prints a text view file, as decoded CBOR.
