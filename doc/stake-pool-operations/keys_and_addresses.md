# Creating keys and addresses

In the Shelley era of Cardano, every stakeholder can have two sets of keys and addresses:

* Payment Keys and addresses: To send and receive transactions
* Stake Keys and addresses: To control protocol participation, create a stake pool, delegate and receive rewards.

#### Payment key pair

To generate a _payment key pair_:

    cardano-cli shelley address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey

This creates two files (`payment.vkey` and `payment.skey`), one containing the _public verification key_, one the _private signing key_.

#### Stake key pair
To generate a _stake key pair_ :

		cardano-cli shelley stake-address key-gen \
		--verification-key-file stake.vkey \
		--signing-key-file stake.skey

#### Payment address
When including both verification keys `payment.vkey` and `stake.vkey` to build the address, the resulting `payment address` is associated with this stake keys and address.

		cardano-cli shelley address build \
		--payment-verification-key-file payment.vkey \
		--stake-verification-key-file stake.vkey \
		--out-file payment.addr \
		--testnet-magic 42

#### Stake address

To generate a `stake address`:

		cardano-cli shelley stake-address build \
		--stake-verification-key-file stake.vkey \
		--out-file stake.addr \
		--testnet-magic 42

This address __CAN'T__ receive payments but will receive the rewards from participating in the protocol.



#### Query the balance of an address

To query the balance of an address we need a running node and the environment variable `CARDANO_NODE_SOCKET_PATH` set to the path to the node.socket:

    cardano-cli shelley query utxo \
    --address $(cat payment.addr) \
    --testnet-magic 42

```
                            TxHash                                 TxIx        Lovelace
--------------------------------------------------------------------------------------------
```

**Note. `--testnet magic 42` identifies the testnets, for mainnet, use `--mainnet` instead.
