# Building and signing transactions

Transactions vary in complexity, depending on their intended outcomes, but all transactions share a number of attributes:

* Input - contains funds that are spent by the transaction. It is simply the output of an earlier transaction. A transaction can have multiple inputs.
* Output - determine where the funds go to. An output is given by a payment address and an amount. A transaction can have multiple outputs.
* Payment address - an address that can receive payments, This is the only type of addresses that can be specified in a transaction output.
* Payment and stake key pairs - sets of files containing a public verification key and a private signing key.
* Time-to-live (TTL) - represents a slot, or deadline by which a transaction must be submitted. The TTL is an absolute slot number, rather than a relative one, which means that the --ttl value should be greater than the current slot number. A transaction becomes invalid once its ttl expires.

When building and submitting a transaction you need to check the current tip of the blockchain, for example, if the tip is slot 4000, and you want to submit a transaction to send some ADA, you should set the TTL to 4100, so that you have enough time to build and submit a transaction. Submitting a transaction with a TTL set in the past would result in a tx submission error.

In the following example, the ttl value for this transaction is 200000. The output is written to a `tx001.raw` file.

`cardano-cli shelley transaction build-raw \
     --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
     --tx-out $(cat payment2.addr)+100000000 \
     --tx-out $(cat payment.addr)+999899832035 \
     --ttl 200000 \
     --fee 167965 \
     --out-file tx001.raw`

**Signing**
A transaction must prove that it has the right to spend its inputs. In the most common case, this means that a transaction must be signed by the signing keys belonging to the payment addresses of the inputs. If a transaction contains certificates, it must additionally be signed by somebody with the right to issue those certificates. For example, a stake address registration certificate must be signed by the signing key of the corresponding stake key pair

**Fee calculation**
Every transaction on the blockchain carries a fee, which needs to be calculated each time. This fee calculation requires protocol parameters.

The Cardano CLI is used to query the protocol parameters which are then written to the protocol.json file, for example:

`cardano-cli shelley query protocol-parameters \
     --testnet-magic 42 \
     --out-file protocol.json`

where `magic42` identifies the testnet. Other testnets would be identified by different names and numbers.

This is an example of a fee calculation transaction:

 `cardano-cli shelley transaction calculate-min-fee \
     --tx-in-count 1 \
     --tx-out-count 2 \
     --ttl 250000 \
     --testnet-magic 42 \
     --signing-key-file payment.skey \
     --protocol-params-file protocol.json`

**Building a raw transaction**

A transaction is considered 'raw' until it reaches the signing stage.

Use the build-raw command to create raw transactions. In this example, the transaction is written to a tx001.raw file.

 `cardano-cli shelley transaction build-raw \
     --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
     --tx-out $(cat payment2.addr)+100000000 \
     --tx-out $(cat payment.addr)+999899832035 \
     --ttl 200000 \
     --fee 167965 \
     --out-file tx001.raw`

**Signing a transaction**

Keys are created in pairs (public verification key and private signing key). For example, payment.vkey and payment.skey, respectively.

Use the key-gen command to generate the keys.

 `cardano-cli shelley address key-gen \
     --verification-key-file payment.vkey \
     --signing-key-file payment.skey`

The following example uses the payment key payment.skey to sign the raw transaction tx001.raw and writes it out to a tx001.signed file.

`cardano-cli shelley transaction sign \
     --tx-body-file tx001.raw \
     --signing-key-file payment.skey \
     --testnet-magic 42 \
     --out-file tx001.signed`

Once a transaction is signed, it can be submitted to a node, which will share it with the network so it can be included in a block on the chain.

**Registering a stake address**

A stake address must be registered on the blockchain if you intend to delegate your stake to one or more pools later on. Registering a stake address requires a certificate and a deposit.

To create a registration certificate, use the following code:

`cardano-cli shelley stake-address registration-certificate \
     --staking-verification-key-file stake.vkey \
     --out-file stake.cert`

A deposit amount is specified in the protocol.json file, under keyDeposit. This amount has to be included in the transaction containing the certificate, but will be refunded when the key is deregistered.

`"keyDeposit": 400000,`
