# Building and signing transactions

Transactions vary in complexity, depending on their intended outcomes, but all transactions share a number of attributes:

* Input - contains funds that are spent by the transaction. It is simply the output of an earlier transaction. A transaction can have multiple inputs.
* Output - determine where the funds go to. An output is given by a payment address and an amount. A transaction can have multiple outputs.
* Payment address - an address that can receive payments, This is the only type of addresses that can be specified in a transaction output.
* Payment and stake key pairs - sets of files containing a public verification key and a private signing key.
* Time-to-live (TTL) - represents a slot, or deadline by which a transaction must be submitted. The TTL is an absolute slot number, rather than a relative one, which means that the --ttl value should be greater than the current slot number. A transaction becomes invalid once its ttl expires.

To create a transaction we need to follow this process:

* Get the protocol parameters
* Draft the transaction
* Calculate the fee
* Define the time-to-live (TTL) for the transaction
* Build the transaction
* Sign the transaction
* Submit the transaction

**Protocol parameters**

Query and save the parameters in **protocol.json**

    cardano-cli shelley query protocol-parameters \
    --mainnet \
    --out-file protocol.json

**Draft the transaction**

In the draft `tx-out`, `ttl` and `fee` can be zero. Later we use the `out-file` `tx.draft` to calculate the `fee`

    cardano-cli shelley transaction build-raw \
    --tx-in <TxHash>#<TxIx> \
    --tx-out <Address>+<Lovelace> \
    --tx-out <Address>+0 \
    --ttl 0 \
    --fee 0 \
    --out-file tx.draft

**Calculate the fee**

Use `tx.draft` as `tx-body-file`. **Witnesses** are the amount of keys that must sign the transaction.   

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

For example:

    > 167965

**Determine the TTL**

When building and submitting a transaction you need to check the current tip of the blockchain, for example, if the tip is slot 4000, you should set the TTL to (4000 + N slots), so that you have enough time to build and submit a transaction. Submitting a transaction with a TTL set in the past would result in a tx error.

    cardano-cli shelley query tip --mainnet

Look for the value of `SlotNo`

    {
        "blockNo": 16829,
        "headerHash": "3e6f59b10d605e7f59ba8383cb0ddcd42480ddcc0a85d41bad1e4648eb5465ad",
        "slotNo": 369200
    }

Therefore, if N = 200 slots

    ttl = 369200 + 200
    ttl = 369400
    
**Build the transaction**

This time we include all the paramenters:

    cardano-cli shelley transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+100000000 \
    --tx-out $(cat payment.addr)+999899832035 \
    --ttl 369400 \
    --fee 167965 \
    --out-file tx.raw

**Signing**

A transaction must prove that it has the right to spend its inputs. In the most common case, this means that a transaction must be signed by the signing keys belonging to the payment addresses of the inputs. If a transaction contains certificates, it must additionally be signed by somebody with the right to issue those certificates. For example, a stake address registration certificate must be signed by the signing key of the corresponding stake key pair.

    cardano-cli shelley transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --mainnet \
    --out-file tx.signed

**Submit**

    cardano-cli shelley transaction submit \
    --tx-file tx.signed \
    --mainnet