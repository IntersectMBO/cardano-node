# Creating a simple transaction

Creating a transaction requires multiple steps:

* Get the protocol parameters
* Get the UTXO to spend
* Calculate the fee
* Define the time-to-live (TTL) for the transaction
* Build the transaction
* Sign the transaction
* Submit the transaction

#### Get protocol parameters

Get the protocol parameters and save them to `protocol.json` with:

```bash
cardano-cli query protocol-parameters \
  --mainnet \
  --out-file protocol.json
```

## Get the **UTXO** to spend:

Generate the payment address from your payment verification key (see [getting started docs](https://docs.cardano.org/native-tokens/getting-started)):

```bash
cardano-cli address build \
  --payment-verification-key-file payment.vkey \
  --out-file payment.addr \
  --mainnet
```

Which produces the following payment address:

```bash
cat payment.addr
addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz
```

Query all UTXOs belonging to this payment.addr and choose one (or more) that cover the amount you wish to send in the transaction:

```bash
cardano-cli query utxo \
  --address $(cat payment.addr) \
  --mainnet
```

```
                          TxHash                                 TxIx        Amount
---------------------------------------------------------------------------------------
4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99     4         20000000 lovelace
```

## Draft the transaction

Because Cardano uses an [extended UTXO model](https://docs.cardano.org/learn/eutxo-explainer),
a simple transaction requires at least one input and one output, but the most
common case is to use one input and two outputs (you'll see why):

* input: a valid UTXO (unspent and with enough token(s) amount) from `payment.addr`. (see above)
* output1: the address that receives the value of the transaction (receiver.addr).
* output2: the address that receives the change of the transaction (change.addr - if you want to use the original payment.addr to get your change back, simply copy payment.addr to change.addr).

Create a draft of the transaction and save it as tx.draft.

Note that for `--tx-in` we use the following syntax: `TxHash#TxIx` where `TxHash` is the transaction hash and `TxIx` is the index; for `--tx-out` we use: `TxOut+Lovelace` where `TxOut` is the hex encoded address followed by the amount in `Lovelace`. For the transaction draft --tx-out, --invalid-hereafter and --fee can be set to zero.

```bash
cardano-cli transaction build-raw \
  --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
  --tx-out $(cat receiver.addr)+0 \
  --tx-out $(cat change.addr)+0 \
  --invalid-hereafter 0 \
  --fee 0 \
  --out-file tx.draft
```

## Calculate the fee

This is a draft transaction to calculate the fee:

```bash
cardano-cli transaction calculate-min-fee \
  --tx-body-file tx.draft \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --byron-witness-count 0 \
  --mainnet \
  --protocol-params-file protocol.json

> 167965
```

## Calculate the change to send back to change.addr

All amounts must be in lovelace:

```bash
expr <UTXO BALANCE> - <AMOUNT TO SEND> - <TRANSACTION FEE>
```

For example, if we send 10 ada from a UTXO containing 20 ada, the change to send back to `change.addr` after paying the fee is: 9.832035 ada

```bash
expr 20000000 - 10000000 - 167965

> 9832035
```

## Determine the TTL (time to live) for the transaction

To build the transaction we need to specify the **TTL**, which is the slot (unit of time in Cardano) height limit for our transaction to be included in a block. If it is not in a block by that slot, the transaction will be cancelled. So **TTL = slot + N slots**. Where **slot** is the current blockchain slot and **N** is the amount of slots you want to add to give the transaction a window to be included in a block.

Query the tip of the blockchain:

```
cardano-cli query tip --mainnet
```

Look for the value of `slot`

```json
{
  "epoch": 259,
  "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
  "slot": 26633911,
  "block": 5580350
}
```

Calculate your `invalid-hereafter`, for example:  26633911 + 200 slots = 26634111

## Build the transaction

We write the transaction in a file, we will name it `tx.raw`:

```bash
cardano-cli transaction build-raw \
  --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
  --tx-out $(cat receiver.addr)+10000000 \
  --tx-out $(cat change.addr)+9832035 \
  --invalid-hereafter 26634111 \
  --fee 167965 \
  --out-file tx.raw
```

## Sign the transaction

Sign the transaction with the signing key **payment.skey** and save the signed transaction in **tx.signed**

```bash
cardano-cli transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file payment.skey \
  --mainnet \
  --out-file tx.signed
```

## Submit the transaction

```bash
cardano-cli transaction submit \
  --tx-file tx.signed \
  --mainnet
```

## Check the balances

We must give it some time to get incorporated into the blockchain, but eventually, we will see the effect:

```bash
cardano-cli query utxo \
  --address $(cat change.addr) \
  --mainnet
```

```
                            TxHash                                 TxIx         Amount
----------------------------------------------------------------------------------------
b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     1         9832035 lovelace
```

```bash
cardano-cli query utxo \
    --address $(cat receiver.addr) \
    --mainnet
```

```
                            TxHash                                 TxIx         Amount
----------------------------------------------------------------------------------------
b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     0         10000000 lovelace
```

**Note**`--mainnet` identifies the Cardano mainnet, for **preproduction testnet** use `--testnet-magic 1` and for **preview testnet** use `--testnet-magic 2`
