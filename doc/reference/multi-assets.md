# Multi-assets

From the Mary era onwards, Cardano supports [multi-assets](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ma/latest/download-by-type/doc-pdf/shelley-ma).

### What is a multi-asset?

Multi-assets are user defined tokens. They are native i.e you don't have to use smart contracts to mint/burn them and a single tx output can have a mixture of ADA and multi-assets. Each token is identified by its asset ID which consists of a policy ID (hash of the minting policy) and an asset name. Tokens with the same asset ID are fungible.

### How to mint a multi-asset token

#### Step 1 - create a script

First, generate the keys that you require witnesses from using the
`cardano-cli address key-gen` command. Until we hard fork to the Mary era, we will
use a simple script to generate our policy ID. Note that you can use a Plutus script when they are available.
Construct a simple script in JSON syntax
as described [here](./simple-scripts.md). For this example, we will describe the process using the following script:

```json
 {
  "keyHash": $KEYHASH,
  "type": "sig"
 }
```

where `$KEYHASH` is generated as follows:

```bash
cardano-cli address key-hash --payment-verification-key-file policy.vkey
```

Generate the policy ID as from the simple script jus created:

```bash
cardano-cli transaction policyid --script-file policy.script
```

Construct the tx body and specify the multi-asset you would like to mint. Note that you must spend at least the minimum UTxO value. In the txbody below we are minting 5 tokens of a particular multi-asset and spending 5 those tokens.

```bash
cardano-cli transaction build-raw \
            --mary-era \
            --fee 0 \
            --tx-in $TXIN \
            --tx-out $ADDR + 5 $POLICYID.couttscoin\
            --mint 5 $POLICYID.yourassetname \
            --out-file txbody
```

Sign the transaction with the appropriate signing keys and include the script:

```bash
cardano-cli transaction sign \
            --signing-key-file txin.skey \
            --signing-key-file policy.skey \
            --script-file $SCRIPT \
            --testnet-magic 42 \
            --tx-body-file  txbody \
            --out-file      tx
```

Submit tx:

```bash
cardano-cli transaction submit --tx-file  tx --testnet-magic 42
```

### How to burn a multi-asset token

Create tx body that will burn 5 couttscoins:

```bash
cardano-cli transaction build-raw \
            --mary-era \
            --fee 0 \
            --tx-in $TXIN \
            --tx-out $TXOUT\
            --mint -5 $POLICYID.couttscoin \
            --out-file txbodyburn
```
Sign the transaction as before:

```bash
cardano-cli transaction sign \
            --signing-key-file txin.skey \
            --signing-key-file policy.skey \
            --script-file $SCRIPT \
            --testnet-magic 42 \
            --tx-body-file  txbodyburn \
            --out-file      txburn
```

Submit tx:

```bash
cardano-cli transaction submit --tx-file txburn --testnet-magic 42
```