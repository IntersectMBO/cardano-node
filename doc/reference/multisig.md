# Multi-Signature Scripts

A multi-signature scheme allows an unspent transaction output to be used as an input to
a new transaction if a pre-defined combination of signatures is provided, e.g., two persons
have to sign simultaneously, two out of three keys have to be provided, etc.

The multi-signature scripts are written as JSON objects. We have three types of scripts:

## all

The `"type"` key's value, `"all"`, indicates that in order to spend this tx output, we require the corresponding signatures of all the payment key hashes listed.

```json
{
    "scripts": [
        {
            "paymentKeyHash": "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "a687dcc24e00dd3caafbeb5e68f97ca8ef269cb6fe971345eb951756",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "0bd1d702b2e6188fe0857a6dc7ffb0675229bab58c86638ffa87ed6d",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "dd0044a26cf7d4491ecea720fda11afb59d5725b53afa605fdf695e6",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "cf223afe150cc8e89f11edaacbbd55b011ba44fbedef66fbd37d8c9d",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "372643e7ef4b41fd2649ada30a89d35cb90b7c14cb5de252e6ce6cb7",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "aa453dc184c5037d60e3fbbadb023f4a41bac112f249b76be9bb37ad",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "6b732c60c267bab894854d6dd57a04a94e603fcc4c36274c9ed75952",
            "type": "requireSignature"
        }
    ],
    "type": "all"
}
```



## any

The `"type"` key's value, `"any"`, indicates that in order to spend this tx output, we require one corresponding signature from the listed payment key hashes.

```json
{
    "scripts": [
        {
            "paymentKeyHash": "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "b12e094d1db7c0fba5121f22db193d0060efed8be43654f861bb68ae",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "9be49d56442b4b8b16cab4e43e238bbdefc6c803d554c82fcd5facc3",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "622be5fab3b5c3f371a50a535e4d3349c942a98cecee93b24e2fd11d",
            "type": "requireSignature"
        }
    ],
    "type": "any"
}
```

## atLeast

The `"type"` key's value, `"atLeast"`, indicates that in order to spend this tx output, we require `"atLeast"` 2 corresponding signatures from the list of payment key hashes. The `"required"` key indicates the minimum number of signatures we need to spend the tx output.

```json
{
    "scripts": [
        {
            "paymentKeyHash": "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538",
            "type": "requireSignature"
        },
        {
            "paymentKeyHash": "686024aecb5884d73a11b9ae4e63931112ba737e878d74638b78513a",
            "type": "requireSignature"
        }
    ],
    "required": 2,
    "type": "atLeast"
}
```
### Example of using multi-signature scripts

Let's walk through how one would use a multi-signature script. This is a multistep process, involving the creation of a multi-signature address, sending ADA to that address followed by gathering the required witnesses in order to spend the ADA from the multi-signature address. We will demonstrate this with an `all` script.

#### Sending ada to a script address

#### Step 1 - Create multi-signature script

You will first need to generate the keys that you require witnessing from via the `cardano-cli shelley address key-gen` command. Then you construct the multi-signature script as follows:

```
cardano-cli -- shelley transaction build-multisig
  --all
  --payment-verification-key-file payVerKey1
  --payment-verification-key-file payVerKey2
  --payment-verification-key-file payVerKey3
  --out-file allMultiSigScript
```

This will output a JSON file with the `all` format described above.


#### Step 2 - Create multi-signature address
We need a special multi-signature address to use with our multi-signature script. Please note the network magic must match with your network's magic (or if its mainnet use the `--mainnet` flag instead). This can be constructed as follows:

```
cardano-cli shelley address build-script
  --script-file allMultiSigScript
  --testnet-magic 42
  --out-file script.addr
```

#### Step 3 - Construct and submit a tx to the multi-signature address
We then need to construct and submit a tx to send ADA to the multi-signature address.

Construct the tx body:
```bash
cardano-cli shelley transaction build-raw
    --ttl 1000
    --fee 0
    --tx-in utxoinput
    --tx-out $(< script.addr)+$amount
    --out-file txbody
```

Create the utxo witness:

```bash
cardano-cli shelley transaction witness
  --tx-body-file txbody
  --signing-key-file utxoSignKey
  --testnet-magic 42
  --out-file utxoWitness
```

Assemble the utxo witness and tx body to create the transaction:

```bash
cardano-cli shelley transaction sign-witness
  --tx-body-file txbody
  --witness-file utxoWitness
  --out-file allWitnessesTx
```

After submiting the above tx, the inputs associated with the multi-signature address are now "guarded" by the multi-signature script.

### Sending ADA from a script address

#### Step 1 - Construct the tx body

```bash
cardano-cli shelley transaction build-raw \
    --ttl 1000 \
    --fee 0 \
    --tx-in (txin of script address)
    --tx-out yourspecifiedtxout \
    --out-file spendScriptTxBody
```

#### Step 2 - Construct the required witnesses

We need to construct the script witness and the three required witnesses set out by the script. We do this as follows:

```bash
cardano-cli shelley transaction witness \
  --tx-body-file spendScriptTxBody \
  --script-file allMultiSigScript \
  --testnet-magic 42 \
  --out-file scriptWitness

cardano-cli shelley transaction witness \
  --tx-body-file spendScriptTxBody \
  --signing-key-file paySignKey1 \
  --testnet-magic 42 \
  --out-file key1witness

cardano-cli shelley transaction witness \
  --tx-body-file spendScriptTxBody \
  --signing-key-file paySignKey2 \
  --testnet-magic 42 \
  --out-file key2witness

cardano-cli shelley transaction witness \
  --tx-body-file spendScriptTxBody \
  --signing-key-file paySignKey3 \
  --testnet-magic 42 \
  --out-file key3witness

```

#### Step 3 - Construct & submit the transaction
You must assemble the transaction with the script witness and all the required witnesses.

```bash
cardano-cli shelley transaction sign-witness \
  --tx-body-file spendScriptTxBody \
  --witness-file scriptWitness \
  --witness-file key1witness \
  --witness-file key2witness \
  --witness-file key3witness \
  --out-file spendMultiSig
```
You can now submit this tx!