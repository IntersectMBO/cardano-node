# Multi signatures

Multi signature (multisig) is a Cardano Shelley feature that consists of a set of schemes and a simple scripting language. Multisig schemes are used for payment and delegation purposes, specifying one or more combinations of cryptographic signatures needed to authorize a transaction. Multisig allows value addresses and stake addresses to use either keypairs or scripts to authorize and process a transaction.

In Shelley, an address must provide information on how to spend ada and how to control the associated stake. For these reasons, there are two types of addresses: *payment addresses* and *stake addresses*.

Addresses are objects that have a user-facing binary representation, which means that they appear in the UTxO, and users can track them using a wallet or the Cardano Explorer. Addresses also contain credentials that govern access rights. This means that when using a payment address for spending purposes, or a stake address for delegation purposes a witness is required for the credential, that should be specific to the particular transaction. There are two types of such credentials:

+ **Key credential** - a credential can be constructed from a pair of a *signing key (sk)* and a corresponding *verification key (vk)*. The credential is a cryptographic hash of the verification key *H(vk)*. A witness for a key credential consists of the *vk* and a transaction signature of the *sk*.
+ **Script credential** - tokens and stake can also be controlled by a validator script, which can either succeed or fail to validate on a given input. In this case, the credential is the hash of the script. A witness for a script credential is the script itself, as well as a script input that makes it valid.

## Multi signature scripts

In Shelley, multisig script credentials are used to require signatures from multiple parties to validate a transaction. Examples include M of N schemes, where a transaction can be authorized if at least *M* distinct keys, from a set of *N* keys, sign the transaction. With multisig script credentials, it is possible to require single or multiple signatures both for funds spending and stake delegation purposes. Using multisig scripts, a witness should include the validator script matching the hash in the script credential, and a set of witnesses for individual key credentials. The validator script will determine whether those witnesses are sufficient for the funds to be spent.

Multisig script terms form a tree-like structure and are evaluated with the [`evalNativeMultiSigScript` function](https://github.com/input-output-hk/cardano-ledger-specs/blob/5398d8783559c60f2d819e80f84fe430834f2399/shelley/chain-and-ledger/executable-spec/src/Shelley/Spec/Ledger/Tx.hs#L337).

These functions return `true` when supplied key hashes are a valid combination for the script, otherwise, they return `false`. The following are the four constructors that make up the multisig script scheme:

+ RequireSignature - the signature of a key with a specific hash is required
+ RequireAllOf - signatures of all of the keys that hash to the values specified in the given list are required
+ RequireAnyOf - a single signature is required, by a key hashing to one of the given values in the list (this constructor is redundant and can be expressed using RequireMOf)
+ RequireMOf - m of the keys with the hashes specified in the list are required to sign

Multi signature scripts can be written as JSON objects. There are three types of scripts:

## all

The `type` key's value `all` indicates that in order to spend this tx output, corresponding signatures of all the listed payment key hashes are required.

```json
{
    "scripts": [
        {
            "keyHash": "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a",
            "type": "sig"
        },
        {
            "keyHash": "a687dcc24e00dd3caafbeb5e68f97ca8ef269cb6fe971345eb951756",
            "type": "sig"
        },
        {
            "keyHash": "0bd1d702b2e6188fe0857a6dc7ffb0675229bab58c86638ffa87ed6d",
            "type": "sig"
        },
        {
            "keyHash": "dd0044a26cf7d4491ecea720fda11afb59d5725b53afa605fdf695e6",
            "type": "sig"
        },
        {
            "keyHash": "cf223afe150cc8e89f11edaacbbd55b011ba44fbedef66fbd37d8c9d",
            "type": "sig"
        },
        {
            "keyHash": "372643e7ef4b41fd2649ada30a89d35cb90b7c14cb5de252e6ce6cb7",
            "type": "sig"
        },
        {
            "keyHash": "aa453dc184c5037d60e3fbbadb023f4a41bac112f249b76be9bb37ad",
            "type": "sig"
        },
        {
            "keyHash": "6b732c60c267bab894854d6dd57a04a94e603fcc4c36274c9ed75952",
            "type": "sig"
        }
    ],
    "type": "all"
}
```

## any

The `type` key's value `any` indicates that in order to spend this tx output, one corresponding signature from the listed payment key hashes is required.

```json
{
    "scripts": [
        {
            "keyHash": "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09",
            "type": "sig"
        },
        {
            "keyHash": "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321",
            "type": "sig"
        },
        {
            "keyHash": "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8",
            "type": "sig"
        },
        {
            "keyHash": "b12e094d1db7c0fba5121f22db193d0060efed8be43654f861bb68ae",
            "type": "sig"
        },
        {
            "keyHash": "9be49d56442b4b8b16cab4e43e238bbdefc6c803d554c82fcd5facc3",
            "type": "sig"
        },
        {
            "keyHash": "622be5fab3b5c3f371a50a535e4d3349c942a98cecee93b24e2fd11d",
            "type": "sig"
        }
    ],
    "type": "any"
}
```

## atLeast

The `type` key's value `atLeast` indicates that in order to spend this tx output, at least 2 corresponding signatures from the list of payment key hashes are required. The `required` key indicates the minimum number of signatures needed in order to spend the tx output.

```json
{
    "scripts": [
        {
            "keyHash": "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413",
            "type": "sig"
        },
        {
            "keyHash": "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614",
            "type": "sig"
        },
        {
            "keyHash": "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538",
            "type": "sig"
        },
        {
            "keyHash": "686024aecb5884d73a11b9ae4e63931112ba737e878d74638b78513a",
            "type": "sig"
        }
    ],
    "required": 2,
    "type": "atLeast"
}
```

### Example of using multi signature scripts

Below is an example that specifies how to use a multi signature script. This is a step-by-step process involving:

+ the creation of a multi signature address
+ sending ada to that address
+ gathering required witnesses in order to spend ada from the multi signature address.

The process is an example based on using the `all` script.

#### Sending ada to a script address

#### Step 1 - create a multi signature script

First, generate the keys that you require witnessing from using the `cardano-cli shelley address key-gen` command. Then, construct a multi signature script as described above. For this example, we will describe the process using an `all` multisig script (`allMultiSigScript`) as follows:

```json
{
    "scripts": [
        {
            "keyHash": "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a",
            "type": "sig"
        },
        {
            "keyHash": "a687dcc24e00dd3caafbeb5e68f97ca8ef269cb6fe971345eb951756",
            "type": "sig"
        },
        {
            "keyHash": "0bd1d702b2e6188fe0857a6dc7ffb0675229bab58c86638ffa87ed6d",
            "type": "sig"
        }
    ],
    "type": "all"
}
```

#### Step 2 - create a multi signature address

A multi signature address is required in order to use a multi signature script. Please note that network parameters must match with your personal network’s parameters (if you are on mainnet, use the `--mainnet flag`). Construct this as follows:

```
cardano-cli shelley address build-script
  --script-file allMultiSigScript
  --testnet-magic 42
  --out-file script.addr
```

#### Step 3 - construct and submit a transaction (tx) to the multi signature address

To construct and submit a tx to send ada to the multi signature address, construct the tx body:

```bash
cardano-cli shelley transaction build-raw
    --ttl 1000
    --fee 0
    --tx-in utxoinput
    --tx-out $(< script.addr)+$amount
    --out-file txbody
```

Create the UTxO witness:

```bash
cardano-cli shelley transaction witness
  --tx-body-file txbody
  --signing-key-file utxoSignKey
  --testnet-magic 42
  --out-file utxoWitness
```

Assemble the UTxO witness and the tx body to create the transaction:

```bash
cardano-cli shelley transaction assemble
  --tx-body-file txbody
  --witness-file utxoWitness
  --out-file allWitnessesTx
```

After submitting the above tx, the inputs associated with the multi signature address will be "guarded" by the multi signature script.

### Sending ada from a script address

#### Step 1 - construct the tx body

```bash
cardano-cli shelley transaction build-raw \
    --ttl 1000 \
    --fee 0 \
    --tx-in (txin of script address)
    --tx-out yourspecifiedtxout \
    --out-file spendScriptTxBody
```

#### Step 2 - construct required witnesses

To construct the script witness and three witnesses required by the example `all` script, run the following commands:

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

#### Step 3 - construct and submit the transaction
To construct and submit a transaction, you must assemble it with the script witness and all the required witnesses.

```bash
cardano-cli shelley transaction assemble \
  --tx-body-file spendScriptTxBody \
  --witness-file scriptWitness \
  --witness-file key1witness \
  --witness-file key2witness \
  --witness-file key3witness \
  --out-file spendMultiSig
```
You can now submit this tx via `cardano-cli shelley transaction submit`!
