# Plutus Minting Scripts

## What is a Plutus minting script?

A Plutus minting script is required to validate the minting of multi-asset tokens. Unlike Mary-era scripts, Plutus scripts support more logic beyond required signatures and timelocks.

### An example of using a Plutus minting script

Below is an example that shows how to use a Plutus minting script. This is a step-by-step
process involving:

+ the creation of the `AlwaysSucceeds` Plutus minting script (i.e. anybody can mint)
+ the creation of a transaction that mints multi-assets using the `AlwaysSucceeds` Plutus minting script

In this example we will use the [anyone can mint](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-example/src/PlutusExample/PlutusVersion1/MintingScript.hs) Plutus minting script. To execute it, we require the following:

- Collateral tx input(s) that are provided and forfeited if the Plutus script fails to execute.
- Serialization of the Plutus script in the text envelope format (required for cardano-cli).
- A redeemer.

#### Creating the `AlwaysSucceeds` Plutus minting script

The plutus-example executable will automagically generate several Plutus scripts in the CLI-compatible text envelope format.

Run the following commands:

```bash
cd plutus-example

cabal run exe:plutus-example
```

This will output `anyone-can-mint.plutus` in the `generated-plutus-scripts` dir.

#### Setting up a local Alonzo node cluster

First follow the [install doc](../../../doc/getting-started/install.md) if you haven't. This convenient script will set up an Alonzo cluster immediately on your local machine:

```bash
./scripts/byron-to-alonzo/mkfiles.sh alonzo
```

Follow the instructions displayed in the terminal to start your Alonzo cluster.

#### Minting multi-assets using the Plutus minting script

To mint a multi-asset, you need the Plutus script policy ID and a redeemer. Because this minting script always succeeds, you can use any redeemer:

```
$ cardano-cli transaction policyid --script-file scripts/plutus/scripts/anyone-can-mint.plutus
> $policyid
```

You can find an example redeemer at: `scripts/plutus/data/42.redeemer`

For more information regarding `tx-in-collateral` see [here](plutus-spending-script-example.md).

```bash
cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --tx-in "$txin" \
  --tx-in-collateral "$txinCollateral" \
  --mint "5 $policyid.4D696C6C6172436F696E0A" \
  --mint-script-file "scripts/plutus/scripts/anyone-can-mint.plutus" \
  --mint-redeemer-file "scripts/plutus/data/42.redeemer" \
  --tx-out "$dummyaddress+$spendable + 5 $policyid.4D696C6C6172436F696E0A" \
  --protocol-params-file pparams.json \
  --out-file "plutusmint.body"

cardano-cli transaction sign \
  --tx-body-file "plutusmint.body" \
  --testnet-magic 42 \
  --signing-key-file "$skey" \
  --out-file "plutusmint.tx"
```

You can use the [simple-minting-policy.sh](../../../scripts/plutus/simple-minting-policy.sh) in conjunction with the [mkfiles.sh alonzo](../../../scripts/byron-to-alonzo/mkfiles.sh) script to automagically run the `AlwaysSucceeds` minting script.


