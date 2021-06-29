# Plutus Minting Scripts

## What is a Plutus minting script?

This is a type of Plutus script that is required to validate the minting of multi-asset tokens. We can do this using Mary era scripts, however, Plutus scripts allow us to encode more logic beyond requiring verification keys and timelocks. The Plutus minting script expects only a redeemer in order to successfully validate the minting of a multi-asset.

### An example of using a Plutus minting script

Below is an example that shows how to use a Plutus minting script. This is a step-by-step
process involving:

+ the creation of the `AlwaysSucceeds` Plutus minting script (i.e. anybody can mint)
+ the creation of a transaction that mints multi-assets using the `AlwaysSucceeds` Plutus minting script

In this example we will use the [anyone can mint](../../../plutus-example/plutus-example/src/Cardano/PlutusExample/MintingScript.hs) Plutus minting script. In order to execute a Plutus minting script, we require the following:

- Collateral tx input(s) - these are provided and are forfeited in the event the Plutus script fails to execute.
- The Plutus script should be serialized in the text envelope format. `cardano-cli` expects Plutus scripts to be serialized in the text envelope format.
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

There is a convenient script that will set up an Alonzo cluster immediately on your local machine.

Run the following command:

```bash
cabal install cardano-cli
cabal install cardano-node
./scripts/byron-to-alonzo/mkfiles.sh alonzo
```

Follow the instructions displayed in the terminal to start your Alonzo cluster.

#### Minting multi-assets using the Plutus minting script

We need the policy ID of our Plutus script and a redeemer. In this case, we can use any redeemer because the minting script always succeeds:

```bash
> cardano-cli transaction policyid --script-file scripts/plutus/scripts/anyone-can-mint.plutus
> fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50
```

There is an example redeemer at: `scripts/plutus/data/42.redeemer`

For more information regarding `tx-in-collateral` and `mint-execution-units` see [here](plutus-spending-script-example.md).

```bash
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee "$txfee" \
  --tx-in "$txin" \
  --tx-in-collateral "$txinCollateral" \
  --mint-script-file "scripts/plutus/scripts/anyone-can-mint.plutus" \
  --mint-redeemer-file "scripts/plutus/data/42.redeemer" \
  --mint-execution-units "($plutusrequiredspace, $plutusrequiredtime)" \
  --tx-out "$dummyaddress+$spendable + 5 2dce00a8d52ccd0c53be5165dd7a7e8e1d08d87f05f8f91047ca5d0b.4D696C6C6172436F696E0A" \
  --mint "5 2dce00a8d52ccd0c53be5165dd7a7e8e1d08d87f05f8f91047ca5d0b.4D696C6C6172436F696E0A" \
  --protocol-params-file pparams.json \
  --out-file "plutusmint.body"

cardano-cli transaction sign \
  --tx-body-file "plutusmint.body" \
  --testnet-magic 42 \
  --signing-key-file "$skey" \
  --out-file "plutusmint.tx"
```

You can use the [simple-minting-policy.sh](../../../scripts/plutus/simple-minting-policy.sh) in conjunction with the [mkfiles.sh alonzo](../../../scripts/byron-to-alonzo/mkfiles.sh) script to automagically run the `AlwaysSucceeds` minting script.


