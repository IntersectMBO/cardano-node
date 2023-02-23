# Plutus Scripts

## What is a reference script?

A reference script is a script that exists at a particular transaction output. It can be used to witness, for example, a UTxO at the corresponding script address of said reference script. This is useful because the script does not have to be included in the transaction anymore, which significantly reduces the transaction size.

## What is an inline datum?

An inline datum, is a datum that exists at a transaction output. We no longer have to include a datum within our transaction for our plutus spending scripts. Instead we can specify the transaction output where our datum exists to be used in conjunction with our Plutus spending script. This reduces the overall size of our transaction.

## What is a read only reference input

In the case where we are not using a reference input to reference another transaction input (at a Plutus script address), we can specify a read only reference input that is simply exposed in the Plutus script context.

## What are total and return collateral?

The total collateral field lets users write transactions whose collateral is evident by just looking at the tx body instead of requiring information in the UTxO. The specification of total collateral is optional. It does not change how the collateral is computed but transactions whose collateral is different than the amount specified will be invalid.

Return collateral allows us to specify an output with the remainder of our collateral input(s) in the event we overcollateralize our transaction. This allows us to avoid overpaying the collateral.

### An example of using a Plutus V2 reference script

Below is an example that shows how to use a reference Plutus spending script with an inline datum and a reference minting script. Here we discuss a [shell script example of how to use a reference script to spend a tx input and a reference minting script to mint tokens](../../../scripts/babbage/example-babbage-script-usage.sh). This is a step-by-step process involving:

+ the creation of the `Required Redeemer` Plutus txin script
+ the creation of the `Required Redeemer` Plutus script at a transaction output (creation of the reference script)
+ the creation of the inline datum at a transaction output
+ sending ada to the Plutus script address
+ spending ada at the Plutus script address using the Plutus reference script
+ creating a read only reference tx output
+ the creation of the reference [minting script](../../../scripts/plutus/scripts/v2) at a transaction output.

In this example we will use the [Required Redeemer](../../../scripts/plutus/scripts/v2/required-redeemer.plutus) Plutus spending script and a [minting script](../../../scripts/plutus/scripts/v2/minting-script.plutus). In order to execute a reference Plutus spending script, we require the following:

- Collateral tx input(s) - these are provided and are forfeited in the event the Plutus script fails to execute.
- A Plutus tx output. This is the tx output that sits at the Plutus script address.
- The reference transaction input containing the corresponding Plutus script. We must create the transaction output containing the reference Plutus script.
- An inline datum at the Plutus tx output. The Plutus spending script requires an inline datum or datum hash and in this case we are using an inline datum.
- A read only reference input with an inline datum.

#### Creating the `Required Redeemer` Plutus spending script and the minting script

The plutus-example executable will automagically generate several Plutus scripts in the CLI-compatible text envelope format.

Run the following commands:

```bash
git clone git@github.com:input-output-hk/plutus-apps.git

cd plutus-apps/plutus-example

cabal run exe:plutus-example
```

This will output `required-redeemer.plutus` and `minting-script.plutus` in the `generated-plutus-scripts/v2` dir.

#### Setting up a local Babbage node cluster

There is a convenient script that will set up a Babbage cluster immediately on your local machine.

Run the following command:

```bash
cabal install cardano-cli
cabal install cardano-node
./scripts/babbage/mkfiles.sh
```

To start your babbage cluster, you need to run the `example/run/all.sh` shell script.
The remainder of this guide provides a brief walkthrough of the [shell script example](../../../scripts/babbage/example-babbage-script-usage.sh) that automatically creates a reference script and spends the utxo at
the reference script's corresponding script address.

#### Creating a reference script at a transaction output, inline datum and
#### sending ada to the script address (with a datum)

In order to use a reference script, we must first create this script at a particular transaction output.

```bash
cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 42 \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$readonlyaddress+$lovelaceattxindiv6" \
  --tx-out-reference-script-file "$plutusstakescript" \
  --tx-out-inline-datum-file "$datumfilepath" \
  --tx-out "$utxoaddr+$lovelace" \
  --tx-out "$plutusspendingscriptaddr+$lovelace" \
  --tx-out-inline-datum-file "$datumfilepath" \
  --tx-out "$dummyaddress+$lovelaceattxindiv6" \
  --tx-out-reference-script-file "$plutusspendingscript" \
  --tx-out "$addressformintingrefscript+$lovelaceattxindiv6" \
  --tx-out-reference-script-file "$plutusmintingscript" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/create-datum-output.body"
```

The following should be noted about this build command:

Firstly, we are sending ada and an inline datum to the plutus script address. This is reflected in the following lines:

```bash
...
--tx-out "$plutusspendingscriptaddr+$lovelace" \
--tx-out-inline-datum-file "$datumfilepath" \
...
```

We have seen this before in the [plutus-spending-script-example.md](plutus-spending-script-example.md).

Secondly, we are creating a reference script at a tx output:

```bash
...
--tx-out "$dummyaddress+$lovelaceattxindiv6" \
--tx-out-reference-script-file "$plutusspendingscript" \
...
```

Specifying the `--reference-script-file` after the `--tx-out` option will construct a transaction that creates a reference script at that transaction output.

Thirdly, we are preparing a txout to be used as a read only reference input:

```bash
...
--tx-out "$readonlyaddress+$lovelaceattxindiv6" \
--tx-out-inline-datum-file "$datumfilepath" \
...
```

Fourthly, we create the minting reference script at a tx output:

```bash
...
--tx-out "$addressformintingrefscript+$lovelaceattxindiv6" \
--tx-out-reference-script-file "$plutusmintingscript" \
...
```

We sign and then submit as usual:

```bash
cardano-cli transaction sign \
  --tx-body-file $WORK/create-datum-output.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file $UTXO_SKEY \
  --out-file $WORK/create-datum-output.tx
```


#### Spending ada at the script address and minting tokens

Now that there is ada at our script address, we must construct the appropriate transaction in order to spend it.
Because we are using the `build` command, we should only note the following:

`$plutusutxotxin` - This is the tx input that sits at the Plutus script address (NB: It has a datum hash).
`spending-tx-in-reference` - This specifies the reference input you are using to witness a transaction input.
`spending-plutus-script-v2`- This specifies the version of the reference script at the reference input.
`spending-reference-tx-in-inline-datum-present` - This indicates that we are using an inline datum which exists at the utxo we are trying to spend (the utxo at the Plutus script address).
`spending-reference-tx-in-redeemer-file` - This is the redeemer to be used with the reference script.
`read-only-tx-in-reference` - This is a non-witnessing reference input. This will only be exposed in the Plutus script context.
`mint-tx-in-reference` - This specifies the reference input you are using to mint tokens.
`mint-plutus-script-v2` - This specifies the version of the reference script at the reference input.
`mint-reference-tx-in-redeemer-file` - This is the redeemer to be used with the reference script.
`policy-id` - Because we do not have direct access to the minting script we must specify the policy id.
`tx-total-collateral` - This is the total required collateral for our transaction. This can be computed by multiplying the `collateralPercentage` and the transaction fee.
`tx-out-return-collateral` - If our collateral inputs over collateralize our transaction, we can return the excess to ourselves via this cli option.

```bash
cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --read-only-tx-in-reference "$readonlyrefinput" \
  --tx-in "$txin1" \
  --tx-in-collateral "$txinCollateral" \
  --tx-total-collateral 529503 \
  --tx-out-return-collateral "$utxoaddr+$returncollateral" \
  --out-file "$WORK/test-alonzo-ref-script.body" \
  --tx-in "$plutuslockedutxotxin" \
  --spending-tx-in-reference "$plutusreferencescripttxin" \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file "$redeemerfilepath" \
  --mint "5 $mintpolicyid.4D696C6C6172436F696E" \
  --mint-tx-in-reference "$mintingscriptrefinput" \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file "$redeemerfilepath" \
  --policy-id "$mintpolicyid" \
  --tx-out "$dummyaddress2+10000000 + 5 $mintpolicyid.4D696C6C6172436F696E" \
  --protocol-params-file "$WORK/pparams.json"

cardano-cli transaction sign \
  --tx-body-file $WORK/test-alonzo-ref-script.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/alonzo-ref-script.tx
```

If there is ada at `$dummyaddress2`, then the Plutus script was successfully executed.


