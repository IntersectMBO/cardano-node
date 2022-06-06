# Plutus Scripts

## What is a reference script?

A reference script is a script that exists at a particular transaction output. It can be used to witness, for example, a UTxO at the corresponding script address of said reference script. This is useful because the script does not have to be included in the transaction anymore, which significantly reduces the transaction size.

### An example of using a Plutus V2 reference script

Below is an example that shows how to use a Plutus spending script. Here we discuss a [shell script example of how to use a reference script to spend a tx input](scripts/plutus/example-reference-script-usage.sh). This is a step-by-step process involving:

+ the creation of the `Required Redeemer` Plutus txin script
+ the creation of the `Required Redeemer` Plutus script at a transaction output (creation of the reference script)
+ sending ada and a datum to the Plutus script address
+ spending ada at the Plutus script address using the Plutus reference script

In this example we will use the [Required Redeemer](scripts/plutus/scripts/v2/required-redeemer.plutus) Plutus spending script. In order to execute a reference Plutus spending script, we require the following:

- Collateral tx input(s) - these are provided and are forfeited in the event the Plutus script fails to execute.
- A Plutus tx output with accompanying datum hash. This is the tx output that sits at the Plutus script address. It must have a datum hash, otherwise, it is unspendable.
- The reference transaction input containing the corresponding Plutus script. We must create the transaction output containing the reference Plutus script.

#### Creating the `Required Redeemer` Plutus spending script

The plutus-example executable will automagically generate several Plutus scripts in the CLI-compatible text envelope format.

Run the following commands:

```bash
git clone git@github.com:input-output-hk/plutus-apps.git

cd plutus-apps/plutus-example

cabal run exe:plutus-example
```

This will output `required-redeemer.plutus` in the `generated-plutus-scripts/v2` dir.

#### Setting up a local Babbage node cluster

There is a convenient script that will set up a Babbage cluster immediately on your local machine.

Run the following command:

```bash
cabal install cardano-cli
cabal install cardano-node
./scripts/babbage/mkfiles.sh
```

To start your babbage cluster, you need to run the `example/run/all.sh` shell script.
The remainder of this guide provides a brief walkthrough of the [shell script example](scripts/plutus/example-reference-script-usage.sh) that automatically creates a reference script and spends the utxo at
the reference script's corresponding script address.

#### Creating a reference script at a transaction output and
#### sending ada to the script address (with a datum)

In order to use a reference script, we must first create this script at a particular transaction output.

```bash
cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 42 \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$utxoaddr+$lovelace" \
  --tx-out "$plutusscriptaddr+$lovelace" \
  --tx-out-datum-hash "$scriptdatumhash" \
  --tx-out "$dummyaddress+$lovelaceattxindiv3" \
  --tx-out-reference-script-file "$plutusscriptinuse" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/create-datum-output.body"
```

The following should be noted about this build command:

Firstly, we are sending ada to the plutus script address along with a datum hash. This is reflected in the following lines:

```bash
...
--tx-out "$plutusscriptaddr+$lovelace" \
--tx-out-datum-hash "$scriptdatumhash" \
...
```

We have seen this before in the [plutus-spending-script-example.md](doc/reference/plutus/plutus-spending-script-example.md).

Secondly, we are creating a reference script at a tx output:

```bash
...
--tx-out "$dummyaddress+$lovelaceattxindiv3" \
--tx-out-reference-script-file "$plutusscriptinuse" \
...
```

Specifying the `--reference-script-file` after the `--tx-out` option will construct a transaction that creates a reference script at that transaction output.

We sign and then submit as usual:

```bash
cardano-cli transaction sign \
  --tx-body-file $WORK/create-datum-output.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file $UTXO_SKEY \
  --out-file $WORK/create-datum-output.tx
```


#### Spending ada at the script address

Now that there is ada at our script address, we must construct the appropriate transaction in order to spend it.
Because we are using the `build` command, we should only note the following:

`$plutusutxotxin` - This is the tx input that sits at the Plutus script address (NB: It has a datum hash).
`tx-in-reference` - This specifies the reference input you are using to witness a transaction input.
`plutus-script-v2`- This specifies the version of the reference script at the reference input.
`reference-tx-in-datum-file` - This is the datum to be used with the reference script.
`reference-tx-in-redeemer-file` - This is the redeemer to be used with the reference script.

```bash
cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin1" \
  --tx-in-collateral "$txinCollateral" \
  --out-file "$WORK/test-alonzo-ref-script.body" \
  --tx-in "$plutuslockedutxotxin" \
  --tx-in-reference "$plutusreferencescripttxin" \
  --plutus-script-v2 \
  --reference-tx-in-datum-file "$datumfilepath"  \
  --reference-tx-in-redeemer-file "$redeemerfilepath" \
  --tx-out "$dummyaddress2+10000000" \
  --protocol-params-file "$WORK/pparams.json"

cardano-cli transaction sign \
  --tx-body-file $WORK/test-alonzo-ref-script.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/alonzo-ref-script.tx
```

If there is ada at `$dummyaddress2`, then the Plutus script was successfully executed. Conversely, if the Plutus script failed, the collateral input would have been consumed.

You can use the [example-txin-locking-plutus-script.sh](../../../scripts/plutus/example-txin-locking-plutus-script.sh) in conjunction with [mkfiles.sh alonzo](../../../scripts/byron-to-alonzo/mkfiles.sh) script to automagically run the `AlwaysSucceeds` script.

