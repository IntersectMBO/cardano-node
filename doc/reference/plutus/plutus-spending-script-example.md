# Plutus Scripts

## What is a Plutus spending script?

This is a type of Plutus script that is required to validate the spending of a tx output at its own script address. The tx output at the Plutus script address *must* be associated with a datum hash, otherwise, the tx output will be unspendable! The purpose of this datum hash is to encode the state of the contract. A demonstration of this can be seen in [lecture #7](https://youtu.be/oJupInqvJUI) of the Plutus Pioneers Program. The Plutus spending script expects a datum and a redeemer in order to successfully validate the spending of the tx output at its own script address; note that the redeemer is considered to be the user input. These are supplied in the transaction being submitted along with the Plutus script itself and specified transaction execution units.

The transaction execution units are an upper bound or budget of what will be spent to execute the Plutus script. If you don't specify a high enough value to cover script execution, your transaction will still be successful (provided it is a valid tx) but you will lose your collateral. The collateral is the transaction input(s) you specify to be consumed if your script fails to execute. There is a protocol parameter `collateralPercent` that determines what percentage of your inputs you must supply as collateral.

*Note that in order to use a tx input as collateral, it **cannot** reside at a script address; it must reside at a ‘normal’ payment address and it cannot contain any multi-assets.*

### An example of using a Plutus spending script

Below is an example that shows how to use a Plutus spending script. This is a step-by-step
process involving:

+ the creation of the `AlwaysSucceeds` Plutus txin script
+ sending ADA to the Plutus script address
+ spending ADA at the Plutus script address

In this example we will use the [AlwaysSucceeds](../../../plutus-example/plutus-example/src/Cardano/PlutusExample/AlwaysSucceeds.hs) Plutus spending script. In order to execute a Plutus spending script, we require the following:

- Collateral tx input(s) - these are provided and are forfeited in the event the Plutus script fails to execute.
- A Plutus tx output with accompanying datum hash. This is the tx output that sits at the Plutus script address. It must have a datum hash, otherwise, it is unspendable.
- The Plutus script serialized in the text envelope format. `cardano-cli` expects Plutus scripts to be serialized in the text envelope format.

#### Creating the `AlwaysSucceeds` Plutus spending script

The plutus-example executable will automagically generate several Plutus scripts in the CLI-compatible text envelope format.

Run the following commands:

```bash
cd plutus-example

cabal run exe:plutus-example
```

This will output `always-succeeds-txin.plutus` in the `generated-plutus-scripts` dir.

#### Setting up a local Alonzo node cluster

There is a convenient script that will set up an Alonzo cluster immediately on your local machine.

Run the following command:

```bash
cabal install cardano-cli
cabal install cardano-node
./scripts/byron-to-alonzo/mkfiles.sh alonzo
```

Follow the instructions displayed in the terminal to start your Alonzo cluster.

#### Sending ADA to the script address

In order to require a Plutus script to validate the spending of a tx output, we must put the tx output at the script address of the said Plutus script. However, before we do that, we must create a datum hash:

```bash
> cardano-cli transaction hash-script-data --script-data-value 42
> 9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b
```
In this example, the script we are using always succeeds so we can use any datum hash. We calculate the script address as follows:

```bash
> cardano-cli address build --payment-script-file plutus-example/generated-plutus-scripts/always-succeeds-txin.plutus  --testnet-magic 42
> addr_test1wzeqkp6ne3xm6gz39l874va4ujgl4kr0e46pf3ey8xsu3jsgkpcj2
```

Now, we should create the tx that will send ADA to the script address of our `AlwaysSucceeds` script:

```bash
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in $txin \
  --tx-out "addr_test1wzeqkp6ne3xm6gz39l874va4ujgl4kr0e46pf3ey8xsu3jsgkpcj2+$lovelace" \
  --tx-out-datum-hash 9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b \
  --out-file create-datum-output.body

cardano-cli transaction sign \
  --tx-body-file create-datum-output.body \
  --testnet-magic 42 \
  --signing-key-file $UTXO_SKEY \
  --out-file create-datum-output.tx
```

#### Spending ADA at the script address

Now that there is ADA at our script address, we must construct the appropriate transaction in order to spend it.

`$plutusutxotxin` - This is the tx input that sits at the Plutus script address (NB: It has a datum hash).
`$plutusrequiredtime` and `$plutusrequiredspace` - These make up the Plutus script execution budget and are part of the `$txfee`
`tx-in-redeemer-value` - We must also supply a redeemer value even though the Plutus script will succeed regardless of the redeemer.

```bash
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee "$txfee" \
  --tx-in $plutusutxotxin \
  --tx-in-collateral $txinCollateral \
  --tx-out "$dummyaddress+$spendable" \
  --tx-in-script-file $plutusscriptinuse \
  --tx-in-datum-value 42  \
  --protocol-params-file pparams.json\
  --tx-in-redeemer-value 42 \
  --tx-in-execution-units "($plutusrequiredtime, $plutusrequiredspace)" \
  --out-file test-alonzo.body

cardano-cli transaction sign \
  --tx-body-file test-alonzo.body \
  --testnet-magic 42 \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file alonzo.tx
```

If there is ADA at `$dummyaddress` then the Plutus script was successfully executed. Conversely, if the Plutus script failed, the collateral input would have been consumed.

You can use the [example-txin-locking-plutus-script.sh](../../../scripts/plutus/example-txin-locking-plutus-script.sh) in conjunction with [mkfiles.sh alonzo](../../../scripts/byron-to-alonzo/mkfiles.sh) script to automagically run the `AlwaysSucceeds` script.

