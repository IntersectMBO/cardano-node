# Babbage reference certifying script example

## Babbage reference certifying plutus script example

For an overview of reference script usuage in the Babbage era, please see [here](babbage-script-example.md).

In this example we will delegate stake at a plutus script address to an SPO using a plutus reference script. This is achieved when we run the [register-and-delegate-script-staking-address.sh](../../../scripts/babbage/staking-example/register-and-delegate-script-staking-address.sh) script. Below we will outline the key parts

First we create the reference script at a tx output as usual with the following transaction:

```bash
cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --tx-out "$dummyaddress+$lovelaceattxindiv3" \
  --tx-out-reference-script-file "$certifyingscript" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/create-stake-reference-script.body"
```

Now that we have out certifying reference script at a tx output and we have funded the script address in question. We then register our script stake address as usual. We can now use our reference input to validate the delegation of our stake that exists at our script address with the following tx:

```bash
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated3" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --witness-override 3 \
  --certificate-file "$WORK/script.delegcert" \
  --certificate-tx-in-reference "$plutusreferencescripttxin" \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file "scripts/plutus/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-delegation-cert.txbody"
```

Once this transaction is successfully submitted, we must wait 2 epochs until rewards begin to accumulate at the script staking address. The shell script helpfully output the cli command needed to check rewards at the staking address. The output will eventually look something like the following:

```bash
cardano-cli query stake-address-info --testnet-magic 42 --address stake_test17zwga8d8lq0re2gysheja0hunqfhezkzvzs89gqvf2h3gtgtc7vzc

[
    {
        "address": "stake_test17zwga8d8lq0re2gysheja0hunqfhezkzvzs89gqvf2h3gtgtc7vzc",
        "delegation": "pool1dg6f0r6gwqfath8zspxfhcnpjxnqr5q0wjupe62f7st4j2am4x3",
        "rewardAccountBalance": 25142
    }
]
```



