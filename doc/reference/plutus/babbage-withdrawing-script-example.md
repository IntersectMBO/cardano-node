# Babbage reference withdrawing script example

## Babbage reference withdrawing plutus script example

For an overview of reference script usuage in the Babbage era, please see [here](babbage-script-example.md).

In this example we will withdraw our rewards from a plutus script address using a plutus reference script. We must first run the [register-and-delegate-script-staking-address.sh](../../../scripts/babbage/staking-example/register-and-delegate-script-staking-address.sh) script in order to register and delegate the stake at our plutus script address. We will briefly walk through the [claim-script-staking-rewards.sh](../../../scripts/babbage/staking-example/claim-script-staking-rewards.sh) script that automatically does this all for us.

Now that we already have our certifying reference script at a tx output so we can use our reference input again to validate the withdrawal of our rewards at our plutus script address with the following tx:

```bash
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-in "$txin1" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+$totalspendable" \
  --withdrawal "$stakingscriptaddr+$rewardamt" \
  --withdrawal-tx-in-reference "$plutusreferencescripttxin" \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file "scripts/plutus/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-withdrawal.txbody"
```

After this transaction is submitted, if successful, we should have 0 rewards at our plutus staking script address:

```bash
cardano-cli query stake-address-info --testnet-magic 42 --address stake_test17zwga8d8lq0re2gysheja0hunqfhezkzvzs89gqvf2h3gtgtc7vzc

[
    {
        "address": "stake_test17zwga8d8lq0re2gysheja0hunqfhezkzvzs89gqvf2h3gtgtc7vzc",
        "delegation": "pool1dg6f0r6gwqfath8zspxfhcnpjxnqr5q0wjupe62f7st4j2am4x3",
        "rewardAccountBalance": 0
    }
]
```



