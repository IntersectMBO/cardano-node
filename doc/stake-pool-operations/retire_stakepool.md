# Retiring a Stake Pool

To retire a pool we need to:

0. Make sure that any pool owners are aware of your retirement intention and will not remove their Pledge until the pool has retired. 
1. Create a **deregistration certificate** and specify an epoch when your pool will retire. There is a limit on how far in the future this can be (given by a protocol parameter - see below). Make sure you give sufficient time for delegators to switch pools
2. Submit the certificate to the blockchain with a **transaction**
3. Notify your delegators of your intention to retire the pool using your usual communication mechanisms. Wallets and other tools should highlight that the pool is retiring and discourage new delegations.
4. Keep your system working normally until we reach the epoch you chose on the certificate for the pool to expire. Retirement takes place when the first block of the selected epoch is processed (before the transactions for that block are processed.) 
5. Move your funds out from your pledge address to a different wallet.  
6. When the pool is effectively retired, the pool deposit is sent back to the rewards account.
7. **Wait for any pending rewards to be paid.**
8. Withdraw any rewards and pool deposit from the rewards account.
9. **Once you have successfully withdrawn your rewards and deposit, de-register the stake key**

**IMPORTANT:**
**Do NOT deregister your stake key before the pool deposit is paid back to your rewards account or your pool deposit will have no place to go and will be sent to the treasury.**

The deregistration certificate contains the _epoch_ in which we want to retire the pool. This epoch must be _after_ the current epoch and _not later than_ `eMax` epochs in the future, where `eMax` is a protocol parameter.

First, you need to figure out the current epoch. The simplest way to get the current epoch is to use the `tip` command:

```bash
> cardano-cli query tip --testnet-magic 42 --cardano-mode
{
  "epoch": 51,
  "hash": "19fdfc54e7a1e2fb7ff45254a6aa0aa88ea7a80de4b48297924c9abf353f9cb7",
  "slot": 21592,
  "block": 1123022
}
```

You can also get the current epoch if you are monitoring a node with [EKG](../logging-monitoring/ekg.md):

```
curl -s -m 3 -H 'Accept: application/json' http://127.0.0.1:12788/ | jq '.cardano.node.metrics.epoch.int.val'
> 51
```

So we are currently in epoch 51.

We can look up `eMax` by querying the current protocol parameters:

    cardano-cli query protocol-parameters \
    --mainnet \
    --out-file protocol.json

    cat protocol.json | grep eMax
    > "eMax": 18,

This means the earliest epoch for retirement is 52 (one in the future), and the latest is 69 (current epoch plus `eMax`).

For example, we can decide to retire in epoch 53.

#### Create deregistration certificate

**WARNING** This involves the __cold keys__. Take the necessary precautions to not expose your cold keys to the internet.

Create the deregistration certificate and save it as `pool-deregistration.cert`:

    cardano-cli stake-pool deregistration-certificate \
    --cold-verification-key-file cold.vkey \
    --epoch 53 \
    --out-file pool-deregistration.cert

#### Draft the transaction

    cardano-cli transaction build-raw \
    --tx-in <TxHash>#<TxIx> \
    --tx-out $(cat payment.addr)+0 \
    --invalid-hereafter 0 \
    --fee 0 \
    --out-file tx.draft \
    --certificate-file pool-deregistration.cert

#### Calculate the fees:

    cardano-cli transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 2 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

For example:

    > 171309

We query our address for a suitable UTxO to use as input:

    cardano-cli query utxo \
    --address $(cat payment.addr) \
    --mainnet



           TxHash             TxIx      Amount
    ------------------------------------------------
    9db6cf...                    0      999999267766 lovelace

We calculate our change:

    expr 999999267766 - 171309
    > 999999096457

#### Build, sign and submit the transaction:

Build the raw transaction:

    cardano-cli transaction build-raw \
    --tx-in 9db6cf...#0 \
    --tx-out $(cat payment.addr)+999999096457 \
    --invalid-hereafter 860000 \
    --fee 171309 \
    --out-file tx.raw \
    --certificate-file pool-deregistration.cert

**Sign it with both the payment signing key and the cold signing key
(the first signature is necessary because we are spending funds from `payment.addr`,
the second because the certificate needs to be signed by the pool owner):**

    cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file cold.skey \
    --mainnet \
    --out-file tx.signed

And submit to the blockchain:

    cardano-cli transaction submit \
    --tx-file tx.signed \
    --mainnet

The pool will retire at the end of epoch 52.

If we change our mind, we can create and submit a new registration certificate before epoch 53, which will then overrule the deregistration certificate.
