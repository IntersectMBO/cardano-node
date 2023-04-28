# Operational certificate and key evolving signature

To create an operational certificate for a block-producing node, you need a _KES key pair_.

Here 'KES' stands for _**K**ey **E**volving **S**ignature_, which means that after a certain _period_, the key will _evolve_ to a new key
and discard its old version. This is useful because it means that even if an attacker compromises the key and gets access to the signing key, he can only use that to sign blocks _from now on_, but not blocks dating from _earlier periods_, making it impossible for the attacker to rewrite the history. See how to generate a new KES key pair below.

A KES key can only evolve for a certain number of periods and becomes useless afterwards.
This means that before that number of periods has passed, the node operator has to generate a new KES key pair, issue a new operational node certificate with that new key pair and restart the node with the new certificate.

### KES key generation

```bash
cardano-cli node key-gen-KES \
  --verification-key-file kes.vkey \
  --signing-key-file kes.skey
```

To find out how long one period lasts and for how long a key can evolve, we can look into the _genesis file_. If that file is called `mainnet-shelley-genesis.json`,
we can type:

```bash
cat mainnet-shelley-genesis.json | grep KES
"slotsPerKESPeriod": 129600,
"maxKESEvolutions": 62,
```

The key will evolve after each period of 129600 slots and it can evolve up to 62 times before it needs to be renewed.

Before creating an operational certificate for the node, we need to figure out the start of the KES validity period (ie, which KES evolution period we are in).

Check the current tip of the blockchain:

```bash
cardano-cli query tip --mainnet
{
    "epoch": 259,
    "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
    "slot": 26633911,
    "block": 5580350
}
```

In this example, we are currently in slot 26633911, and we know from the genesis file that one period lasts for 129600 slots. Calculate the current period:

```bash
expr 26633911 / 129600
> 205
```

### Issuing an operational certificate

```bash
cardano-cli node issue-op-cert \
  --kes-verification-key-file kes.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter opcert.counter \
  --kes-period 205 \
  --out-file opcert.cert
```

**This command increments the cold counter by 1.**

## :warning: Vasil hard fork breaking changes

Note that from the Vasil hard fork onwards there is a new rule for the opcert.counter:

A new operational certificate will be considered valid only if its counter value is exactly one more than the previous operational certificate that has successfully minted at least one block.

Prior to Vasil, it was permitted to use any strictly larger counter value than used on the chain previously, but this is no longer permitted. It must be incremented by exactly one. In particular, this means one cannot use the current time or slot number as a counter value.

### Find the counter value used on-chain

You can find the operational certificate counter that is currently registered on the chain for _any_ stake pool:

```bash
cardano-cli query  protocol-state --testnet-magic 42 | grep <hex_pool_id>

> <hex_pool_id>: 3,
```
The output is a map **pool_id** to **opcert_counter**. So in this example the current operational certificate counter registerd on-chain is **3**. 

In addition, as a stake pool owner/operator, you have the `cardano-cli query kes-period-info` command, which requires you to provide the operational certificate that you want to query. This provides you with more details:


```bash
> cardano-cli query kes-period-info --testnet-magic 42 --op-cert-file node-spo3/opcert.cert
```

```
✓ Operational certificate's KES period is within the correct KES period interval
✓ The operational certificate counter agrees with the node protocol state operational certificate
{
    "qKesCurrentKesPeriod": 15,
    "qKesEndKesInterval": 18,
    "qKesKesKeyExpiry": null,
    "qKesMaxKESEvolutions": 6,
    "qKesNodeStateOperationalCertificateNumber": 3,
    "qKesOnDiskOperationalCertificateNumber": 3,
    "qKesRemainingSlotsInKesPeriod": 690,
    "qKesSlotsPerKesPeriod": 300,
    "qKesStartKesInterval": 12
}
```

Where:

* `"qKesNodeStateOperationalCertificateNumber": 3,` is the last counter registered **OnChain** to forge a block.

and

* `"qKesOnDiskOperationalCertificateNumber": 3,` is the counter value of your **OnDisk** operational certificate.

When your pool has already forged a block with the current operational certificate the above values will match.

If your pool has **not** yet forged a block with the current operational certificate,  `cardano-cli query kes-period-info` shows that `"qKesOnDiskOperationalCertificateNumber"` is greater than `"qKesNodeStateOperationalCertificateNumber"` by exactly one (1), for example:

```json
"qKesNodeStateOperationalCertificateNumber": 2,
"qKesOnDiskOperationalCertificateNumber": 3,
```

Once your pool forges a block, `qKesNodeStateOperationalCertificateNumber` will be updated accordingly **(OnChain)** and these values will be the same.

### How do I know if I have issued an _invalid_ operational certificate

Run `cardano-cli query kes-period-info` right after you have created the new operational certificate. If the **OnDisk** counter differs from the **OnChain** counter by more than 1, your certificate will be _invalid_, for example:

```json
"qKesNodeStateOperationalCertificateNumber": 2,
"qKesOnDiskOperationalCertificateNumber": 4,
```

Any blocks forged with such a certificate will be _invalid blocks_.

Issuing a **valid certificate** after having issued an **invalid** one requires a new **operational certificate counter** and a new operatinal certificate.


### No blocks forged with the current operational certificate. What to do with the counter?

Keep in mind that the new rule is that the counter must be **one** more than the last counter used to _actually_ forge a block in the _current chain_.

**If we have incremented the counter on a previous operational certificate but we have NOT forged a block with it, the counter must not be incremented again when issuing a new operational certificate (ie, it remains the same). In this case, we need to issue a new counter before issuing the new operational certificate.**

**See: Issue new counter and operational certificate below.**

Note that in this scenario we **cannot** use `cardano-cli node issue-op-cert...` right away to issue a new operational certificate because this would increment the **OnDisk** counter by 1 more, resulting in an invalid certificate, for example:

```
"qKesNodeStateOperationalCertificateNumber": 2,
"qKesOnDiskOperationalCertificateNumber": 4,
```

If we issue a certificate with such a counter, any block we forge with it will be an **invalid block**. The node logs will show the error:

```
Invalid block 237602735e6b56985109480aefbc4821f57e6389736be238ffbec4c0188f9702 at slot 5206: ExtValidationErrorHeader (HeaderProtocolError (HardForkValidationErrFromEra S (S (S (S (S (Z (WrapValidationErr {unwrapValidationErr = CounterOverIncrementedOCERT 2 4}))))))))
```

### Issue new counter and operational certificate

**On a running node:**

1. Query `kes-period-info`. You can save the json output to a file using the `--out-file` flag

```bash
> cardano-cli query kes-period-info --testnet-magic 42 --op-cert-file node-spo3/opcert.cert --out-file kes_period_info.json

> cat kes_period_info.json

>
{
    "qKesCurrentKesPeriod": 18,
    "qKesEndKesInterval": 18,
    "qKesKesKeyExpiry": null,
    "qKesMaxKESEvolutions": 6,
    "qKesNodeStateOperationalCertificateNumber": 2,
    "qKesOnDiskOperationalCertificateNumber": 4,
    "qKesRemainingSlotsInKesPeriod": 198,
    "qKesSlotsPerKesPeriod": 300,
    "qKesStartKesInterval": 12
}
```

**On your cold environment:**

2. Create a new counter

Take the **OnChain** counter from above. In this case `"qKesNodeStateOperationalCertificateNumber": 2` and increment it by exactly 1 when issuing the new counter:

```bash
cardano-cli node new-counter \
  --cold-verification-key-file cold.vkey \
  --counter-value $((2 + 1)) \
  --operational-certificate-issue-counter-file opcert.counter
```

3. Generate new KES keys if needed (because of _MaxKESEvolutions_ reached):

```bash
cardano-cli node key-gen-KES \
  --verification-key-file kes.vkey \
  --signing-key-file kes.skey
```

4. Issue the new operational certificate:

```bash
  cardano-cli node issue-op-cert --kes-verification-key-file kes.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter-file opcert.counter \
  --kes-period 18 \
  --out-file opcert.cert
```

5. Copy the new `opcert.cert` and `kes.skey` to your block producer node and restart it.
