# Key Evolving Signature and KES period

To create an operational certificate for a block-producing node, you need a _KES key pair_.

Here "KES" stands for _**K**ey **E**volving **S**ignature_, which means that after a certain _period_, the key will _evolve_ to a new key
and discard its old version. This is useful, because it means that even if an attacker compromises the key and gets access to the signing key,
he can only use that to sign blocks _from now on_, but not blocks dating from _earlier periods_, making it impossible for the attacker to rewrite history.

A KES key can only evolve for a certain number of periods and becomes useless afterwards.
This means that before that number of periods has passed, the node operator has to generate a new KES key pair, issue a new operational node certificate with that new key pair and restart the node with the new certificate.

```
cardano-cli node key-gen-KES \
--verification-key-file kes.vkey \
--signing-key-file kes.skey
```

To find out how long one period is and for how long a key can evolve, we can look into the _genesis file_. If that file is called `mainnet-shelley-genesis.json`,
we can type

```
cat mainnet-shelley-genesis.json | grep KES

"slotsPerKESPeriod": 129600,
"maxKESEvolutions": 62,
```

The key will evolve after each period of 129600 slots and it can evolve up-to 62 times before it needs to be renewed.

Before we can create an operational certificate for our node, we need to figure out the start of the KES validity period, i.e. which KES evolution period we are in.

We check the current tip of the blockchain:

```
cardano-cli query tip --mainnet

{
    "epoch": 259,
    "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
    "slot": 26633911,
    "block": 5580350
}
```
In this example, we are currently in slot 26633911, and we know from the genesis file that one period lasts for 129600 slots. So we calculate the current period:

```
expr 26633911 / 129600
> 205
```
With this we are able to generate an operational certificate for our stake pool:

```
cardano-cli node issue-op-cert \
--kes-verification-key-file kes.vkey \
--cold-signing-key-file cold.skey \
--operational-certificate-issue-counter cold.counter \
--kes-period 205 \
--out-file opcert.cert
```
This command increments the cold.counter by 1.

## :warning: Vasil hard fork breaking changes

Note that from the Vasil hard fork onwards there is a new rule for the cold.counter:

It is only valid for new operational certificates to use the counter value previously used in a block successfully included on the chain) plus *exactly* one. Prior to Vasil, it was permitted to use any strictly larger counter value than used on chain previously, but this is no longer permitted. It must be incremented by exactly one. In particular, this means one cannot use the current time or slot number as an issue number.

### Find the counter value used on-chain

To find out the actual `OpCert` counter used on-chain, run this command:

```
cardano-cli query kes-period-info --testnet-magic 42 --op-cert-file node-spo3/opcert.cert
✓ Operational certificate's KES period is within the correct KES period interval
✓ The operational certificate counter agrees with the node protocol state counter
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

* `"qKesNodeStateOperationalCertificateNumber": 3,` is the last counter registered **on-chian** to forge a block.

* `"qKesOnDiskOperationalCertificateNumber": 3,` is the counter used on your current operational certificate.


**When your pool has already forged a block in the current KES interval these values must match.**

If your pool has **not** yet forged a block in the current KES Intervalit is ok that `cardano-cli query kes-period-info` shows that `"qKesOnDiskOperationalCertificateNumber"` is greater than `"qKesNodeStateOperationalCertificateNumber"` by exactly one (+1), for example:

```
"qKesNodeStateOperationalCertificateNumber": 2,
"qKesOnDiskOperationalCertificateNumber": 3,
```

<<<<<<< Updated upstream
    cardano-cli node issue-op-cert \
    --kes-verification-key-file kes.vkey \
    --cold-signing-key-file cold.skey \
    --operational-certificate-issue-counter cold.counter \
    --kes-period 205 \
    --out-file node.cert
=======
Nothing to worry about, when your pool forges a block `qKesNodeStateOperationalCertificateNumber` will be updated accordingly and these values will be the same.

### How do I know if I have issued an _invalid_ operational certificate

An easy way to know is to `query kes-period-info` right after you have issued the new operational certificate. If the **OnDisk** counter differs from the **OnChain** counter by more than 1, your certificate is _invalid_, for example:

```
"qKesNodeStateOperationalCertificateNumber": 2,
"qKesOnDiskOperationalCertificateNumber": 4,
```

### My pool has not forged a block in the current KES interval and I need to issue new operational certificate. What should I do with the counter?

Keep in mind that the rule is that the counter must be one more than the last counter used to _actually_ forge a block in the _current chain_. So if we have not forged a block during the current KES interval, the counter must not be incremented when issuing a new OpCert.

In this scenario `cardano-cli query kes-period-info` will show that the **OnDisk** counter is greater than the **On-Chain** counter

```
"qKesNodeStateOperationalCertificateNumber": 2,
"qKesOnDiskOperationalCertificateNumber": 3,
```

Therefore, we **cannot** use `cardano-cli node issue-op-cert...` right away to issue a new operational certificate because this would increment the **OnDisk** counter by 1 more, resulting in something like:

```
"qKesNodeStateOperationalCertificateNumber": 2,
"qKesOnDiskOperationalCertificateNumber": 4,
```

If we issue a certificate with such a counter, any block we forge with it will be an **Invalid Block**. The node logs will show the **error**:

```
[CLR:cardano.node.ChainDB:Error:25] [2022-06-30 03:25:37.01 UTC] Invalid block 237602735e6b56985109480aefbc4821f57e6389736be238ffbec4c0188f9702 at slot 5206: ExtValidationErrorHeader (HeaderProtocolError (HardForkValidationErrFromEra S (S (S (S (S (Z (WrapValidationErr {unwrapValidationErr = CounterOverIncrementedOCERT 2 4}))))))))
```

### Recovering after having issued an _invalid_ certificate OR to issue a new certificate when our pool has not forged a block during the current KES interval:

1. Query `kes-period-info`. You can save the json output to a file using the `--out-file` flag

```
cardano-cli query kes-period-info --testnet-magic 42 --op-cert-file node-spo3/opcert.cert --out-file kes_period_info.json

cat kes_period_info.json

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
2. Create a new counter (on your cold environment)

Take the **OnChain** counter from above. In this case `"qKesNodeStateOperationalCertificateNumber": 2` and increment it by exactly 1 when issuing the new counter:

```
cardano-cli node new-counter \
--cold-verification-key-file cold.vkey \
--counter-value $((2 + 1)) \
--operational-certificate-issue-counter-file cold.counter
```
3. Generate new KES keys if needed (because of MaxKESEvolutions reached):

```
cardano-cli node key-gen-KES \
--verification-key-file kes.vkey \
--signing-key-file kes.skey
```
4. Issue the new operational certificate:

```
cardano-cli node issue-op-cert --kes-verification-key-file kes.vkey \
--cold-signing-key-file cold.skey \
--operational-certificate-issue-counter-file cold.counter \
--kes-period 18 \
--out-file "opcert.cert"
```
5. Copy `opcert.cert` and `kes.skey` to your block producer and restart it.
>>>>>>> Stashed changes
