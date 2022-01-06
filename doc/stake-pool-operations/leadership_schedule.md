## Leadership Schedule


### Get the leadership schedule for the current epoch and the next epoch

You can calculate the leadership schedule for your stake pool with:
- Shelley genesis file
- Stake pool ID
- VRF signing key

Specifying the `--current` flag will yield the leadership schedule for the current epoch as follows:

```bash
> cardano-cli query leadership-schedule \
    --testnet-magic 42 \
    --genesis example/shelley/genesis.json \
    --stake-pool-id  pool12t0y7agkqct89pf00eeytkvfjlquv76tjy27duannan9w63ckxv \
    --vrf-signing-key-file example/node-pool1/shelley/vrf.skey
    --current

>
     SlotNo                          UTC Time
-------------------------------------------------------------
     4073                   2021-12-29 17:26:54.998001755 UTC
     4126                   2021-12-29 17:27:00.298001755 UTC
     4206                   2021-12-29 17:27:08.298001755 UTC
     4256                   2021-12-29 17:27:13.298001755 UTC
     4309                   2021-12-29 17:27:18.598001755 UTC
     4376                   2021-12-29 17:27:25.298001755 UTC
     4423                   2021-12-29 17:27:29.998001755 UTC
     4433                   2021-12-29 17:27:30.998001755 UTC
```
