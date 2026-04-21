Used to create and cache the dataset, what is passed to `create-testnet-data` arguments `--spec-shelley`, `--spec-alonzo` and `--spec-conway`.

```
cardano-cli latest genesis create-testnet-data --spec-shelley genesis/shelley-genesis.json --spec-alonzo genesis/alonzo-genesis.json --spec-conway genesis/conway-genesis.json --genesis-keys 0 --pools 0 --stake-delegators 0 --committee-keys 0 --drep-keys 0 --stuffed-utxo 0 --utxo-keys 0 --total-supply 0 --delegated-supply 0 --testnet-magic 42 --start-time 1970-01-01T00:00:00Z
```

# What each parameter controls

## Always-created files (regardless of parameters):

- `shelley-genesis.json`
- `byron-genesis.json`
- `byron.genesis.spec.json`
- `alonzo-genesis.json`
- `conway-genesis.json`
- `dijkstra-genesis.json`
- `byron-gen-command/` (directory, always present)

## When each parameter takes effect

| Parameter            | Activation                                                                |
|----------------------|---------------------------------------------------------------------------|
| `--total-supply`     | >= 1                                                                      |
| `--delegated-supply` | >= 1 AND (`utxo-keys >= 1` OR (`pools >= 1` AND `stake-delegators >= 1`)) |
| `--utxo-keys`        | >= 1                                                                      |
| `--pools`            | >= 2 (1 entry always exists by default)                                   |
| `--stake-delegators` | >= 1 (keys always created; JSON fields only when `pools >= 1`)            |
| `--drep-keys`        | >= 1                                                                      |
| `--stuffed-utxo`     | >= 1                                                                      |
| `--genesis-keys`     | (unused by us, always 0)                                                  |
| `--committee-keys`   | (unused by us, always 0)                                                  |
| `--testnet-magic`    | always 42                                                                 |

## What each parameter changes in JSON files

### Always-present, unchanged files

- `alonzo-genesis.json`
- `dijkstra-genesis.json`

### Shelley and Conway

| Parameter            | shelley-genesis.json                                                           | conway-genesis.json |
|----------------------|--------------------------------------------------------------------------------|---------------------|
| `--total-supply`     | `maxLovelaceSupply`, `initialFunds` values                                     | --                  |
| `--delegated-supply` | `initialFunds` values (splits lovelace between utxo and staked entries)        | --                  |
| `--utxo-keys`        | `initialFunds` (adds enterprise address entries)                               | --                  |
| `--stake-delegators` | `initialFunds` (adds staked address entries), `staking.stake`, `staking.pools` | --                  |
| `--drep-keys`        | --                                                                             | `initialDReps`      |
| `--stuffed-utxo`     | `initialFunds` (adds entries with 0 lovelace)                                  | --                  |
| `--testnet-magic`    | `networkMagic`                                                                 | --                  |

## Byron (byron-genesis.json)

Only `--pools` affects `byron-genesis.json`. No other parameter changes it.

The three fields `bootStakeholders`, `heavyDelegation` and `nonAvvmBalances`
always have at least 1 entry (a default BFT entry), even with `--pools 0`.
Setting `--pools 1` produces the same structure as `--pools 0`. Additional
entries appear starting at `--pools 2`.

| pools | `bootStakeholders`        | `heavyDelegation` | `nonAvvmBalances` |
|-------|---------------------------|-------------------|-------------------|
| 0     | 1 entry (weight 1)        | 1 entry           | 1 x 3,000,000,000 |
| 1     | 1 entry (weight 1)        | 1 entry           | 1 x 3,000,000,000 |
| 5     | 5 entries (weight 1 each) | 5 entries         | 5 x 3,000,000,000 |

The value in `nonAvvmBalances` is always **3,000,000,000 per entry**, hardcoded.
It does not change with `--total-supply`, `--delegated-supply`, or any other
parameter. This is Byron-era lovelace in a separate ledger from Shelley's
`initialFunds`.

All cryptographic material in these three fields (key hashes, public keys,
delegation certs, and Byron addresses) is **randomly generated on every run**.
Two calls with identical parameters will produce different keys and addresses
but the same structure and the same monetary values.

## What files and directories each parameter creates

| Parameter | Directory created | Contents per entry |
|---|---|---|
| `--pools` | `pools-keys/poolN/` | `byron-delegate.key`, `byron-delegation.cert`, `cold.skey`, `cold.vkey`, `kes.skey`, `kes.vkey`, `opcert.cert`, `opcert.counter`, `staking-reward.skey`, `staking-reward.vkey`, `vrf.skey`, `vrf.vkey` (12 files) |
| `--stake-delegators` | `stake-delegators/delegatorN/` | `payment.skey`, `payment.vkey`, `staking.skey`, `staking.vkey` (4 files) |
| `--drep-keys` | `drep-keys/drepN/` | `drep.skey`, `drep.vkey` (2 files) |
| `--utxo-keys` | `utxo-keys/utxoN/` | `utxo.skey`, `utxo.vkey` (2 files) |
| `--pools` | `byron-gen-command/` | `genesis-keys.NNN.key` (1 per pool) |
| (always) | `byron-gen-command/` | When pools=0: `delegate-keys.000.key`, `delegation-cert.000.json`, `genesis-keys.000.key` (default BFT entry) |

# Funds distribution

`--total-supply` sets `maxLovelaceSupply` in shelley-genesis.json. The actual
lovelace distributed in `initialFunds` is less: 10% of each supply slice goes to
Shelley reserves (the pot from which staking rewards are paid).

```
reserves = total-supply - utxo_total - staked_total
```

## `--utxo-keys`

```
non_delegated = total-supply - delegated-supply
utxo_total = non_delegated - floor(non_delegated / 10)
utxo_each = floor(utxo_total / utxo-keys)
```

## `--staked-delegators`

```
staked_total = delegated-supply - floor(delegated-supply / 10)
staked_each = floor(staked_total / stake-delegators)
```

UTxO key addresses use enterprise address format (prefix `60`).
Staked delegator addresses use staked address format (prefix `00`).

`--delegated-supply` controls how the supply is split between these two groups.
Without it (or set to 0), the full supply (minus reserves) goes to UTxO keys.
It requires `--utxo-keys >= 1` or (`--pools >= 1` AND `--stake-delegators >= 1`)
to have any visible effect.

`staking.pools` in shelley-genesis.json = min(pools, stake-delegators).
`staking.stake` = stake-delegators (when pools >= 1), otherwise 0.

Example with `--total-supply 1000000 --delegated-supply 500000` (`maxLovelaceSupply` = 1,000,000):

| utxo-keys | pools | stake | utxo total | staked total | initialFunds | utxo each | staked each |
|-----------|-------|-------|------------|--------------|--------------|-----------|-------------|
|         0 |     0 |     0 |          0 |            0 |            0 |        -- |          -- |
|         0 |     0 |     1 |          0 |            0 |            0 |        -- |          -- |
|         0 |     0 |    10 |          0 |            0 |            0 |        -- |          -- |
|         0 |     1 |     0 |          0 |            0 |            0 |        -- |          -- |
|         0 |     1 |     1 |          0 |      450,000 |      450,000 |        -- |     450,000 |
|         0 |     1 |    10 |          0 |      450,000 |      450,000 |        -- |      45,000 |
|         0 |     5 |     0 |          0 |            0 |            0 |        -- |          -- |
|         0 |     5 |     1 |          0 |      450,000 |      450,000 |        -- |     450,000 |
|         0 |     5 |    10 |          0 |      450,000 |      450,000 |        -- |      45,000 |
|         1 |     0 |     0 |    450,000 |            0 |      450,000 |   450,000 |          -- |
|         1 |     0 |     1 |    450,000 |            0 |      450,000 |   450,000 |          -- |
|         1 |     0 |    10 |    450,000 |            0 |      450,000 |   450,000 |          -- |
|         1 |     1 |     0 |    450,000 |            0 |      450,000 |   450,000 |          -- |
|         1 |     1 |     1 |    450,000 |      450,000 |      900,000 |   450,000 |     450,000 |
|         1 |     1 |    10 |    450,000 |      450,000 |      900,000 |   450,000 |      45,000 |
|         1 |     5 |     0 |    450,000 |            0 |      450,000 |   450,000 |          -- |
|         1 |     5 |     1 |    450,000 |      450,000 |      900,000 |   450,000 |     450,000 |
|         1 |     5 |    10 |    450,000 |      450,000 |      900,000 |   450,000 |      45,000 |
|         2 |     0 |     0 |    450,000 |            0 |      450,000 |   225,000 |          -- |
|         2 |     0 |     1 |    450,000 |            0 |      450,000 |   225,000 |          -- |
|         2 |     0 |    10 |    450,000 |            0 |      450,000 |   225,000 |          -- |
|         2 |     1 |     0 |    450,000 |            0 |      450,000 |   225,000 |          -- |
|         2 |     1 |     1 |    450,000 |      450,000 |      900,000 |   225,000 |     450,000 |
|         2 |     1 |    10 |    450,000 |      450,000 |      900,000 |   225,000 |      45,000 |
|         2 |     5 |     0 |    450,000 |            0 |      450,000 |   225,000 |          -- |
|         2 |     5 |     1 |    450,000 |      450,000 |      900,000 |   225,000 |     450,000 |
|         2 |     5 |    10 |    450,000 |      450,000 |      900,000 |   225,000 |      45,000 |

