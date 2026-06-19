# 1. Overview

`genesis/` holds four hand-derived minimal-valid spec files
(`shelley.json`, `alonzo.json`, `conway.json`, `dijkstra.json`): the
base-case shapes that the relevant parsers still accept, with every value the
parser tolerates driven down to its "zero" (`0`, `""`, `{}`, `[]`, default). The
intent is to neutralise the *data* the specs carry, not to alter their
*structure*. No `cardano-cli` invocation reproduces them.

The mainnet originals they were derived from live in `genesis/base/` (see
`genesis/base/README.md` for sources, including the pre-mainnet placeholder for
`dijkstra-genesis.json` while Dijkstra has not activated).

Section 3 below describes what each `create-testnet-data` flag does relative to
this zero baseline.

# 2. How the genesis ripper backend uses it

The ripper (`nix/workbench/genesis/genesis-ripper.sh`) splits each genesis file
into two orthogonal halves and caches them separately:

- **Dataset**: the heavy fields driven by `--pools`, `--stake-delegators`,
  `--total-supply`, ... (`initialFunds`, `staking`, `genDelegs`,
  `bootStakeholders`, `initialDReps`, `delegs`, ...). Cached in
  `$cache_dir/genesis/dataset/`.
- **Protocol**: the protocol parameters (slot/epoch length, cost models,
  governance thresholds, ...). Cached in `$cache_dir/genesis/protocol/`.

Two profiles that share dataset knobs but tweak pparams reuse one dataset entry,
two profiles that share pparams but vary sizing reuse one protocol entry. The
zero specs in this directory are what makes that decoupling possible: feeding
them as `--spec-*` inputs to `create-testnet-data` keeps the dataset output
independent of the pparams. The two pparams create-testnet-data does bake into
dataset fields (shelley `minUTxOValue`, conway `dRepDeposit`) are injected into
temp specs and folded into the dataset cache key by `dataset-cache-ensure`, so
identical dataset knobs still produce byte-identical fields.

At run start, `derive-from-cache-ripper` stitches dataset + protocol + per-run
timing (`startTime`, `systemStart`) into the final
`genesis.{byron,shelley,conway}.json`. Alonzo and Dijkstra have no dataset side
and are taken straight from the protocol cache.

# 3. What each parameter controls

## Always-created files (regardless of parameters):

- `byron-genesis.json`
- `byron.genesis.spec.json`
- `byron-gen-command/` (directory)
- `shelley-genesis.json`
- `alonzo-genesis.json`
- `conway-genesis.json`
- `dijkstra-genesis.json`

## When each parameter takes effect

| Parameter            | Activation                                                                |
|----------------------|---------------------------------------------------------------------------|
| `--committee-keys`   | (unused by us, always 0)                                                  |
| `--genesis-keys`     | >= 1                                                                      |
| `--pools`            | >= 1                                                                      |
| `--stake-delegators` | >= 1 (keys always created; JSON fields only when `pools >= 1`)            |
| `--drep-keys`        | >= 1                                                                      |
| `--stuffed-utxo`     | >= 1                                                                      |
| `--utxo-keys`        | >= 1                                                                      |
| `--total-supply`     | >= 1                                                                      |
| `--delegated-supply` | >= 1 AND (`utxo-keys >= 1` OR (`pools >= 1` AND `stake-delegators >= 1`)) |
| `--testnet-magic`    | we always pass 42 here (writes `networkMagic` in shelley-genesis.json)    |

## What files each parameter creates

| Parameter            | Directory created                     | Contents per entry |
|----------------------|---------------------------------------|--------------------|
| `--genesis-keys`     | `genesis-keys/genesisN/`              | `key.skey`, `key.vkey` (2 files) |
| `--genesis-keys`     | `delegate-keys/delegateN/`            | `key.skey`, `key.vkey`, `kes.skey`, `kes.vkey`, `vrf.skey`, `vrf.vkey`, `opcert.cert`, `opcert.counter` (8 files) |
| `--pools`            | `pools-keys/poolN/`                   | `byron-delegate.key`, `byron-delegation.cert`, `cold.skey`, `cold.vkey`, `kes.skey`, `kes.vkey`, `opcert.cert`, `opcert.counter`, `staking-reward.skey`, `staking-reward.vkey`, `vrf.skey`, `vrf.vkey` (12 files) |
| `--pools`            | `byron-gen-command/` (always present) | `genesis-keys.NNN.key` (one per pool, or `000` when `--pools 0`); `delegate-keys.000.key`, `delegation-cert.000.json` (only when `--pools 0`) |
| `--stake-delegators` | `stake-delegators/delegatorN/`        | `payment.skey`, `payment.vkey`, `staking.skey`, `staking.vkey` (4 files) |
| `--drep-keys`        | `drep-keys/drepN/`                    | `drep.skey`, `drep.vkey` (2 files) |
| `--utxo-keys`        | `utxo-keys/utxoN/`                    | `utxo.skey`, `utxo.vkey` (2 files) |

Each of `pools-keys/`, `genesis-keys/`, `delegate-keys/`, `utxo-keys/` and
`drep-keys/` additionally contains a `README.md` emitted by `cardano-cli`
(documentation only, not used by the workbench). `stake-delegators/` and
`byron-gen-command/` do not get one.

### What each file is

Across every key directory the suffix convention is `.skey` = signing (private)
key, `.vkey` = verification (public) key, `.cert` = certificate, `.counter` =
monotonic issuance counter. `cardano-cli` emits the full set of files above for
every entry, but the workbench itself only reads a small subset at runtime, the
rest is generated for completeness and parity with a real network.

#### Used by the workbench

| File                                       | Read by                                        | Role at runtime |
|--------------------------------------------|------------------------------------------------|-----------------|
| `pools-keys/poolN/opcert.cert`             | `service/nodes.nix` (`operationalCertificate`) | Operational certificate the pool node presents to prove it is authorised to sign blocks in the current KES period. |
| `pools-keys/poolN/kes.skey`                | `service/nodes.nix` (`kesKey`)                 | KES signing key the pool node loads to sign block headers; evolves each KES period and is replaced wholesale every `maxKESEvolutions` periods via a fresh opcert. |
| `pools-keys/poolN/vrf.skey`                | `service/nodes.nix` (`vrfKey`)                 | VRF signing key the pool node loads to produce slot-leader proofs. |
| `utxo-keys/utxo1/utxo.skey`                | `service/generator.nix` (`sigKey`)             | Signs the funding transactions the workload generator emits, drawing from the genesis-allocated UTxO in `shelley-genesis.json::initialFunds`. |
| `utxo-keys/utxo2/utxo.{skey,vkey}`         | `workload/voting.nix`                          | Funds source (and matching vkey) for the voting workload's governance transactions. |
| `stake-delegators/delegatorN/staking.vkey` | `workload/voting.nix`                          | Stake credential used as the deposit-return and funds-receiving address when the voting workload submits governance actions. |
| `drep-keys/drepN/drep.{skey,vkey}`         | `workload/voting.nix`                          | DRep identity. `hash(drep.vkey)` is the DRep ID in `conway-genesis.json::initialDReps`/`delegs`; the skey signs `Vote` transactions. |

That is the entire runtime input from this directory tree, three files per pool
node plus a handful of UTxO/stake/DRep keys consumed by the workload scripts.

#### Generated but not consumed

The remaining files are emitted by `cardano-cli create-testnet-data` but
never read by the workbench. Listed for awareness:

- `genesis-keys/genesisN/key.{skey,vkey}` and the whole
  `delegate-keys/delegateN/` directory (`key.{skey,vkey}`, `kes.{skey,vkey}`,
  `vrf.{skey,vkey}`, `opcert.cert`, `opcert.counter`): BFT/genesis-delegation
  key material. The workbench launches Shelley pools, not BFT-delegated nodes,
  so no service ever loads these.
- `pools-keys/poolN/cold.{skey,vkey}`: pool cold identity (`hash(cold.vkey)` is
  the pool ID in `shelley-genesis.json::staking.pools`). Used at *generation*
  time to sign the initial opcert; never re-loaded at runtime because the
  workbench does not rotate KES.
- `pools-keys/poolN/kes.vkey`, `vrf.vkey`: only their hashes survive at runtime,
  bound into `opcert.cert` and the staking record. The vkey files themselves are
  not read.
- `pools-keys/poolN/opcert.counter`: only consumed if a new opcert is ever
  minted (it isn't).
- `pools-keys/poolN/staking-reward.{skey,vkey}`: the pool reward account. Block
  rewards accumulate at the address derived from the vkey hash but are never
  withdrawn in a benchmark run.
- `pools-keys/poolN/byron-delegate.key`,
  `pools-keys/poolN/byron-delegation.cert`: Byron-era heavy-delegation
  artifacts. Workbench profiles start past the Byron era, so the node never
  reads them.
- `stake-delegators/delegatorN/payment.{skey,vkey}` and
  `stake-delegators/delegatorN/staking.skey`: payment credential and the stake
  *signing* key. The voting workload only consumes the corresponding
  `staking.vkey` (above); nothing signs as a delegator.
- `byron-gen-command/*` (`genesis-keys.NNN.key`, `delegate-keys.000.key`,
  `delegation-cert.000.json`): Byron CLI artifacts. Same reason as the byron
  pool files: the workbench never enters Byron-era operation.

## What each parameter writes into the JSON

Two of the five output files never vary: `alonzo-genesis.json` and
`dijkstra-genesis.json` are byte-identical across all flag combinations (and
there is no `--spec-dijkstra`). The other three carry every flag-driven field,
documented one file per section below. In each section the table is
authoritative: one row per top-level key, with its driver and its size, value, or
shape. Every top-level key not listed is fixed by the zero spec and identical in
every output.

All key-derived values in these files (key hashes, public keys, certificates,
addresses, DRep and pool IDs) are regenerated on every run: identical flags
produce different key material but the same structure, counts, and monetary
values. This holds for every era below.

### Parameter to field index

Reverse lookup, parameter to the keys it writes. For a key's size, guard, and
shape see its row in the per-file section. Activation guards (the minimum values
that make a field non-empty) are in **When each parameter takes effect** above.

| Parameter            | Top-level keys it writes |
|----------------------|--------------------------|
| `--total-supply`     | shelley `maxLovelaceSupply`, `initialFunds` (values) |
| `--delegated-supply` | shelley `initialFunds` (UTxO vs staked split) |
| `--utxo-keys`        | shelley `initialFunds` (enterprise entries) |
| `--stuffed-utxo`     | shelley `initialFunds` (enterprise entries) |
| `--stake-delegators` | shelley `initialFunds` (base entries), `staking.stake`; conway `delegs` |
| `--pools`            | byron `bootStakeholders`, `heavyDelegation`, `nonAvvmBalances`; shelley `staking.pools` |
| `--genesis-keys`     | shelley `genDelegs` |
| `--drep-keys`        | conway `initialDReps`, `delegs` |
| `--testnet-magic`    | shelley `networkMagic` |
| `--start-time`       | byron `startTime`, shelley `systemStart` |

## byron-genesis.json

Only `--pools` shapes byron content. `--start-time` writes the top-level
`startTime` scalar (a per-run timing field, hardcoded to `1970-01-01T00:00:00Z`
in the test sweep). Nothing else touches byron.

| Top-level key      | Driven by      | Count / value |
|--------------------|----------------|---------------|
| `bootStakeholders` | `--pools`      | `max(pools, 1)` entries, weight 1 each |
| `heavyDelegation`  | `--pools`      | `max(pools, 1)` entries |
| `nonAvvmBalances`  | `--pools`      | `max(pools, 1)` entries, `3,000,000,000` each (hardcoded) |
| `startTime`        | `--start-time` | per-run epoch seconds |

The three `--pools` fields always have at least one entry (a default BFT entry),
so `--pools 0` and `--pools 1` give the same structure; extra entries begin at
`--pools 2`. The `nonAvvmBalances` value is always `3,000,000,000` per entry
regardless of `--total-supply`, `--delegated-supply`, or anything else; it is
Byron-era lovelace, a separate ledger from Shelley's `initialFunds`.

| pools | entries per field |
|-------|-------------------|
| 0     | 1                 |
| 1     | 1                 |
| 5     | 5                 |

### `byron-gen-command/`

Always present, but its contents depend on `--pools`:

- `genesis-keys.NNN.key`: `max(--pools, 1)` files, numbered from `000`.
- `delegate-keys.000.key` and `delegation-cert.000.json`: only when `--pools 0` (the BFT pair).

| pools | files in `byron-gen-command/` |
|-------|-------------------------------|
| 0     | `genesis-keys.000.key`, `delegate-keys.000.key`, `delegation-cert.000.json` |
| 1     | `genesis-keys.000.key` |
| 5     | `genesis-keys.000.key` ... `genesis-keys.004.key` |

## shelley-genesis.json

Always exactly **15 top-level keys**: 9 fixed by the zero spec, plus 6 shaped by
the flags (`genDelegs`, `initialFunds`, `maxLovelaceSupply`, `networkMagic`,
`staking`, `systemStart`).

### Static keys (from the zero spec)

Identical in every output regardless of any flag. `securityParam` and
`activeSlotsCoeff` are the only non-zero values; both are pinned because the
parser rejects `0` for them.

| Key                 | Value                        |
|---------------------|------------------------------|
| `activeSlotsCoeff`  | `0.01`                       |
| `epochLength`       | `0`                          |
| `maxKESEvolutions`  | `0`                          |
| `networkId`         | `"Mainnet"` (see note)       |
| `protocolParams`    | all sub-fields `0` / neutral |
| `securityParam`     | `1`                          |
| `slotLength`        | `0`                          |
| `slotsPerKESPeriod` | `0`                          |
| `updateQuorum`      | `0`                          |

`networkId` vs `networkMagic`: `--testnet-magic 42` overwrites `networkMagic`
(`764824073` -> `42`) but never touches `networkId`, which stays at the spec's
`"Mainnet"`. Pool `accountAddress` records still read `"network": "Testnet"`
because that field comes from the flag-supplied network, not from `networkId`.
This is a `cardano-cli` quirk, not a local change.

### Flag-shaped keys

| Key                 | Driven by                                             | Size / value                                                                            | Absent (guard fails) |
|---------------------|-------------------------------------------------------|----------------------------------------------------------------------------------------|----------------------|
| `maxLovelaceSupply` | `--total-supply`                                      | verbatim copy (the ledger cap, not the distributed total)                              | `0`                  |
| `networkMagic`      | `--testnet-magic`                                     | verbatim copy (always `42` here)                                                        | always set           |
| `systemStart`       | `--start-time`                                        | ISO-8601 UTC string; per-run timing                                                     | always set           |
| `genDelegs`         | `--genesis-keys`                                      | size = `--genesis-keys`; shape below                                                    | `{}`                 |
| `initialFunds`      | `--utxo-keys`, `--stuffed-utxo`, `--stake-delegators` | entry table below; values from `--total-supply` / `--delegated-supply` / `minUTxOValue` | `{}`                 |
| `staking.pools`     | `--pools` and `--stake-delegators`                    | size = `min(pools, stake-delegators)`; shape below                                      | `{}`                 |
| `staking.stake`     | `--stake-delegators` (needs `--pools >= 1`)           | size = `--stake-delegators`; shape below                                                | `{}`                 |

`systemStart` is per-run: the ripper strips it from the protocol cache and
re-injects it from the profile's timing at assembly.

### Field shapes

`genDelegs` maps a genesis cold-key hash to its delegation:

```
"<genesis-cold-key-hash-28B>": { "delegate": "<28B>", "vrf": "<32B>" }
```

`initialFunds` is a flat `<address-hex>` -> `<lovelace>` map merging up to three
streams. The address prefix separates staked (`00`) from enterprise (`60`), but
`--utxo-keys` and `--stuffed-utxo` share the `60` prefix and are told apart only
by value:

| Stream               | Prefix          | Bytes | Hex len | Value per entry |
|----------------------|-----------------|-------|---------|-----------------|
| `--utxo-keys`        | `60` enterprise | 29    | 58      | `utxo_each` (Section 4) |
| `--stuffed-utxo`     | `60` enterprise | 29    | 58      | `minUTxOValue` from `--spec-shelley` (`0` under the zero spec) |
| `--stake-delegators` | `00` base       | 57    | 114     | `staked_each` (Section 4) |

One entry per stream (`<address-hex>: <lovelace>`):

```
"60<payment-hash-28B>": <utxo_each>                    # --utxo-keys
"60<payment-hash-28B>": 0                              # --stuffed-utxo (value = minUTxOValue)
"00<payment-hash-28B><stake-hash-28B>": <staked_each>  # --stake-delegators
```

Total entries = `utxo-keys + stuffed-utxo + stake-delegators`, the
stake-delegator stream present only when `--pools >= 1`. Each stream's per-entry
value is `floor(slice / count)`, except its first entry, which also absorbs the
division remainder. The slice math (the 10% withheld, the `--delegated-supply`
split) is in Section 4.

> **Spec-derived stuffed value.** The stuffed-UTxO value is not hardcoded:
> `create-testnet-data` sets each stuffed entry to `minUTxOValue` from
> `--spec-shelley` (`mkStuffedUtxo`, `Run.hs:924`), `0` here only because the zero
> spec zeroes it. The ripper injects the profile's `minUTxOValue` into the dataset
> shelley spec and folds it into the dataset cache key, so the stuffed value
> matches the real (`jq`) genesis even for a `pparamsEpoch` (~208-289) where it is
> non-zero. Same handling as the conway `dRepDeposit`.

`staking.pools` maps a pool ID to a fully-zeroed pool record:

```
"<pool-id-28B>": {
  "accountAddress": { "credential": { "keyHash": "<28B>" }, "network": "Testnet" },
  "cost": 0, "margin": 0, "metadata": null, "owners": [],
  "pledge": 0, "poolId": "<same-pool-id>", "relays": [],
  "vrf": "<32B>"
}
```

`staking.stake` maps a stake-key hash to the pool ID it delegates to:

```
"<stake-key-hash-28B>": "<pool-id-28B>"
```

Delegations spread evenly across the `min(pools, stake-delegators)` pools
(`--pools 5 --stake-delegators 10` gives 2 stake keys per pool; any remainder
front-loads onto the first pools). These stake-key hashes are the same ones in
`conway-genesis.json::delegs`.

### Sizes by pools x stake-delegators

| pools | stake-delegators | `staking.pools` | `staking.stake` |
|-------|------------------|-----------------|-----------------|
| 0     | 0                | 0               | 0               |
| 0     | 10               | 0               | 0               |
| 1     | 0                | 0               | 0               |
| 1     | 1                | 1               | 1               |
| 1     | 10               | 1               | 10              |
| 5     | 1                | 1               | 1               |
| 5     | 10               | 5               | 10 (2 per pool) |

## conway-genesis.json

Only `--drep-keys` and `--stake-delegators` shape conway content; every other
top-level key comes from the zero spec.

| Key            | Driven by                              | Size / shape |
|----------------|----------------------------------------|--------------|
| `initialDReps` | `--drep-keys`                          | size = `--drep-keys`; each entry `{ deposit, expiry }` (see below) |
| `delegs`       | `--drep-keys` and `--stake-delegators` | size = `--stake-delegators`; stake-key hash -> DRep ID |

Both are omitted when their guard fails:

| drep-keys | stake-delegators | `initialDReps` | `delegs`  |
|-----------|------------------|----------------|-----------|
| 0         | 0                | absent         | absent    |
| 0         | >= 1             | absent         | absent    |
| D >= 1    | 0                | D entries      | absent    |
| D >= 1    | N >= 1           | D entries      | N entries |

`create-testnet-data` drops `initialDReps`/`delegs` entirely when empty; the
ripper projects each to `{}` while building `dataset.conway.json`, so the
assembled `genesis.conway.json` always has both keys (cardano-node's parser
rejects `null` but accepts `{}`).

`initialDReps` keys a DRep ID to its deposit and expiry:

```
"keyHash-<drep-id-28B>": { "deposit": <coin>, "expiry": 1000 }
```

- `expiry` is hardcoded by `create-testnet-data` (`drepExpiry = EpochNo 1_000`,
  `Run.hs:522`); not a spec or flag value.
- `deposit = max(1000000, dRepDeposit)`, with `dRepDeposit` from the
  `--spec-conway` params (`Run.hs:488,515`). It is `1000000` here only because the
  zero spec sets `dRepDeposit: 0`; a profile with `dRepDeposit: 500000000` makes
  every entry `500000000`. The ripper injects the profile's value into the dataset
  conway spec and folds it into the dataset cache key, so the result matches the
  real (`jq`) backend. Same handling as the stuffed-UTxO value above.

`delegs` keys a stake-key hash to the DRep it delegates to:

```
"keyHash-<stake-hash-28B>": { "dRep": "drep-keyHash-<drep-id-28B>", "kind": "DelegVote" }
```

The pairing is round-robin, not random: delegator `i` delegates to DRep
`i mod drep-keys`, so each `dRep` value references an `initialDReps` key. The
stake-key hashes are the same ones in `shelley-genesis.json::staking.stake`.

# 4. Funds distribution

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

## `--stake-delegators`

```
staked_total = delegated-supply - floor(delegated-supply / 10)
staked_each = floor(staked_total / stake-delegators)
```

`--delegated-supply` sets the staked slice; the rest (`total-supply` minus
`delegated-supply`) funds the UTxO slice. Set it to `0` and the whole supply
(minus the withheld 10%) goes to UTxO keys. Address prefixes, per-stream entry
counts, and the activation guard live in the **shelley-genesis.json** section
above.

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

# 5. Constitutional Committee (`--committee-keys 0`)

We always pass `--committee-keys 0` here for simplicity. Non-zero would generate
`N` committee cold-key pairs and populate
`conway-genesis.json::committee.members` with their hashes and expiration epochs.

## Why the voting profile still ratifies

The voting overlay (`bench/cardano-profile/data/genesis/overlays/voting.json`)
sets `committee.members: {}`, `committee.threshold: 0`, and
`committeeMinSize: 0`. That combination short-circuits the Conway ledger:
`activeCommitteeSize (0) >= committeeMinSize (0)` returns `VotingThreshold 0`,
and threshold `0` is treated as auto-YES. The CC vote is removed from the
equation; **DReps alone decide ratification** (SPOs join for actions that need
pool consent).

Verified in
`cardano-ledger/eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Internal.hs`
(`votingCommitteeThresholdInternal`) and `.../Rules/Ratify.hs`
(`committeeAccepted`).

## What we don't benchmark

- **Hot-key authorization**: each member's `AuthCommitteeHotKey` cert (cold-sig over hot key).
- **CC vote transactions**: at mainnet thresholds each ratifying action accumulates ~`threshold x |CC|` of them.
- **Vote aggregation under non-zero threshold**: counting YES/NO/Abstain across active members per action vs. the short-circuit.
- **`CommitteeState` bookkeeping**: hot-key map, resignations, expirations.
- **No-Confidence / Update-Committee actions**: inert without a real CC to remove or replace.

To exercise any of these: set `committee.members` to N keys with `threshold = 2/3`,
`committeeMinSize = N`, and have the workload submit `AuthCommitteeHotKey`
certs + CC `Vote` transactions.

