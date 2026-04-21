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

- **Dataset** — the heavy fields driven by `--pools`, `--stake-delegators`,
  `--total-supply`, ... (`initialFunds`, `staking`, `genDelegs`,
  `bootStakeholders`, `initialDReps`, `delegs`, ...). Cached in
  `$cache_dir/genesis/dataset/`.
- **Protocol** — the protocol parameters (slot/epoch length, cost models,
  governance thresholds, ...). Cached in `$cache_dir/genesis/protocol/`.

Two profiles that share dataset knobs but tweak pparams reuse one dataset entry,
two profiles that share pparams but vary sizing reuse one protocol entry. The
zero specs in this directory are what makes that decoupling possible: feeding
them as `--spec-*` inputs to `create-testnet-data` guarantees the dataset output
is independent of any meaningful pparams, so identical dataset knobs always
produce byte-identical fields.

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

## What each parameter changes in JSON files

### Always-present, unchanged files

- `alonzo-genesis.json`   (byte-identical across all parameter combinations)
- `dijkstra-genesis.json` (byte-identical across all parameter combinations, no `--spec-dijkstra`)

### Per-era top-level mutability map

The only top-level keys whose presence or value depends on the CLI flags below.
Every other top-level key in `byron`/`shelley`/`conway` is fixed by the zero
spec and is the same in every output.

| Parameter            | byron-genesis.json                                                | shelley-genesis.json                                                                                          | conway-genesis.json                                                                                                            |
|----------------------|-------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| `--total-supply`     | --                                                                | `maxLovelaceSupply`; `initialFunds` values                                                                    | --                                                                                                                             |
| `--delegated-supply` | --                                                                | `initialFunds` values (splits lovelace between UTxO and staked entries)                                       | --                                                                                                                             |
| `--utxo-keys`        | --                                                                | `initialFunds` (adds `60`-prefix enterprise-address entries)                                                  | --                                                                                                                             |
| `--pools`            | `bootStakeholders`, `heavyDelegation`, `nonAvvmBalances` (entries when `--pools >= 2`; see Byron section) | `staking.pools` (size = `min(--pools, --stake-delegators)`; empty when either is `0`)                         | --                                                                                                                             |
| `--stake-delegators` | --                                                                | When `--pools >= 1`: `initialFunds` (adds `00`-prefix staked-address entries), `staking.stake`, `staking.pools` | `delegs` (only when `--drep-keys >= 1`; size = `--stake-delegators`; each entry is a stake-key hash → randomly-picked DRep) |
| `--genesis-keys`     | --                                                                | `genDelegs` (maps genesis cold-key hash → delegate + VRF; size = `--genesis-keys`)                            | --                                                                                                                             |
| `--drep-keys`        | --                                                                | --                                                                                                            | `initialDReps` (size = `--drep-keys`); `delegs` (only when `--stake-delegators >= 1`; see row above)                            |
| `--stuffed-utxo`     | --                                                                | `initialFunds` (adds `60`-prefix entries with 0 lovelace)                                                     | --                                                                                                                             |
| `--testnet-magic`    | --                                                                | `networkMagic`                                                                                                | --                                                                                                                             |
| `--start-time`       | `startTime` (epoch seconds)                                       | `systemStart` (ISO-8601 UTC string)                                                                           | --                                                                                                                             |

See the per-era sections below for field shapes, randomness, and cross-era
references (e.g. shelley's `staking.stake` keys vs. conway's `delegs` keys).

## Byron (byron-genesis.json)

Within the dataset parameters above, only `--pools` shapes the content of
`byron-genesis.json` (`--start-time` writes the top-level `startTime` scalar,
but is a per-run timing field, hardcoded to `1970-01-01T00:00:00Z` in the test
sweep). No other parameter changes byron.

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

### `byron-gen-command/`

Always present, but its contents depend on `--pools`:

- `genesis-keys.NNN.key`: `max(--pools, 1)` files, numbered `000` onwards.
- `delegate-keys.000.key` and `delegation-cert.000.json`: only when `--pools = 0` (the BFT pair).

| pools | files in `byron-gen-command/` |
|-------|-------------------------------|
| 0     | `genesis-keys.000.key`, `delegate-keys.000.key`, `delegation-cert.000.json` |
| 1     | `genesis-keys.000.key` |
| 5     | `genesis-keys.000.key`, `genesis-keys.001.key`, `genesis-keys.002.key`, `genesis-keys.003.key`, `genesis-keys.004.key` |

### Cryptographic material

All cryptographic material in these three fields (key hashes, public keys,
delegation certs, and Byron addresses) is **randomly generated on every run**.
Two calls with identical parameters will produce different keys and addresses
but the same structure and the same monetary values.

## Shelley (shelley-genesis.json)

`shelley-genesis.json` always has exactly **15 top-level keys**. Nine come
straight from the zero spec and are byte-identical in every output; the
remaining six (`genDelegs`, `initialFunds`, `maxLovelaceSupply`, `networkMagic`,
`staking`, `systemStart`) are shaped by the CLI flags.
`staking` is broken out into its two sub-fields (`staking.pools` and
`staking.stake`) below because they have independent activation rules.

### Static (zero-spec) top-level keys

These nine keys appear in every output with the same value regardless of any
flag (`securityParam` and `activeSlotsCoeff` are the only non-zero values,
both are pinned in the zero spec because the parser rejects `0` for them).

| Key                    | Value                          |
|------------------------|--------------------------------|
| `activeSlotsCoeff`     | `1.0E-9`                       |
| `epochLength`          | `0`                            |
| `maxKESEvolutions`     | `0`                            |
| `networkId`            | `"Mainnet"` (see note below)   |
| `protocolParams`       | all sub-fields `0` / neutral   |
| `securityParam`        | `1`                            |
| `slotLength`           | `0`                            |
| `slotsPerKESPeriod`    | `0`                            |
| `updateQuorum`         | `0`                            |

Note on `networkId` vs `networkMagic`: `--testnet-magic 42` overwrites
`networkMagic` (`764824073` → `42`), but **does not** touch the `networkId`
field, which stays at the spec's `"Mainnet"`. Despite that, the addresses
emitted inside `staking.pools[*].accountAddress` carry
`"network": "Testnet"`. This is a `cardano-cli` quirk, not a bug introduced
here.

### Mutable top-level keys

| Key                 | Driven by                                        | Default when guard fails |
|---------------------|--------------------------------------------------|--------------------------|
| `genDelegs`         | `--genesis-keys`                                 | `{}`                     |
| `initialFunds`      | `--utxo-keys`, `--stuffed-utxo`, `--stake-delegators` (×`--pools >= 1`); values from `--total-supply`, `--delegated-supply` | `{}` |
| `maxLovelaceSupply` | `--total-supply`                                 | `0`                      |
| `networkMagic`      | `--testnet-magic`                                | (no guard; always set)   |
| `staking.pools`     | `--pools` AND `--stake-delegators`               | `{}`                     |
| `staking.stake`     | `--stake-delegators` (only when `--pools >= 1`)  | `{}`                     |
| `systemStart`       | `--start-time`                                   | (no guard; always set)   |

#### `genDelegs`

Size = `--genesis-keys` (`0` when the flag is `0`).
Each entry maps a genesis cold-key hash to its delegation:

```
"<genesis-cold-key-hash-28B>": { "delegate": "<28B>", "vrf": "<32B>" }
```

All three hashes are randomly generated per call.

#### `initialFunds`

A flat map of `<address-hex>` → `<lovelace>` aggregating up to three streams.
The address byte tells them apart:

| Source              | Address prefix | Total bytes | Hex length | Value                                                                |
|---------------------|----------------|-------------|------------|----------------------------------------------------------------------|
| `--utxo-keys`       | `60` (enterprise) | 29       | 58         | `utxo_each` = floor(utxo_total / utxo-keys) — see section 4          |
| `--stuffed-utxo`    | `60` (enterprise) | 29       | 58         | `0` always                                                           |
| `--stake-delegators` (active only when `--pools >= 1`) | `00` (base) | 57 | 114 | `staked_each` = floor(staked_total / stake-delegators) — see section 4 |

Total entry count = `utxo-keys + stuffed-utxo + (stake-delegators if pools >= 1 else 0)`.

The exact lovelace math (10% reserves, splits driven by `--delegated-supply`) is
documented under **Section 4. Funds distribution**.

#### `maxLovelaceSupply`

Set verbatim to `--total-supply` (default `0`). Not divided by anything; this is
the ledger cap, not the distributed total.

#### `networkMagic`

Set verbatim to `--testnet-magic` (we always pass `42`). Unrelated to
`networkId` (see note above).

#### `staking.pools`

Size = `min(--pools, --stake-delegators)`. Empty when either is `0`.
Each entry maps a pool ID to a fully-zeroed pool record:

```
"<pool-id-28B>": {
  "accountAddress": { "credential": { "keyHash": "<28B>" }, "network": "Testnet" },
  "cost": 0, "margin": 0, "metadata": null, "owners": [],
  "pledge": 0, "poolId": "<same-pool-id>", "relays": [],
  "vrf": "<32B>"
}
```

#### `staking.stake`

Size = `--stake-delegators` when `--pools >= 1`; otherwise `0`.
Each entry maps a stake-key hash to a pool ID it delegates to:

```
"<stake-key-hash-28B>": "<pool-id-28B>"
```

Delegations are spread **evenly** across the `min(pools, stake-delegators)`
pools (e.g. `--pools 5 --stake-delegators 10` → exactly 2 stake keys per
pool). The stake-key hashes are the **same hashes** referenced by
`conway-genesis.json::delegs` (when both `--stake-delegators >= 1` AND
`--drep-keys >= 1`).

#### `systemStart`

ISO-8601 UTC string equal to `--start-time`, hardcoded to
`1970-01-01T00:00:00Z` in this analysis. Per-run timing field — the ripper
backend strips it from the protocol-cache file and re-injects it from the
profile's timing at run assembly.

### Per-pair examples

Sanity-check rows:

| pools | stake-delegators | `staking.pools` size | `staking.stake` size |
|-------|------------------|----------------------|----------------------|
| 0     | 0                | 0                    | 0                    |
| 0     | 10               | 0                    | 0                    |
| 1     | 0                | 0                    | 0                    |
| 1     | 1                | 1                    | 1                    |
| 1     | 10               | 1                    | 10                   |
| 5     | 1                | 1                    | 1                    |
| 5     | 10               | 5                    | 10 (2 per pool)      |

## Conway (conway-genesis.json)

Only `--drep-keys` and `--stake-delegators` shape the content of
`conway-genesis.json`. Every other top-level key comes from the zero spec and
is fixed.

| drep-keys | stake-delegators | `initialDReps`           | `delegs`                                                       |
|-----------|------------------|--------------------------|----------------------------------------------------------------|
| 0         | 0                | absent                   | absent                                                         |
| 0         | >= 1             | absent                   | absent                                                         |
| D >= 1    | 0                | D entries                | absent                                                         |
| D >= 1    | N >= 1           | D entries                | N entries (each stake-key hash randomly assigned to one of D DReps) |

Both `initialDReps` and `delegs` are **omitted entirely** from
`conway-genesis.json` when their guard fails. The ripper backend then projects
each missing field to `{}` while building `conway.dataset.json`, so the
assembled `genesis.conway.json` always exposes both keys — cardano-node's
parser tolerates `{}` but rejects `null`.

Each `initialDReps` entry has fixed values `deposit: 1000000`, `expiry: 1000`
that don't depend on any flag.

The stake-key hashes used in `delegs` are the **same hashes** that appear in
`shelley-genesis.json::staking.stake`. Like all key material here, the
hashes themselves are randomly generated per run.

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

- **Hot-key authorization** — each member's `AuthCommitteeHotKey` cert (cold-sig over hot key).
- **CC vote transactions** — at mainnet thresholds each ratifying action accumulates ~`threshold × |CC|` of them.
- **Vote aggregation under non-zero threshold** — counting YES/NO/Abstain across active members per action vs. the short-circuit.
- **`CommitteeState` bookkeeping** — hot-key map, resignations, expirations.
- **No-Confidence / Update-Committee actions** — inert without a real CC to remove or replace.

To exercise any of these: set `committee.members` to N keys with `threshold = 2/3`,
`committeeMinSize = N`, and have the workload submit `AuthCommitteeHotKey`
certs + CC `Vote` transactions.

