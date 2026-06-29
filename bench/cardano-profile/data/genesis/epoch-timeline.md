# Epoch timeline

Quick reference for the per-epoch evolution of Cardano hard forks and the Plutus
cost-model parameters tracked in `epoch-timeline.json`.

Cross-reference: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md

| Date    | Phase    | Era     | Slot Number | Epoch Number | Protocol Version | Ledger Protocol | Consensus Mechanism     | Notes              |
|---------|----------|---------|------------:|-------------:|-----------------:|-----------------|-------------------------|--------------------|
| 2017/09 | Byron    | Byron   |           0 |            0 |              0,0 | -               | Ouroboros Classic       |                    |
| 2020/02 | Byron    | Byron   |     3801600 |          176 |              1,0 | -               | Ouroboros BFT           |                    |
| 2020/07 | Shelley  | Shelley |     4492800 |          208 |              2,0 | TPraos          | Ouroboros Praos         |                    |
| 2020/12 | Goguen   | Allegra |    16588800 |          236 |              3,0 | TPraos          | Ouroboros Praos         |                    |
| 2021/03 | Goguen   | Mary    |    23068800 |          251 |              4,0 | TPraos          | Ouroboros Praos         |                    |
| 2021/09 | Goguen   | Alonzo  |    39916975 |          290 |              5,0 | TPraos          | Ouroboros Praos         |                    |
| 2021/10 | Goguen   | Alonzo  |    43372972 |          298 |              6,0 | TPraos          | Ouroboros Praos         | intra-era hardfork |
| 2022/09 | Goguen   | Babbage |    72316896 |          365 |              7,0 | Praos           | Ouroboros Praos         | Vasil HF           |
| 2023/02 | Goguen   | Babbage |    84844885 |          394 |              8,0 | Praos           | Ouroboros Praos         | Valentine HF       |
| 2024/09 | Voltaire | Conway  |   133660855 |          507 |              9,0 | Praos           | Ouroboros Genesis/Praos | Chang HF           |
| 2025/01 | Voltaire | Conway  |   146620809 |          537 |             10,0 | Praos           | Ouroboros Genesis/Praos | Plomin HF          |

## Cost-model parameter timeline

Each row matches an `epoch` entry in `epoch-timeline.json`. For each Plutus
language version the columns are:

* **new**   — parameters appended at the tail of the version's positional
              cost-model array (e.g. a brand-new Plutus builtin).
* **upd**   — existing positions whose value is re-set by this entry.
* **total** — running parameter count for that version, after this entry.

| Epoch            | V1 new/upd/total | V2 new/upd/total | V3 new/upd/total
|------------------+------------------+------------------+-----------------
| 298              |  166 /   0 / 166 | --               | --
| 306              |    0 /   0 / 166 | --               | --
| 317              |    0 /   0 / 166 | --               | --
| 319              |    0 /   0 / 166 | --               | --
| 321              |    0 /   0 / 166 | --               | --
| 328              |    0 /   0 / 166 | --               | --
| 335              |    0 /   0 / 166 | --               | --
| 366 (Vasil)      |    0 / 100 / 166 |  175 /   0 / 175 | --
| 394 (Valentine)  |    0 /   2 / 166 |    0 /   7 / 175 | --
| 445              |    0 /   0 / 166 |    0 /   0 / 175 | --
| 507 (Chang)      |    0 /  85 / 166 |   10 /  90 / 185 |  251 /   0 / 251
| 537              |    0 /   0 / 166 |    0 /   0 / 185 |   46 /   0 / 297
| ??? (Van Rossem) |  166 /   7 / 332 |  147 /   4 / 332 |   53 /  46 / 350

The `Van Rossem` row is the preview of `ParameterChange` governance action
`c82f3834898e4d70d3605fa0d92ffe31345701075b107a54309c1525f9581f62#0` (proposed
at epoch 633, not yet enacted at time of writing). Its values currently live in
`bench/cardano-profile/data/genesis/overlays/v11-preview.json` rather than in
`epoch-timeline.json`; the row will move out of preview and the epoch column
will be set to the enactment epoch once the action is enacted on mainnet.

## Editing the timeline

When a new hard fork or `ParameterChange` action takes effect, add a single
entry to `epoch-timeline.json` and a single row to the table above.

### 1. Add a JSON entry, keyed by epoch

Entries are loaded by `Cardano.Benchmarking.Profile.Genesis.epochTimeline`,
which folds them in ascending epoch order. Each entry carries only the *delta*
from the previous one — anything not mentioned is inherited unchanged.

A typical entry looks like:

```json
"<epoch>": {
  "epoch": <epoch>,
  "description": "<HF name or governance action description>",
  "epoch_params": {
    "byron":    { ... },
    "shelley":  { "protocolParams": { ... } },
    "alonzo":   { ... },
    "conway":   { ... },
    "dijkstra": { ... }
  },
  "cost_model": {
    "plutusV1": { "<param-name>": <value>, ... },
    "plutusV2": { "<param-name>": <value>, ... },
    "plutusV3": { "<param-name>": <value>, ... }
  }
}
```

Two top-level fields carry the actual values: `epoch_params` for the
era-specific protocol parameters, and `cost_model` for the Plutus cost-model
parameters. They are merged independently and have different semantics,
covered in §2 and §3 below. The same shape is also used by every overlay
under `data/genesis/overlays/*.json`; an overlay is just an `epoch_params` /
`cost_model` pair that profiles opt into via their `pparamsOverlays` field,
and that gets folded on top of the timeline by `shelleyAlonzoConway` via the
same right-biased merge described below.

### 2. `epoch_params` — protocol parameters per era group

The `epoch_params` block has five optional sub-objects — `byron`, `shelley`,
`alonzo`, `conway`, `dijkstra` — each a free-form JSON KeyMap that mirrors
the corresponding era's genesis JSON shape that `cardano-node` expects (e.g.
`shelley.protocolParams.maxBlockBodySize`, `conway.committee.members`,
`dijkstra.maxRefScriptSizePerBlock`). They are *not* indexed by any canonical
list — each one is just whatever JSON the matching genesis-file parser
accepts.

The merge across timeline entries (and overlays) is a **deep right-biased
union** (`Cardano.Benchmarking.Profile.Genesis.union` /
`unionWithKey`), with two intentional special cases:

* An empty object `{}` on the right **wipes** the left side wholesale. This
  is the only way an overlay can express "I want this collection emptied"
  (e.g. `conway.committee.members: {}` to disable the committee). A plain
  deep merge with `{}` would otherwise add no keys and the left content
  would survive.
* If two object sides differ in a top-level `tag` key (the Aeson tagged-sum
  encoding), the right side **replaces** the left wholesale, instead of
  deep-merging stale payload keys from the old constructor into the new
  one.

Everything else deep-merges, right wins on leaf conflicts. So:

* **Append** a new field — just include it in the new entry; the merge adds
  it.
* **Update** an existing field — set it to the new value in the new entry;
  the merge overwrites.
* **Drop** a collection — set the field to `{}` in the new entry (special
  case above).

A few values inside `byron` and `shelley` are *also* filled from
profile-level fields by `Cardano.Benchmarking.Profile.genesisParams` (Step
2a of `Profile.hs`) after the timeline merge: `byron.protocolConsts.k` /
`protocolMagic`, and `shelley.protocolParams.{slotLength, epochLength,
securityParam, activeSlotsCoeff, networkId, networkMagic}`. Whatever
`epoch-timeline.json` puts there for those keys is overwritten by the
profile; everything else in the era objects flows through as edited.

### 3. `cost_model` — Plutus cost models

Unlike `epoch_params`, the `cost_model` block is indexed by a canonical
parameter name list per language version. The same structure is used in
`epoch-timeline.json` entries and in `data/genesis/overlays/*.json`.

#### 3.1 Wire format: named JSON objects

Each `cost_model.plutusV{1,2,3}` block is a flat JSON **object** keyed by the
canonical parameter name (lower-kebab form of the constructor in
`PlutusLedgerApi.V{1,2,3}.ParamName`, with `'` replaced by `-`):

```json
"cost_model": {
  "plutusV1": {
    "addInteger-cpu-arguments-intercept": 100788,
    "addInteger-cpu-arguments-slope":     420,
    ...
  },
  "plutusV2": { ... },
  "plutusV3": { ... }
}
```

Key order *inside* the object is irrelevant — JSON objects are unordered, and
the merge across entries / overlays is a deep right-biased union keyed by
parameter name. What matters is that each key is a name the workbench
recognises (see §3.3).

V1 keeps the original Alonzo-era spellings `blake2b-…` / `verifySignature-…`
at positions 14–16 / 163–165 for backwards compatibility with the on-chain V1
cost model; V2 and V3 use the renamed `blake2b_256-…` /
`verifyEd25519Signature-…` keys.

#### 3.2 How those objects reach the final genesis

`genesisCostModels` (Step 2b in
`bench/cardano-profile/src/Cardano/Benchmarking/Profile.hs`) takes the merged
object and writes each language version into the generated genesis JSON in
one of two shapes, mirroring the strict shape mainnet shipped with at the
introducing hard fork:

| Lang | Where it lands                                       | Shape  | Entry count                                              |
|------|------------------------------------------------------|--------|----------------------------------------------------------|
| V1   | `alonzo.costModels.PlutusV1`                         | object | exactly 166 (first 166 canonical names; strict parser)   |
| V3   | `conway.plutusV3CostModel`                           | array  | exactly 251 (first 251 canonical positions; strict parser)|
| V1, V2, V3 | `alonzo.extraConfig.costModels.PlutusV{1,2,3}` | array  | the full param count; length check deferred to Plutus's `mkEvaluationContext` |

The two mainnet-shape slots (`alonzo.costModels.PlutusV1` and
`conway.plutusV3CostModel`) cannot grow — the ledger parsers reject anything
but exactly 166 / 251 entries (see `cardano-ledger#5379` and
`cardano-ledger#5241`). Anything subsequent hard forks have appended on
mainnet (the 10 V2 entries at Vasil→Chang, the 46 V3 entries at epoch 537,
the PV11 additions) reaches runtime through the lenient
`alonzo.extraConfig.costModels.*` array form (added in `cardano-ledger#5342`,
surfaced for the CLI in `cardano-cli#1352`) and is applied at the Alonzo era
transition as a per-language replacement.

Where the object form is kept (V1 at `alonzo.costModels.PlutusV1`), the JSON
keys go through unchanged. Where the array form is emitted (the V3 mainnet
slot and all three `extraConfig` slots), the object → array conversion is
positional: `genesisCostModels` walks the canonical name list for the
language version, looks up each name in the object, and writes the value
into the array at that position. An unknown name in the input or a name
missing from the canonical list crashes loudly rather than silently aliasing.

#### 3.3 The canonical name lists, and the only allowed edits

The canonical ordering used by the object → array conversion lives in
`bench/cardano-profile/src/Cardano/Benchmarking/Profile/Genesis.hs`:

* `plutusV1CostNames` — currently 332 entries.
* `plutusV2CostNames` — currently 332 entries.
* `plutusV3CostNames` — currently 350 entries.

Each list is the same lower-kebab transform of the constructors in the
corresponding `PlutusLedgerApi.V{1,2,3}.ParamName` enum (with the V1 legacy
spellings noted in §3.1), **in the exact same order as the enum**. The list
IS the positional index used for the array form; if its order diverges from
the Plutus enum, every cost from the first mismatched position onwards is
mis-priced.

Allowed edits to these lists:

* **Append** at the tail when a new Plutus primitive is added upstream
  (e.g. PV11 appended 166 V1, 147 V2, 53 V3 entries).
* **Rename** an entry only when its constructor was renamed upstream AND the
  on-chain JSON parameter name is meant to follow (so almost never — V1's
  legacy `blake2b` / `verifySignature` names are *not* tracking the upstream
  rename, on purpose).

Forbidden edits:

* Inserting mid-list, reordering, or removing entries — any of these shifts
  every subsequent position and silently mis-prices the array form. Each
  list's tail is `repeat (error "PlutusVN cost with no name")` so reading
  past the known names crashes loudly rather than silently aliasing.

When upstream Plutus adds a parameter you must, in the same change:

* append the new constructor's lower-kebab name to the corresponding
  `plutusV{N}CostNames` list (at the same position the constructor has in
  the enum), and
* add a key for it in the `cost_model.plutusV{N}` object of the new entry in
  `epoch-timeline.json` (or in the overlay) that introduces the parameter,
  with its value.

### 4. Add a row to the cost-model parameter timeline table above

Count, per Plutus version: how many of the entries you just added are new
appends (extend the running `total`), and how many are value updates to
existing positions; carry forward the previous row's `total` plus your new
appends.
