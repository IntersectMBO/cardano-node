# Examples

`canonical.jsonl` holds one event per observable in the catalogue (plus both edges of `TookSnapshot`), in canonical form. It validates against [`../schema/observables.schema.json`](../schema/observables.schema.json).

**Provenance.** Every line is a real event from a Haskell node's log (`log-node-10.jsonl`, a 6-node workbench cluster run, September 2026), not a hand-written example. Lines were taken unchanged, except for these normalisations to canonical form, each one a documented deviation (specification §8):

| Normalisation | Deviation |
|---|---|
| Points at genesis: `"genesis (origin)"` → `"genesis"` | S1 |
| `TookSnapshot.tip`: `"RealPoint (SlotNo n) <hash>"` → `"<hash>@n"` | S2 |
| ChainHash at genesis: `"GenesisHash"` / `"Genesis"` → `"genesis"` | S3, S4 |
| `BlockContext`: `"current slot"` → `slot`, `tipBlockNo` → `blockNo`, `tip` → `prev` | S5, S6, S7 |
| `Mempool.*`: the optional `tx` / `txs` fields are removed. The log only holds truncated TxIds, and full ones can't be reconstructed from it. | S8 |
| `ChainSync.ServerHeader.Update`: the unspecified `tip` field is removed | S9 |

To keep the lines short, the unconsumed `newSuffixSelectView` / `oldSuffixSelectView` fields of the `ChainDB.AddBlockEvent.*` events were removed as well. Unknown fields are allowed (§2.2).
