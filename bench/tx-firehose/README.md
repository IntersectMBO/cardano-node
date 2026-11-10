# tx-firehose

A minimal, push-based transaction load generator that drives a single
`cardano-node` over the node-to-client (N2C) socket. It queries the UTxO at its
own derived address, then submits self-payments in a tight loop via
`LocalTxSubmission`, recycling outputs on every accept.

Contrast with `tx-generator` / `tx-centrifuge`, which react to N2N pulls from
downstream peers.

## Build

```
cabal build tx-firehose
```

## Run

```
cabal run tx-firehose -- \
  --socket-path /path/to/node.socket \
  --testnet-magic 164 \
  --signing-key-file payment.skey \
  --tps 10
```

The address is derived from `--signing-key-file` (payment key hash). Pass
`--staking-key-file` to derive a base address instead of an enterprise one. Fund
that address before starting, the tool exits if it finds no UTxO. Use `--help`
for a full list of options.

## Transaction shape

Two modes, selected by whether `--inputs-per-tx` is given:

**Both given** — every tx is built to exactly that shape: `--inputs-per-tx`
inputs, `--outputs-per-tx` outputs. The UTxO set grows or shrinks accordingly,
and the tool exits once fewer than `--inputs-per-tx` funds remain.

**Only `--outputs-per-tx N`** — the input count is derived to keep the UTxO set
size constant: steady-state txs spend `N` inputs into `N` outputs, so tx size is
constant too. While the fund set holds fewer than `N` entries — at startup with
a freshly funded address, say — a single tx fans the highest-value fund out into
`N` outputs; that one tx is enough to reach the steady state, and no further
fan-out happens. The default `N = 1` gives the classic 1-in/1-out self-payment.

Every output is an equal split of (inputs − fee), so values stay balanced across
the set. `--fee` must be covered by the inputs and each resulting output must
clear min-UTxO; otherwise the build fails (traced as `TxFirehose.Build.Fail`).

## Colouring the load

Every generated tx carries a colour in metadata label `1022`, three bytes of
RGB. A mempool observer can then attribute each tx to the firehose that made
it, which is what makes mempool fragmentation visible when several generators
feed different parts of a network.

By default the colour is derived from the signing key: SHA-256 over the
verification key picks a hue, with saturation and lightness fixed so the result
is always vivid. The same key loaded elsewhere (e.g. by `mempool-monitor
--own-key-file`) resolves to the same colour. Pass `--color HEX` to override
with an explicit value, e.g. `--color ff0000`.

Hues are uniform over the circle, but at fixed saturation and lightness there
are only about 1500 distinguishable colours, so with a handful of generators
expect some pairs to land close together. **Assign explicit colours for a run
whose whole point is telling generators apart**; the key-derived default is for
convenience.

The colour is printed on stderr at startup, as a swatch when stderr is a
terminal and as bare hex otherwise (`NO_COLOR` is honoured).

Metadata is not free: the auxiliary data hash alone is 32 bytes in the body, so
a coloured tx runs roughly 45 bytes larger. That is about +20% on a minimal
228-byte tx.

## Controlled transaction size

Add `--tx-size BYTES` to pad each transaction with metadata until its **full
signed CBOR serialization** is at least that size, for example:

```sh
cabal run tx-firehose -- \
  --socket-path /path/to/node.socket \
  --testnet-magic 164 \
  --signing-key-file payment.skey \
  --tps 10 --tx-size 4096 --fee 1000000
```

The target includes the body, witnesses, colour tag and padding. The builder
chooses the smallest filler payload that reaches the target, then measures the
final signed transaction. CBOR header boundaries can round the size up by a few
bytes. Transactions already larger than the target keep their natural size;
the option does not change the input/output shape. Size is recalculated for
each transaction, including startup fan-out and later consolidation.

Padding uses metadata label `0`; the colour remains at label `1022`. Filler is
deterministic from every input's transaction ID and output index, in SHA-512
chunks of at most 64 bytes. It varies across input sets without short repeating
patterns. Omitting `--tx-size` keeps the existing colour-only metadata.

With padding enabled, firehose queries protocol parameters at startup and
checks the final transaction against `maxTxSize` and the ledger's minimum fee
before submitting. **`--fee` remains fixed**: increase it for larger
transactions. An insufficient fee produces `TxFirehose.Build.Fail` with the
required lovelace amount; it is not automatically increased. The example fee
is illustrative; the node's parameters determine the requirement. Restart the
generator after protocol-parameter changes to refresh these checks.

## Output

One JSON line per event on **stderr**, in the cardano-node trace schema (`{at,
sev, host, thread, ns, data}`). Namespaces:

- `TxFirehose.Startup.Query` / `TxFirehose.Startup.Seeded`
- `TxFirehose.Submit.Success` — `{txId, size, serializedSize, inputs, outputs}`
- `TxFirehose.Submit.Reject` — `{txId, size, serializedSize, reason}`
- `TxFirehose.Build.Fail`
- `TxFirehose.Exit.MaxErrors`

Both submit events carry a `color` field, so each transaction is attributable
in the log as well as in its metadata.

`size` retains the ledger's size convention used for fees. `serializedSize`
is the full signed CBOR byte count used by `--tx-size`; these can differ by
era-specific envelope bytes.

Pipe stderr into Loki/Vector to filter on `ns` in Grafana.

## Exit behaviour

The process exits non-zero on:

- empty UTxO at startup,
- fund set draining below the tx's input count (recycling stalled),
- `--max-consecutive-errors` consecutive rejects.

Run it under a supervisor (systemd, k8s, `runit`) that restarts it; on restart
it re-queries the UTxO and picks up whatever the chain now says.

## Upstream payload padding

W45's `--pad-bytes N` remains available: it adds N zero bytes, split into
metadata byte strings of at most 64 bytes, at label `1023`. CBOR overhead makes
the metadata larger than N. This differs from `--tx-size`, which targets the
whole signed transaction and uses deterministic pseudorandom filler at label
`0`. Both preserve the colour at label `1022`. Choose one padding option;
positive `--pad-bytes` cannot be combined with `--tx-size`. Both modes query
protocol parameters and reject oversized transactions or insufficient fixed
fees before submission. Payload mode also checks minimum output coin.

## Growing the UTxO set

`--grow-by COUNT` selects a resumable growth workload. COUNT is **net additional
UTxOs**, including the lane-setup transaction; it is not the final global count.
For a global target of 11,000,000, use `11000000 - existing_count`, measured
before starting. Other workloads must keep their UTxO count steady or be paused
if the final global count matters.

Growth uses existing ADA-only funding UTxOs as independent lanes. Each batch
spends one lane's input/change into N retained outputs and one new change output.
If only one eligible funding UTxO exists, it is first split into up to 64 lanes.
Only change is recycled. Retained outputs use a **different payment key** and are never queried or spent by this mode. The total tracked
fund set is bounded by lanes × pipeline depth, rather than the retained set.
The initial funding address query can still require a ledger scan; subsequent
queries use explicit input references.

Create a dedicated payment key for the retained outputs and keep its signing
key for eventual consolidation. Fund the ordinary firehose address derived from
`--signing-key-file`, or its base address if using `--staking-key-file`. Use a
dedicated funding key with sufficiently large ADA-only outputs. With multiple
eligible outputs, initialization selects up to `--growth-lanes` (largest first,
maximum 4096). Each selected output becomes a lane immediately; no split
transaction or setup confirmation wait is needed. Funding outputs need to exist
on the selected chain, but do not have to be immutable yet. Outputs with native
assets, datums or reference scripts are excluded. Unselected outputs remain
untouched and are not automatically added later. Do not let another process spend either the funding lanes or retained outputs while a
run is in progress. Use exactly one journal for a funding key/run.

Example (Bash; replace paths, network magic and `growth`):

```bash
# Generate once, then keep both files. Do not regenerate on resume.
cardano-cli address key-gen \
  --verification-key-file retained.vkey --signing-key-file retained.skey

# Example adds 11M entries; subtract the existing count for an 11M global target.
growth=11000000
args=(
  --socket-path /run/cardano-node/node.socket
  --testnet-magic 164
  --signing-key-file funding.skey
  --retained-verification-key-file retained.vkey
  --grow-by "$growth"
  --growth-state /var/lib/tx-firehose/growth.json
  --growth-lanes 64 --fanout 200 --retained-lovelace 2000000
  --pipeline-depth 64 --growth-poll-seconds 5 --tps 50
)

# Read-only calibration: reports signed size, calculated fee and effective fanout.
# No journal is created and no transaction is submitted.
tx-firehose "${args[@]}" --growth-dry-run +RTS -N2 -RTS

# Start, or resume with the same arguments and journal.
tx-firehose "${args[@]}" +RTS -N2 -RTS
```

For example, pre-funding 1000 suitably sized outputs and using
`--growth-lanes 1000` starts 1000 independent lanes directly. Size each funding
output for its share of retained outputs plus fees and a change reserve. Smaller
outputs may support fewer than `--fanout` retained outputs; the builder reduces
that batch accordingly. Pre-funding avoids both the single-input setup step and
reliance on deep chains for concurrency. With only one eligible input,
`--growth-lanes` is capped at 64 for the split transaction.

Start with a small separate trial before the full run. Growth automatically
calculates fees from the signed transaction and current protocol parameters;
`--fee`, padding, scheduling and ordinary input/output shape options do not
apply. `--fanout` is a maximum: the builder reduces it to fit both signed CBOR
and ledger `maxTxSize`, available funds, and minimum ADA on every output,
including change. The final batch is shortened to avoid exceeding the target.
With the default 2 ADA retained amount, 11M retained outputs alone need about
22M ADA, plus fees and funding-lane reserves. A sufficient aggregate value does
not guarantee every lane can fund an additional batch; unavailable lanes are
skipped, and exhaustion with no outstanding work exits with an error.

`--tps` is a submission ceiling, not a confirmation guarantee. Each round adds
at most one batch per lane, then waits `--growth-poll-seconds`; serial submission,
queries, signing and durable writes add overhead. A lane can pipeline up to
`--pipeline-depth` batches beyond the immutable tip. Once that limit is reached,
it waits for finality before adding more. Size the window approximately as
`lanes × depth >= desired_TPS × finality_delay_seconds`, allowing headroom.
Larger windows increase query, journal and replay costs. The initial lane setup
only waits for selected-chain inclusion before pipelining starts.

### Progress and recovery

Growth persistence requires a POSIX host (Linux/macOS); the existing load modes
remain available on other supported platforms.

The process appends and fsyncs the changed lane to a `.wal` sidecar **before**
submitting each new batch. This avoids rewriting all lanes on every submission.
Periodic checkpoints atomically replace the main JSON file, fsync its directory,
then compact the WAL; sequence numbers make interrupted compaction safe to
replay. A separate advisory lock prevents two processes from using that journal simultaneously. Use durable local
storage supporting these operations, and retain **both the JSON checkpoint and
its `.wal` sidecar** through restarts. Back up the containing directory while
the process is stopped; neither file is a standalone backup.
It holds public input references, amounts and deterministic transaction recipes;
no signing keys. Prefunded lane anchors are finalized independently, and their
initial entries do not count as new growth. Rebuilt pending transactions must
reproduce the recorded change reference exactly, or execution stops.

Progress distinguishes `finalizedGrowth` from `pendingGrowth`. A successful
submission only means mempool acceptance. The runner queries specific change
outputs at the immutable tip, advances completed lane prefixes and discards their
recipes. Finding a later change proves its preceding batches. Volatile rollbacks,
rejections, eviction and uncertain submissions are recovered by replaying the
same pending transactions; they do not allocate extra target batches. A restart
with unchanged arguments resumes that process. The pipeline and rate settings
may be adjusted on resume; identity, target, retained amount, keys and fanout
must match the journal. A completed journal exits without submitting more work.

Normal completion waits until **all requested growth is immutable**, so it may
wait after submission and selected-chain inclusion have finished. An unavailable
socket/query exits; restart with the same journal. A changed protocol parameter
that invalidates a pending transaction requires investigation: the tool stops
instead of silently replacing a transaction that might still land. Era changes
also require intervention. Do not remove/reset the journal to work around an
error: already submitted transactions may still be included. Recovery assumes
the chain does not roll back beyond its immutable tip.

This is a confirmed count of this generator's net growth, inferred from its
exclusive funding chains; it is not a whole-ledger recount. Spending retained
outputs elsewhere breaks that count. Minimal ADA-only enterprise outputs match
an entry-count target, not mainnet's token/datum/script distribution or exact
memory footprint. Later load must spend across the enlarged set if the goal is
to measure access patterns rather than just resident state size.

### Later cleanup

Cleanup is deliberately separate. Retained outputs are ordinary key-controlled
ADA outputs, so a consolidation utility can spend bounded batches with
`retained.skey` into a fresh destination. Keep that key. The regular firehose's
`--inputs-per-tx N --outputs-per-tx 1` can consolidate a small set, but its initial
address query would load all 11M entries and it does not provide the growth
mode's recovery guarantees. It is not the recommended large-scale cleanup path.
A cleanup utility should enumerate/index retained outputs and stream bounded
input batches, respecting transaction-size, fee and confirmation limits.

## Validation

```bash
cabal test tx-firehose:tx-firehose-test
# Or use the pinned Nix dependencies:
nix build '.#checks.x86_64-linux."tx-firehose-0.1.0.0-inplace-tx-firehose-test/tx-firehose-test"'
```

Tests cover both metadata modes, signed-size boundaries, colour preservation,
fanout sizing and conservation, automatic fees, realistic minimum ADA, and
journal prefix advancement/replay accounting across all supported Shelley eras.

Local validation on 2026-11-10 used a disposable Conway testnet with a 16,384-byte
transaction limit. A transaction with 200 retained enterprise outputs plus change
measured 7,647 signed bytes with that fixture's values and metadata. The deployed
network's dry run is authoritative for its fees and exact shape.

A run with 1000 prefunded lanes added exactly 20,007 ledger entries. It survived
SIGKILL with pending submissions, a torn final WAL record and replay of an old
WAL after checkpointing. Concurrent journal access and mismatched resume options
were rejected; restarting after completion submitted nothing. This verifies
functional recovery on a small local chain, not Leios certification throughput
or performance at 11M entries.
