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
