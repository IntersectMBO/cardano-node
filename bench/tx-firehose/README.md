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

## Output

One JSON line per event on **stderr**, in the cardano-node trace schema (`{at,
sev, host, thread, ns, data}`). Namespaces:

- `TxFirehose.Startup.Query` / `TxFirehose.Startup.Seeded`
- `TxFirehose.Submit.Success` — `{txId, size}`
- `TxFirehose.Submit.Reject` — `{txId, size, reason}`
- `TxFirehose.Build.Fail`
- `TxFirehose.Exit.MaxErrors`

Pipe stderr into Loki/Vector to filter on `ns` in Grafana.

## Exit behaviour

The process exits non-zero on:

- empty UTxO at startup,
- fund set draining below `--inputs-per-tx` (recycling stalled),
- `--max-consecutive-errors` consecutive rejects.

Run it under a supervisor (systemd, k8s, `runit`) that restarts it; on restart
it re-queries the UTxO and picks up whatever the chain now says.
