# mempool-monitor

Watches **one** node's mempool over the node-to-client `LocalTxMonitor` protocol
and reports which colours it holds, where a colour is the metadata tag
`tx-firehose --color` writes.

One instance per node is the point. Mempool fragmentation is a statement about
how pools *differ*, so an aggregate view hides exactly the thing under test.

## Run

    mempool-monitor \
      --socket-path /path/to/node.socket \
      --testnet-magic 164 \
      --label bp1 \
      --own-color ff0000 \
      --interval 10

`--own-color` is optional and only used to report the local share, that is how
much of this mempool came from the generator attached to this node.

## What it shows

    mempool-monitor  bp1                  slot 41205
    depth   27015 tx      17.6 / 25.0 MB
    bytes [########################..........]
    colours 2    local ff0000 62%
    [====================|========|====]   <- painted in the real tx colours
      ff0000     16700   62%
      00ff88      7200   27%
      (none)      3115   12%
    drained 27015 tx in 1.84s

The composition bar is painted with the colours the transactions actually carry,
so nothing here invents a palette.

Output follows the handle: a repainting pane when stdout is a terminal, one line
per snapshot when it is a log. `--tsv FILE` additionally appends a row per
snapshot for after-the-fact analysis.

## Cost, and why the interval is generous

`MsgNextTx` is one round trip per transaction and returns the whole
transaction, so draining a 27,000-transaction mempool means 27,000 round trips
and something like 17 MB. Depth and capacity come from `MsgGetSizes`, which is a
single message, so the expensive part is only the colour tally.

`--interval` is the period between snapshot *starts*, not a gap after each drain,
so the cadence is what you asked for rather than that plus however long draining
took. A drain that overruns its period degrades to draining continuously instead
of quietly stretching the cadence, which is visible in `drained ... in Xs`.

Ten seconds is the default because a deployment's mempool depth is unknown and a
drain scales with it. Where the depth is known and drains measure in a second or
two, a shorter period is fine — watch the drain time against the period to see
what fraction of the time a node is being iterated.

Three consequences worth keeping in mind:

- **The observer is not free.** It acquires one snapshot per round rather than
  one per transaction, so the cost should be modest, but it is not nothing.
  Treat "monitor attached" as a condition to measure rather than a neutral act,
  and keep it constant across arms of any comparison.
- **The capacity bar is bytes, not fullness.** `MsgGetSizes` reports only the
  byte projection of a multi-dimensional capacity, and the mempool's own measure
  additionally carries a validation-time dimension that neither `GetSizes` nor
  `GetMeasures` exposes. On the Leios prototype that time budget is what binds
  first, so a mempool that has stopped accepting can show this bar at a fraction
  of full. Trust `depth` and the composition; treat the bar as one dimension of
  several.
- **`drained` is a check, not decoration.** It must agree with the `txs` figure
  that `MsgGetSizes` reports for the same snapshot. Two independent counts of
  one quantity; disagreement means the drain did not complete.
