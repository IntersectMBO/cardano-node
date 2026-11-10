# cardano-node ghc-debug snapshot build

A debug build of `cardano-node` that can produce **ghc-debug heap snapshots** for
**offline** retainer analysis. The intended use is to find what is retaining the
growing `stg_ARR_WORDS_info` (raw byte buffers) and `stg_STACK_info` (thread
stacks) closures that dominate the `-hi` heap profile but, by construction, carry
no allocation-site identity of their own — their *retainers* do, and a snapshot
lets us walk up to them.

## Design: capture on the node, analyse at home

The instrumented node does exactly one thing in the field: **serve its heap for a
self-contained snapshot file** that you ship back. There is deliberately **no
automated analysis or report generation baked in** — capture is the only step
that has to run correctly on a one-shot, and analysis is far more valuable run
offline where it can be re-run against the same snapshot as many times as needed.

Two halves of ghc-debug:

- **Debuggee** = the node, built with the `ghc-debug` cabal flag (links
  `ghc-debug-stub`, wraps `main` with `withGhcDebug`). It serves the ghc-debug
  protocol on the unix socket named by `GHC_DEBUG_SOCKET`.
- **Debugger** = `cardano-debug` (this package), a tiny headless client. Its
  `snapshot` subcommand connects to that socket, pauses the process, writes a
  snapshot, and exits. Snapshotting is *client-side* in ghc-debug — the stub
  alone cannot write a snapshot, which is why this client exists. The same
  binary also analyses a shipped-back snapshot offline (`census` / `retain` /
  `threads`, see below).

The node is additionally built `infoTableMapped` (`-finfo-table-map
-fdistinct-constructor-tables`), so closures in the snapshot map back to source
where GHC records it.

## Build

```sh
nix build .#cardano-node-debug   # instrumented node (IPE + ghc-debug stub)
nix build .#cardano-debug        # snapshot / analysis client
nix build .#caliper              # Javier's offline analysis companion
```

The `jl/leios-w44-debug` branch ports the instrumentation onto the W44 node
base (`b004fe0c5`) and keeps its consensus, network, ledger and API pins.
The default compiler remains GHC 9.6.7. The IPE build also preserves W44's
per-component compiler options, including the `cardano-addresses` symbol
renames and the Plutus external interpreter.

For a combined continuous heap profile and on-demand snapshots, use the
instrumented binary with:

```nix
systemd.services.cardano-node.environment.GHC_DEBUG_SOCKET =
  "/run/cardano-node/ghc-debug.socket";
services.cardano-node = {
  eventlog = true;
  profiling = "space-info"; # -hi
  rts_flags_override = [
    "-i30"
    "-l-agu"
    "--eventlog-flush-interval=300"
  ];
};
```

This takes a heap census every 30 seconds and flushes the reduced eventlog
every five minutes. By default the NixOS service writes `cardano-node.eventlog`
and `cardano-node.hp` in its state directory; `profilingOutputDir` can select
another writable directory. Archive them before restarting, since the filenames
are reused. Keep the same RTS heap/GC settings as the comparison node.

On this branch the `ghc-debug` cabal flag is enabled project-wide (it has to be,
so `ghc-debug-stub` lands in the install plan), so the stub is linked into
*every* build — but it stays **dormant unless `GHC_DEBUG_SOCKET` is set**. This
avoids starting the debug server; it does not undo the IPE instrumentation or
its effects on code size, allocation and performance. What
makes `cardano-node-debug` distinct is that it is built `infoTableMapped` (see
above), so it also serves as the plain IPE build for `-hi` profiling when the
socket is unset.

## Run it

Run `cardano-node-debug` in place of `cardano-node` and set the socket path in
the service environment, e.g. for a NixOS/systemd deployment:

```nix
systemd.services.cardano-node.environment.GHC_DEBUG_SOCKET =
  "/run/cardano-node/ghc-debug.socket";
```

The socket should live somewhere the node's user can create it (e.g. its
`RuntimeDirectory`).

## Capture a snapshot

Run the client as a user that can read/write the socket (e.g. the node user):

```sh
sudo -u cardano-node cardano-debug snapshot \
  /var/lib/cardano-node/leak-$(date -u +%FT%TZ).snapshot \
  "$GHC_DEBUG_SOCKET" \
  --node-revision '<deployed-node-revision>' \
  --node-rts '<deployed-node-RTS-flags>' \
  --reason 'memory growth investigation' \
  +RTS -N1 -RTS
```

> **Operational caveat:** capturing pauses the node (stop-the-world) for the
> duration of the snapshot — seconds to minutes on a multi-GB heap, long
> enough to miss slots / drop peers. Capture deliberately, ideally during low
> activity, not on a tight timer.

The snapshot client caches the **entire heap in its own RAM** before writing the
file, so make sure the host has heap-sized headroom (or run the client on
another machine, below) and disk space larger than the node's heap.

The client reports a monotonic **pause window** covering the pause request,
snapshot transfer/write and resume acknowledgement. It brackets pause/resume
so a snapshot exception attempts to resume the target before connection cleanup.
After resuming, it writes `<snapshot>.capture.json` with UTC timestamps, pause
duration, file size and RTS version/profiling information reported by the stub.
Optional `--node-revision`, `--node-rts`, `--tip`, `--workload` and `--reason`
values are recorded separately as operator-supplied context; omitted values
are left absent, not inferred. See `cardano-debug snapshot --help`.
Existing snapshot or sidecar paths are refused. A failed capture can leave a
partial snapshot without a sidecar; do not treat that file as a successful capture.
If only writing the sidecar fails, the snapshot has already completed and the
target has resumed; the command still exits with an error.

`cardano-debug` retains its existing `-N64` default. Choose the client capability
count explicitly for the host: `+RTS -N1 -RTS` limits traversal concurrency,
while more capabilities may shorten the pause at the cost of more client memory.
`-N1` does not eliminate the heap cache. These are **client** flags, separate
from the deployed node's RTS flags recorded by `--node-rts`.

## Capture from another host

ghc-debug speaks a **unix socket**, so the client can also run off-host —
forward the socket over ssh (unix↔unix), or socat a TCP port to it, then run
against the forwarded socket:

```sh
ssh -L /tmp/ghcdbg.sock:/run/cardano-node/ghc-debug.socket <host>
cardano-debug snapshot /tmp/heap.snapshot /tmp/ghcdbg.sock +RTS -N1 -RTS
```

Either way the node only pays the stop-the-world pause and streams its heap out;
the heap-sized client cache lives on whatever host runs the binary.

## Ship it back

Snapshots are self-contained but can be large (heap-sized). Compress before
sending: `zstd leak-2026-07-15.snapshot`.
Keep its `.capture.json` sidecar alongside it.

## Caliper companion

The `caliper` flake output pins Javier's source at
`3ea7f23b4e452cbc6abecc79f52a0087ecb3e4c7` and uses the same ghc-debug revision
(`2b8cf733f079af6e88eb4d9fa65b1215cc24ebc4`) and GHC as `cardano-debug`.
The Nix package applies a small patch adapting Caliper's non-empty retainer
path types to this revision's list API; the node/stub pin is unchanged.
It can read our raw snapshots for its census, comparison and retainer tools:

```sh
nix run .#caliper -- --help
nix run .#caliper -- census /path/to/heap.snapshot --from-roots +RTS -N1 -RTS
nix run .#caliper -- retainers /path/to/heap.snapshot --match ARR_WORDS --limit 1 +RTS -N1 -RTS
```

Our `.capture.json` is a lightweight capture record, **not** Caliper's `.json`
manifest and is not automatically imported by Caliper. For comparisons, use
Caliper's `manifest backfill` command with the known deployment/input context;
consult `manifest backfill --help`. Backfilling can derive heap facts, but it
cannot recover unrecorded historical RTS flags or workload conditions.
Do not assume that sharing the snapshot format makes two captures comparable.
Keep our thread-label census and targeted info-table commands for the existing
W44 investigation; Caliper provides additional offline analysis.

## Offline analysis (our side)

A snapshot is analysed with **no live process** via
`GHC.Debug.Snapshot.snapshotRun`. Analyse with the **same GHC (9.6.7)** the node
was built with so heap layout interpretation matches. The `cardano-debug` binary
carries five offline subcommands (they only read the snapshot file, no live
node):

```sh
# -hT-style closure-type census → TSV (count / total size / max per type).
# Diff two snapshots to isolate which band is growing.
cardano-debug census  leak.snapshot [census.tsv]

# Walk retainer chains of ARR_WORDS closures (payload ≥ minBytes) up to the
# GC roots, annotated with IPE source locations — what holds the byte buffers.
cardano-debug retain  leak.snapshot [maxPaths] [minBytes]

# Resolve info-table pointers, counts, bytes and source from the snapshot.
cardano-debug census-info leak.snapshot +RTS -N1 -M8G -RTS > info.tsv

# Find paths to a specific info table reported by census-info.
# Useful for thunks as well as byte buffers. Defaults to three paths.
cardano-debug retain-info leak.snapshot 0x2937058 3 +RTS -N1 -M8G -RTS

# Census every TSO by (why_blocked | threadLabel) — what the (leaked) threads
# ARE and what they are blocked on; ouroboros labels its mini-protocol threads.
cardano-debug threads leak.snapshot
```

This is the step that turns "the leak is raw byte buffers + thread stacks" into
"this structure / these threads retain them" — which the `-hi` profile alone
cannot tell us.

`retain-info` reports example paths from GC roots, not every owner or a
dominator/retained-size calculation. Use small limits: adjacent matches can be
members of the same long thunk chain. Info-table addresses must come from the
snapshot; they are not portable across builds. Eventlog labels may use a
different address convention: in the investigated GHC 9.6.7 build, the two
targeted snapshot pointers were 16 bytes above their eventlog labels. Resolve
the source using `census-info` rather than applying that offset blindly.
Analysis can consume
substantial local memory even with a small result limit. Keep output private.
