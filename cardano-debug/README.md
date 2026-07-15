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
```

On this branch the `ghc-debug` cabal flag is enabled project-wide (it has to be,
so `ghc-debug-stub` lands in the install plan), so the stub is linked into
*every* build — but it stays **dormant unless `GHC_DEBUG_SOCKET` is set**, so a
node with no socket configured behaves exactly like an uninstrumented one. What
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
  "$GHC_DEBUG_SOCKET"
```

> **Operational caveat:** capturing pauses the node (stop-the-world) for the
> duration of the snapshot — seconds to minutes on a multi-GB heap, long
> enough to miss slots / drop peers. Capture deliberately, ideally during low
> activity, not on a tight timer.

The snapshot client caches the **entire heap in its own RAM** before writing the
file, so make sure the host has heap-sized headroom (or run the client on
another machine, below) and disk space larger than the node's heap.

## Capture from another host

ghc-debug speaks a **unix socket**, so the client can also run off-host —
forward the socket over ssh (unix↔unix), or socat a TCP port to it, then run
against the forwarded socket:

```sh
ssh -L /tmp/ghcdbg.sock:/run/cardano-node/ghc-debug.socket <host>
cardano-debug snapshot /tmp/heap.snapshot /tmp/ghcdbg.sock
```

Either way the node only pays the stop-the-world pause and streams its heap out;
the heap-sized client cache lives on whatever host runs the binary.

## Ship it back

Snapshots are self-contained but can be large (heap-sized). Compress before
sending: `zstd leak-2026-07-15.snapshot`.

## Offline analysis (our side)

A snapshot is analysed with **no live process** via
`GHC.Debug.Snapshot.snapshotRun`. Analyse with the **same GHC (9.6.7)** the node
was built with so heap layout interpretation matches. The `cardano-debug` binary
carries three offline subcommands (they only read the snapshot file, no live
node):

```sh
# -hT-style closure-type census → TSV (count / total size / max per type).
# Diff two snapshots to isolate which band is growing.
cardano-debug census  leak.snapshot [census.tsv]

# Walk retainer chains of ARR_WORDS closures (payload ≥ minBytes) up to the
# GC roots, annotated with IPE source locations — what holds the byte buffers.
cardano-debug retain  leak.snapshot [maxPaths] [minBytes]

# Census every TSO by (why_blocked | threadLabel) — what the (leaked) threads
# ARE and what they are blocked on; ouroboros labels its mini-protocol threads.
cardano-debug threads leak.snapshot
```

This is the step that turns "the leak is raw byte buffers + thread stacks" into
"this structure / these threads retain them" — which the `-hi` profile alone
cannot tell us.
