# cardano-node ghc-debug snapshot image

A debug build of `cardano-node` that can produce **ghc-debug heap snapshots** for
**offline** retainer analysis. The intended use is to find what is retaining the
growing `stg_ARR_WORDS_info` (raw byte buffers) and `stg_STACK_info` (thread
stacks) closures that dominate the `-hi` heap profile but, by construction, carry
no allocation-site identity of their own — their *retainers* do, and a snapshot
lets us walk up to them.

## Design: capture in the field, analyse at home

The image does exactly one thing in the field: **capture a self-contained
snapshot file** and let you ship it back. There is deliberately **no automated
analysis or report generation baked in** — capture is the only step that has to
run correctly on a one-shot, and analysis is far more valuable run offline where
it can be re-run against the same snapshot as many times as needed.

Two halves of ghc-debug:

- **Debuggee** = the node, built with the `ghc-debug` cabal flag (links
  `ghc-debug-stub`, wraps `main` with `withGhcDebug`). It serves the ghc-debug
  protocol on the unix socket named by `GHC_DEBUG_SOCKET`.
- **Debugger** = `cardano-debug` (this package), a tiny headless client. Its
  `snapshot` subcommand connects to that socket, pauses the process, writes a
  snapshot, and exits. Snapshotting is *client-side* in ghc-debug — the stub
  alone cannot write a snapshot, which is why this client is bundled. The same
  binary also analyses a shipped-back snapshot offline (`census` / `retain` /
  `threads`, see below).

The node is additionally built `infoTableMapped` (`-finfo-table-map
-fdistinct-constructor-tables`), so closures in the snapshot map back to source
where GHC records it.

## Build the image

```sh
nix build .#dockerImage/node-debug
docker load -i result    # loads ghcr.io/intersectmbo/cardano-node-debug:<gitrev>
```

The image is published under a distinct repo name (`cardano-node-debug`) so it
can never clobber a production `cardano-node` tag. On this branch the `ghc-debug`
cabal flag is enabled project-wide (it has to be, so `ghc-debug-stub` lands in
the install plan), so the stub is linked into *every* build — but it stays
**dormant unless `GHC_DEBUG_SOCKET` is set**, so a node with no socket configured
behaves exactly like an uninstrumented one. What makes this image distinct is
that it is built `infoTableMapped` (see below), bundles the `cardano-debug`
client, and presets the socket env.

## Run it

Use it like the normal node image (same `NETWORK` / `run` entrypoint modes). The
image presets `GHC_DEBUG_SOCKET=/ipc/ghc-debug.socket`, which lives on the
standard `/ipc` volume.

## Capture a snapshot

```sh
# default output path: /data/ghc-debug-<UTC timestamp>.snapshot
docker exec <container> take-snapshot

# or choose your own path
docker exec <container> take-snapshot /data/leak-2026-06-26.snapshot
```

> **Operational caveat:** capturing pauses the node (stop-the-world) for the
> duration of the snapshot — seconds to tens of seconds on a multi-GB heap, long
> enough to miss slots / drop peers. Capture deliberately, ideally during low
> activity, not on a tight timer.

`take-snapshot` is a thin wrapper around `cardano-debug snapshot <out>
<socket>`; you can call that directly too.

## Capture from outside the container

The snapshot client caches the **entire heap in its own RAM** before writing the
file, so on a memory-tight node, capturing *inside* the container can push its
cgroup toward OOM. To avoid that — or to capture from another machine — run the
client off-container. `cardano-debug` is a **fully-static (musl)** binary with no
glibc / Nix-store dependencies, so the *same* binary the image ships also runs
anywhere once copied out:

```sh
# copy it out of the image …
docker cp <container>:/usr/local/bin/cardano-debug .
# … or build/ship it standalone:
nix build .#cardano-debug     # → result/bin/cardano-debug
```

ghc-debug speaks a **unix socket** (`GHC_DEBUG_SOCKET=/ipc/ghc-debug.socket` in
the image), so point the client at that socket from off-container:

```sh
# (a) on the Docker HOST, if /ipc is a volume mount — capture RAM lands on the
#     host, NOT the container's memory cgroup:
./cardano-debug snapshot /tmp/heap.snapshot /path/to/ipc/ghc-debug.socket

# (b) from a different host — forward the socket over ssh (unix↔unix), or socat a
#     TCP port to it, then run against the forwarded socket:
ssh -L /tmp/ghcdbg.sock:/ipc/ghc-debug.socket <host>
./cardano-debug snapshot /tmp/heap.snapshot /tmp/ghcdbg.sock
```

Either way the node only pays the stop-the-world pause and streams its heap out;
the heap-sized client cache lives on whatever host runs the binary.

## Ship it back

```sh
docker cp <container>:/data/leak-2026-06-26.snapshot ./
# (or read it off the mounted /data volume) and send it to us.
```

Snapshots are self-contained but can be large (heap-sized). Compress before
sending: `zstd leak-2026-06-26.snapshot`.

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
