# cardano-node ghc-debug snapshot (leios-ipe)

A debug variant of `cardano-node` that can produce **ghc-debug heap snapshots**
for **offline** retainer analysis, on top of the IPE build this branch already
carries. Intended to find what retains the growing `stg_ARR_WORDS_info` (raw
byte buffers) and `stg_STACK_info` (thread stacks) closures that dominate the
`-hi` heap profile but carry no allocation-site identity of their own — only
their *retainers* do, and a snapshot lets us walk up to them.

This is the same approach as the customer OCI image (`jl/oci-ghc-debug`), but
exposed as **flake exes** for the nix/systemd Leios deployment so it can be tried
out here first.

## What this branch exposes

- `cardano-node-ghc-debug` — the node built `infoTableMapped` (IPE) **and** with
  the `ghc-debug` cabal flag: it links `ghc-debug-stub` and wraps `main` with
  `withGhcDebug`, serving the ghc-debug protocol on `$GHC_DEBUG_SOCKET`.
- `cardano-ghc-debug-snapshot` — a tiny headless client that connects to that
  socket, pauses the process, writes a self-contained snapshot, and exits.
  Snapshotting is *client-side* in ghc-debug, which is why this exe is needed.

The default `cardano-node` exe is unchanged (the `ghc-debug` flag is off by
default), so only machines that opt into `cardano-node-ghc-debug` are affected.

## Deploy on Leios

Point the per-machine node package at `cardano-node-ghc-debug` (the same way a
machine opts into `cardano-node-ipe`), and set the socket in the service
environment so the stub and the snapshotter agree:

```
GHC_DEBUG_SOCKET=/run/cardano-node/ghc-debug.socket
```

(If unset, `withGhcDebug` falls back to a socket under the XDG data dir; setting
it explicitly is clearer for a systemd unit and lets the snapshotter find it.)

## Capture a snapshot

```sh
# on the node host, as the user running the node (needs access to the socket):
cardano-ghc-debug-snapshot /var/lib/cardano-node/heap-$(date -u +%Y%m%dT%H%M%SZ).snapshot "$GHC_DEBUG_SOCKET"
```

> **Capture early, not at the ceiling.** A snapshot is ~heap-sized: an 8 GB heap
> near OOM means an ~8 GB+ file, a multi-minute stop-the-world pause, and a real
> risk the capture itself tips the process over OOM. The leak's retainer
> structure is identical at 3 GB and at 8 GB (steady linear accumulation), so
> capture at a moderate heap (e.g. ~3 GB), ideally **two** snapshots at different
> sizes to diff — the delta in the retainer set *is* the leak. Ensure disk
> headroom > heap and memory headroom (cgroup limit − RSS) before capturing.

## Analyse offline

A snapshot loads with **no live process** via `GHC.Debug.Snapshot.snapshotRun`,
analysed with the **same GHC (9.6.7)** the node was built with. Either drive
`ghc-debug-brick` over the snapshot interactively, or script a retainer walk with
`ghc-debug-client` from the leaking `ARR_WORDS` / `STACK` closures up to the
nearest IPE-named retainer — which is what turns "byte buffers + thread stacks"
into "this structure / these threads retain them."
