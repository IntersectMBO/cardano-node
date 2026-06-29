# Verifying the debug node build flags (`ghcDebug` variant)

This directory documents and verifies that the instrumented debug node on this
branch — `.#cardano-node-debug` and the `.#dockerImage/node-debug` image, both
built from the haskell.nix `ghcDebug` project variant (`nix/haskell.nix`) — is
compiled with the flags it is supposed to have. Two independent things:

1. **IPE** — every source-built package compiles with

       -finfo-table-map  -fdistinct-constructor-tables

   across the whole package set, equivalently to a `cabal.project` stanza of:

       package *
         ghc-options: -finfo-table-map -fdistinct-constructor-tables

   These embed Info-Table Provenance Entries so low-overhead `+RTS -hi`
   info-table heap profiles map allocations back to source, without the
   cost-centre overhead of a `-prof` build.

2. **ghc-debug** — the `ghc-debug` cabal flag took effect, linking
   `ghc-debug-stub` into the `cardano-node` exe so the heap can be snapshotted
   on demand over `$GHC_DEBUG_SOCKET` (capture it with the `cardano-debug`
   client; see `../cardano-debug/README.md`).

`ghcDebug` is defined as `infoTableMapped.appendModule {+ghc-debug}`. Because
the `ghc-debug` flag is already enabled **project-wide at plan time** (see
below), that overlay is a no-op delta and the `ghcDebug` and `infoTableMapped`
variants produce a **byte-identical** node derivation — so the IPE sweep run
against `ghcDebug` also covers `infoTableMapped`. Set `VARIANT=infoTableMapped`
to check that variant by name.

## The `ghc-debug` flag is plan-wide (so the stub is in *every* build)

The flag is set in `cabal.project` (`package cardano-node` / `flags: +ghc-debug`),
not only via the haskell.nix module. It has to be: a flag that adds a dependency
(`ghc-debug-stub`) must be set at **plan** time, because the
`--exact-configuration --dependency` list is computed from the plan; setting it
only via an `appendModule` module flag is too late (the stub would be missing at
configure). The consequence is that the stub is linked into **every** node built
off this branch — including the default `.#cardano-node` — and stays **dormant
until `GHC_DEBUG_SOCKET` is set** (`GhcDebugWrap.withGhcDebugIfEnabled` gates on
the env var at runtime), so an ordinary node behaves exactly like an
uninstrumented one. There is therefore **no negative control for the stub**: the
checker confirms it is present in both the `ghcDebug` and the default exe.

The IPE flags, by contrast, are *not* plan-wide — they are added by the
`infoTableMapped` overlay — so the default variant is a valid IPE negative
control (it must NOT carry them).

## haskell.nix gotcha covered by the sweep

The top-level `ghcOptions` is applied as a per-component **default** and is
**overridden (not appended)** by any explicit
`packages.<name>.components.<c>.ghcOptions`. So most packages get the IPE flags,
but a package that sets its own component `ghcOptions` would silently lose them
(this bit `plutus-core` in the 11.0.1 IPE work). The `infoTableMapped` overlay
re-adds the flags at that same per-component path so they merge by
concatenation; the `--all` sweep is what proves no package slipped through.

## Ground truth: grep the derivation, not the config

Verify with the **realized derivation**:

    nix derivation show <component-attr> | grep -- -finfo-table-map
    nix derivation show <node-exe-attr>  | grep -- ghc-debug-stub

Do **not** use
`…ghcDebug.pkg-set.config.packages.<pkg>.components.<c>.ghcOptions` as the IPE
check. The project-wide `ghcOptions` is applied at the component-builder layer
and is only *sometimes* surfaced in that config attr, so it produces **false
negatives** — e.g. it omits the flags for `cardano-node:library` even though the
built library derivation plainly carries them (see `evidence/`). The `.drv` is
authoritative. (The `-DGHC_DEBUG` cpp-option is likewise not reliably surfaced
in the drv JSON, which is why the stub check keys on the `ghc-debug-stub` build
input instead.)

## Why not the build plan?

`ghcOptions` is added by an `appendModule` overlay *after* dependency
resolution, so the haskell.nix plan is byte-identical for the IPE and non-IPE
variants — the plan cannot reflect the flags. cabal's `plan.json` records Cabal
`flags` and dependency resolution, not `ghc-options`. So a plan diff shows
nothing here. Use the `.drv` (Nix) or the actual ghc invocation
(`cabal build -v2 | grep`) instead.

## What is and isn't covered (IPE; same as cabal `package *`)

- **Covered:** every package compiled from source — local packages
  (`cardano-node`, `cardano-tracer`, …) *and* dependencies (SRP deps like
  `ouroboros-consensus`, ledger, and Hackage deps like `aeson`).
- **Not covered:** only the packages haskell.nix takes prebuilt from GHC, i.e.
  `pkg-set.config.nonReinstallablePkgs` — `base`, `ghc-prim`, `ghc-bignum`,
  `integer-gmp`, `integer-simple`, `rts`, plus the GHC-internal libraries since
  `reinstallableLibGhc = false`. These ship prebuilt and are never recompiled,
  so they carry no IPE map. cabal `package *` has the identical limitation.
  Getting IPE into them needs a custom IPE-built GHC.

### "Skip" in `--all` is not "missing the flags"

A `SKIP` means the sweep had nothing to check for that entry — never that a
source-built component lacks the flags. `--all` resolves the two cases the bare
`library` attr can't reach:

- ambiguous `multiple-versions` names (e.g. `text-2.0.2` and `text-2.1.4` both
  exist) fall back to the built version-qualified attr ⇒ `FLAGGED via-versioned`.
- names with no library (exe-/tool-only, e.g. `alex`, `happy`) fall back to
  their exe component(s) ⇒ `FLAGGED via-exe`.

Remaining `SKIP`s: `boot-nonreinstallable` (the only genuinely not-from-source
set), `no-built-version`, `no-library-or-exe`, `eval-error`.

## Files

- `verify-ipe-flags.sh` — authoritative checker (`nix` only, no `jq`). Default
  run does representative `.drv` checks: IPE flags across a local package, an
  SRP dep, a ledger dep, a Hackage dep, the `cardano-node` exe, plus the IPE
  negative control (the normal, non-IPE build must NOT have the flags); then the
  ghc-debug-stub checks on the `ghcDebug` and default `cardano-node` exes (both
  must have the stub). Options:
    - `--all` — exhaustive (slow) IPE sweep: enumerates every `hsPkgs` entry and
      classifies each (`FLAGGED` / `MISSING` / `SKIP`), auto-writing three
      artifacts into a timestamped dir (default `ipe-verification/ipe-all-<ts>/`):
      `report.txt` (per-package), `summary.txt` (per-DRV STATUS + store path),
      and `dump/` (each checked drv JSON). Takes many minutes.
    - `--out DIR` — write the `--all` outputs under `DIR`.
  Env: `SYS` (default `x86_64-linux`), `PROJ`
  (default `.#legacyPackages.$SYS.cardanoNodeProject`),
  `VARIANT` (default `ghcDebug`; e.g. `infoTableMapped`).
- `evidence/cardano-node.exe.ghcdebug.drv.json` — `nix derivation show` of the
  `cardano-node` exe in the `ghcDebug` variant. `grep -- -finfo-table-map` shows
  the IPE flags and `grep -- ghc-debug-stub` shows the stub dependency, in one
  drv. (This is the component whose *config* attr falsely omits the IPE flags —
  proof the drv is the reliable source.)
- `evidence/ouroboros-consensus.library.ghcdebug.drv.json` — same, for a pure
  dependency, proving the IPE flags reach deps (the `package *` property).

## Running

From the flake root (this worktree directory):

    ./ipe-verification/verify-ipe-flags.sh             # fast, representative
    ./ipe-verification/verify-ipe-flags.sh --all       # exhaustive; auto-writes report+summary+dump
    ./ipe-verification/verify-ipe-flags.sh --all --out /tmp/ipe-run   # exhaustive, outputs under /tmp/ipe-run
    VARIANT=infoTableMapped ./ipe-verification/verify-ipe-flags.sh    # check the IPE-only variant by name

Requires `nix` (flakes) only. Quick manual spot check of the saved evidence:

    grep -o -- '-finfo-table-map\|-fdistinct-constructor-tables' \
      ipe-verification/evidence/ouroboros-consensus.library.ghcdebug.drv.json | sort -u
    grep -oE 'ghc-debug-stub-lib-ghc-debug-stub-[0-9.]+' \
      ipe-verification/evidence/cardano-node.exe.ghcdebug.drv.json | sort -u

## End-to-end (functional) confirmation

The derivation checks prove the flags are in the build. To confirm they took
effect in the shipped binary:

- **IPE:** run the node and check that a `-hi` profile resolves allocations to
  source across multiple packages (including dependencies):

      cardano-node run … +RTS -hi -l-agu -RTS
      # dep modules (Ouroboros.Consensus.*, Cardano.Ledger.*, Data.Aeson.*) should resolve.

- **ghc-debug:** set `GHC_DEBUG_SOCKET`, run the node, and capture a snapshot
  with the client (`cardano-debug snapshot <out> <sock>`); a successful capture
  proves the stub is live. See `../cardano-debug/README.md`.
