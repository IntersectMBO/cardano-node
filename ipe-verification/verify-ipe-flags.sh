#!/usr/bin/env bash
#
# Verify the build flags of the instrumented debug node on this branch
# (`.#cardano-node-debug`, built from the haskell.nix `ghcDebug` project
# variant). Two things must hold:
#
#   1. IPE: every source-built package compiles with BOTH
#          -finfo-table-map  -fdistinct-constructor-tables
#      i.e. the Nix build is equivalent to a cabal.project stanza of:
#          package *
#            ghc-options: -finfo-table-map -fdistinct-constructor-tables
#      (so `+RTS -hi` info-table heap profiles map allocations to source).
#
#   2. ghc-debug: the `ghc-debug` cabal flag took effect, linking
#      `ghc-debug-stub` into the cardano-node exe (so the heap can be
#      snapshotted over $GHC_DEBUG_SOCKET). On this branch the flag is enabled
#      PROJECT-WIDE at plan time (cabal.project), so the stub is in EVERY node
#      build -- including the default `.#cardano-node` -- and stays dormant
#      until the socket env is set. There is therefore no negative control for
#      the stub (by design); we confirm it is present in both the ghcDebug and
#      the default exe.
#
# The `ghcDebug` variant is `infoTableMapped.appendModule {+ghc-debug}`; since
# the flag is already plan-wide the two variants produce a byte-identical node
# derivation, so the IPE sweep below (run against `ghcDebug`) also covers
# `infoTableMapped`. Set VARIANT=infoTableMapped to check that one explicitly.
#
# ---------------------------------------------------------------------------
# GROUND TRUTH = the realized derivation:
#
#     nix derivation show <component-attr> | grep -- -finfo-table-map
#     nix derivation show <node-exe-attr>  | grep -- ghc-debug-stub
#
# Do NOT use `...pkg-set.config.packages.<pkg>.components.<c>.ghcOptions` -- the
# project-wide `ghcOptions` is applied at the component-builder layer and is
# only sometimes surfaced there (FALSE NEGATIVES, e.g. cardano-node:library).
#
# Boot/global GHC packages (base, ghc-prim, ghc-bignum, integer-gmp, rts, ...)
# are prebuilt and never recompiled, so they carry no IPE map. Identical to
# cabal `package *`, which also cannot reflag them.
# ---------------------------------------------------------------------------
#
# Usage:
#   ./verify-ipe-flags.sh                 # fast: representative checks + control
#   ./verify-ipe-flags.sh --all           # SLOW: classify every hsPkgs entry
#   ./verify-ipe-flags.sh --all --out DIR # write outputs under DIR instead of the default
#
# With --all, all four artifacts are written automatically into a unique
# timestamped directory (default ./ipe-verification/ipe-all-<timestamp>/, or
# DIR if --out is given):
#   report.txt    per-package, space-aligned columns: <status> <reason> <package>
#   summary.txt   per-DRV,     space-aligned columns: <STATUS> /nix/store/...drv
#   overview.txt  the short end-of-run summary (reason counts + FLAGGED/MISSING
#                 totals); also printed to stdout. Keeps it from being lost in
#                 scrollback behind the long report/summary files.
#   dump/         each checked component's `nix derivation show` JSON
#
# Reasons: FLAGGED (has-flags / via-exe / via-versioned), MISSING (missing-flags),
# SKIP (boot-nonreinstallable / no-built-version / no-library-or-exe / eval-error).
#
# Env:  SYS (default x86_64-linux), PROJ (default .#legacyPackages.$SYS.cardanoNodeProject),
#       VARIANT (default ghcDebug; e.g. infoTableMapped)
# Requires: nix (flakes). No jq. Run from the flake root (this worktree dir).
set -uo pipefail

SYS="${SYS:-x86_64-linux}"
PROJ="${PROJ:-.#legacyPackages.${SYS}.cardanoNodeProject}"
VARIANT="${VARIANT:-ghcDebug}"
IPE="${PROJ}.${VARIANT}.hsPkgs"
FLAGS=(-finfo-table-map -fdistinct-constructor-tables)
STUB="ghc-debug-stub"   # the dep linked by the `ghc-debug` cabal flag

ALL=0; OUT=""
while [ $# -gt 0 ]; do
  case "$1" in
    --all)     ALL=1; shift ;;
    --out)     OUT="${2:?--out requires a directory}"; shift 2 ;;
    --out=*)   OUT="${1#--out=}"; shift ;;
    -h|--help) sed -n '2,62p' "$0"; exit 0 ;;
    *)         echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

rc=0
ERRF="$(mktemp)"; trap 'rm -f "$ERRF"' EXIT
SWEEP=0

# --all auto-produces report + summary + overview + dump under one timestamped dir.
REPORT=""; SUMMARY=""; OVERVIEW=""; DUMP=""
if [ "$ALL" = 1 ]; then
  : "${OUT:=ipe-verification/ipe-all-$(date +%Y%m%d-%H%M%S)}"
  DUMP="$OUT/dump"
  REPORT="$OUT/report.txt"
  SUMMARY="$OUT/summary.txt"
  OVERVIEW="$OUT/overview.txt"
  mkdir -p "$DUMP"
  : > "$SUMMARY"
fi

slug() { printf '%s' "$1" | tr -c 'A-Za-z0-9._-' '_'; }

has_both() {  # <drv-json> -> yes|no  (both IPE flags present)
  local d="$1" f
  for f in "${FLAGS[@]}"; do grep -q -- "$f" <<<"$d" || { echo no; return; }; done
  echo yes
}

# drv_state <attr> <dump-slug> -> flagged | missing | empty
# Dump + per-DRV summary are written only during the sweep (SWEEP=1), so the
# representative checks (incl. the deliberately-flagless control) are excluded.
drv_state() {
  local attr="$1" dslug="${2:-}" out st dp
  out="$(nix derivation show "$attr" 2>"$ERRF")" || out=""
  [ -z "$out" ] && { echo empty; return; }
  if [ "$(has_both "$out")" = yes ]; then st=flagged; else st=missing; fi
  if [ "$SWEEP" = 1 ] && [ -n "$dslug" ]; then
    [ -n "$DUMP" ] && printf '%s\n' "$out" > "$DUMP/${dslug}.drv.json"
    if [ -n "$SUMMARY" ]; then
      # Old nix: keys are full /nix/store/...drv. Newer nix wraps in
      # {"derivations":{"<bare-name>.drv":...}} with no store prefix.
      dp="$(grep -oE '/nix/store/[^"]+\.drv' <<<"$out" | head -1)"
      if [ -z "$dp" ]; then
        dp="$(grep -oE '"derivations":\{"[^"]+\.drv"' <<<"$out" | grep -oE '[^"{:]+\.drv' | head -1)"
        [ -n "$dp" ] && dp="/nix/store/$dp"
      fi
      printf '%-7s  %s\n' "$(printf '%s' "$st" | tr 'a-z' 'A-Z')" "$dp" >> "$SUMMARY"
    fi
  fi
  echo "$st"
}

# classify_name <pkg-name> -> echoes "<STATUS>\t<REASON>"
classify_name() {
  local n="$1" st err v vers e exes had
  st="$(drv_state "${IPE}.\"${n}\".components.library" "lib.$(slug "$n")")"
  case "$st" in
    flagged) printf 'FLAGGED\thas-flags'; return ;;
    missing) printf 'MISSING\tmissing-flags'; return ;;
  esac
  err="$(cat "$ERRF")"
  case "$err" in
    *"Multiple versions for"*)
      vers="$(grep -oE '"[0-9][^"]*"' <<<"$err" | tr -d '"')"; had=no
      for v in $vers; do
        case "$(drv_state "${IPE}.\"${n}-${v}\".components.library" "lib.$(slug "${n}-${v}")")" in
          flagged) had=yes ;; missing) printf 'MISSING\tmissing-flags'; return ;;
        esac
      done
      [ "$had" = yes ] && printf 'FLAGGED\tvia-versioned' || printf 'SKIP\tno-built-version'
      return ;;
    *"does not provide attribute"*|*"attribute '"*"' missing"*)
      exes="$(nix eval --raw "${IPE}.\"${n}\".components.exes" \
                --apply 'es: builtins.concatStringsSep " " (builtins.attrNames es)' 2>/dev/null)" || exes=""
      [ -z "$exes" ] && { printf 'SKIP\tno-library-or-exe'; return; }
      had=no
      for e in $exes; do
        case "$(drv_state "${IPE}.\"${n}\".components.exes.\"${e}\"" "exe.$(slug "${n}.${e}")")" in
          flagged) had=yes ;; missing) printf 'MISSING\tmissing-flags'; return ;;
        esac
      done
      [ "$had" = yes ] && printf 'FLAGGED\tvia-exe' || printf 'SKIP\tno-library-or-exe'
      return ;;
    *"has no value defined"*) printf 'SKIP\tboot-nonreinstallable'; return ;;
    *)                        printf 'SKIP\teval-error'; return ;;
  esac
}

# representative IPE check: <label> <attr> <expect yes|no>
check() {
  local label="$1" attr="$2" expect="$3" st
  case "$(drv_state "$attr")" in
    flagged) st=yes ;;
    missing) st=no  ;;
    *)       echo "ERROR [$label] could not evaluate: $attr"; rc=1; return ;;
  esac
  if [ "$st" = "$expect" ]; then echo "PASS  [$label] ipe=$st"
  else echo "FAIL  [$label] ipe=$st (expected $expect)  <- $attr"; rc=1; fi
}

# ghc-debug stub check: <label> <attr> <expect yes|no>
# Greps the realized drv for the ghc-debug-stub build input.
check_stub() {
  local label="$1" attr="$2" expect="$3" out st
  out="$(nix derivation show "$attr" 2>"$ERRF")" || out=""
  if [ -z "$out" ]; then echo "ERROR [$label] could not evaluate: $attr"; rc=1; return; fi
  if grep -q -- "$STUB" <<<"$out"; then st=yes; else st=no; fi
  if [ "$st" = "$expect" ]; then echo "PASS  [$label] ghc-debug-stub=$st"
  else echo "FAIL  [$label] ghc-debug-stub=$st (expected $expect)  <- $attr"; rc=1; fi
}

echo "## Representative authoritative checks (variant=${VARIANT})"
echo "# IPE flags across the source-built set + negative control:"
check "IPE local: cardano-node library"          "${IPE}.cardano-node.components.library"            yes
check "IPE local: cardano-node exe"              "${IPE}.cardano-node.components.exes.cardano-node"  yes
check "IPE SRP dep: ouroboros-consensus library" "${IPE}.ouroboros-consensus.components.library"     yes
check "IPE ledger dep: cardano-ledger-core lib"  "${IPE}.cardano-ledger-core.components.library"     yes
check "IPE hackage dep: aeson library"           "${IPE}.aeson.components.library"                   yes
check "IPE CONTROL: normal ouroboros-consensus"  "${PROJ}.hsPkgs.ouroboros-consensus.components.library" no
echo "# ghc-debug stub linked by the +ghc-debug flag (plan-wide on this branch,"
echo "# so present in BOTH the debug and the default node exe -- no neg. control):"
check_stub "stub: ${VARIANT} cardano-node exe"   "${IPE}.cardano-node.components.exes.cardano-node"  yes
check_stub "stub: default cardano-node exe"      "${PROJ}.hsPkgs.cardano-node.components.exes.cardano-node" yes

if [ "$ALL" = 1 ]; then
  echo
  echo "## Exhaustive IPE sweep of every hsPkgs entry (SLOW; resolves versioned + exe components)"
  echo "## variant=${VARIANT}; outputs -> ${OUT}/"
  mapfile -t names < <(
    nix eval --raw "$IPE" --apply 'ps: builtins.concatStringsSep "\n" (builtins.attrNames ps)' 2>/dev/null
  )
  [ "${#names[@]}" -gt 0 ] || { echo "ERROR: could not enumerate hsPkgs names"; exit 1; }
  total=${#names[@]}
  echo "enumerated ${total} entries; classifying each ..."

  # Space-aligned columns: status (max "FLAGGED"=7), reason (max
  # "boot-nonreinstallable"=21), then the package name.
  printf '%-7s  %-21s  %s\n' status reason package > "$REPORT"
  declare -A C=()
  missing=()
  i=0
  SWEEP=1   # enable dump + per-DRV summary writing (representative checks excluded)
  for n in "${names[@]}"; do
    i=$((i + 1))
    line="$(classify_name "$n")"
    status="${line%%$'\t'*}"; reason="${line#*$'\t'}"
    printf '%-7s  %-21s  %s\n' "$status" "$reason" "$n" >> "$REPORT"
    C["$reason"]=$(( ${C["$reason"]:-0} + 1 ))
    if [ "$status" = MISSING ]; then missing+=("$n"); rc=1; printf '\n  MISSING flags: %s\n' "$n"; fi
    # flagged + missing + skipped == processed (i), so the line is unambiguous.
    fl=$(( ${C[has-flags]:-0} + ${C[via-exe]:-0} + ${C[via-versioned]:-0} ))
    if [ -t 2 ]; then
      printf '\r  progress %d/%d  flagged=%d missing=%d skipped=%d  (%s)\033[K' \
        "$i" "$total" "$fl" "${#missing[@]}" "$(( i - fl - ${#missing[@]} ))" "$n" >&2
    elif [ $((i % 100)) -eq 0 ]; then
      printf '  progress %d/%d  flagged=%d missing=%d skipped=%d\n' \
        "$i" "$total" "$fl" "${#missing[@]}" "$(( i - fl - ${#missing[@]} ))" >&2
    fi
  done
  [ -t 2 ] && printf '\r\033[K' >&2

  echo
  # The short overview block goes to BOTH stdout and overview.txt (the long
  # report.txt/summary.txt are easy to lose this in; `tee` keeps a copy).
  flagged_total=$(( ${C[has-flags]:-0} + ${C[via-exe]:-0} + ${C[via-versioned]:-0} ))
  {
    echo "## Summary  (variant=${VARIANT})"
    echo "entries enumerated: ${total}"
    printf '  %-22s %s\n' "reason" "count"
    printf '  %-22s %s\n' "----------------------" "-----"
    for r in has-flags via-exe via-versioned missing-flags boot-nonreinstallable no-built-version no-library-or-exe eval-error; do
      [ -n "${C[$r]:-}" ] && printf '  %-22s %d\n' "$r" "${C[$r]}"
    done
    echo
    echo "FLAGGED (source-built, both flags): ${flagged_total}"
    echo "MISSING (source-built, lacks flag): ${#missing[@]}"
    [ "${#missing[@]}" -gt 0 ] && printf '    %s\n' "${missing[@]}"
    echo
    echo "outputs written under ${OUT}/:"
    echo "  report.txt    ($(grep -c . "$REPORT") lines incl header)   per-package status/reason"
    echo "  summary.txt   ($(grep -c . "$SUMMARY") lines)              per-DRV STATUS + /nix/store path"
    echo "  overview.txt                       this short summary"
    echo "  dump/         ($(ls -1 "$DUMP" 2>/dev/null | wc -l) files)            each checked drv JSON"
  } | tee "$OVERVIEW"
fi

echo
if [ "$rc" -eq 0 ]; then echo "RESULT: OK"; else echo "RESULT: FAILURES (see above)"; fi
exit "$rc"
