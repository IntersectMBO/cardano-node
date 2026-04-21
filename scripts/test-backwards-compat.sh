#!/usr/bin/env bash
# Test cardano-testnet against released cardano-node/cardano-cli binaries.
#
# Usage:
#   ./scripts/test-backwards-compat.sh [--min-version VERSION] [--timeout SECS]
#                                       [--cache-dir DIR] [--skip-prereleases]
#
# Examples:
#   ./scripts/test-backwards-compat.sh --min-version 9.2.0
#   ./scripts/test-backwards-compat.sh --min-version 10.1.1 --timeout 120
#
# Requires: gh (GitHub CLI), cabal, tar, standard coreutils
# Assumes:  x86_64-linux

set -euo pipefail

REPO="IntersectMBO/cardano-node"
MIN_VERSION="9.0.0"
TIMEOUT=90
CACHE_DIR="${HOME}/.cache/cardano-testnet-compat"
SKIP_PRERELEASES=true

while [[ $# -gt 0 ]]; do
  case "$1" in
    --min-version)   MIN_VERSION="$2"; shift 2 ;;
    --timeout)       TIMEOUT="$2"; shift 2 ;;
    --cache-dir)     CACHE_DIR="$2"; shift 2 ;;
    --include-prereleases) SKIP_PRERELEASES=false; shift ;;
    --skip-prereleases)    SKIP_PRERELEASES=true; shift ;;
    -h|--help)
      sed -n '2,/^$/{ s/^# //; s/^#$//; p }' "$0"
      exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

mkdir -p "$CACHE_DIR"

# --- helpers ----------------------------------------------------------------

# Compare two semver strings: returns 0 if $1 >= $2
version_ge() {
  printf '%s\n%s\n' "$2" "$1" | sort -t. -k1,1n -k2,2n -k3,3n -C
}

# Download and extract a release tarball, caching the result.
# Sets CARDANO_NODE and CARDANO_CLI on success, returns 1 on failure.
fetch_release() {
  local ver="$1"
  local dest="$CACHE_DIR/$ver"

  if [[ -x "$dest/bin/cardano-node" && -x "$dest/bin/cardano-cli" ]]; then
    export CARDANO_NODE="$dest/bin/cardano-node"
    export CARDANO_CLI="$dest/bin/cardano-cli"
    return 0
  fi

  rm -rf "$dest"
  mkdir -p "$dest"

  # Naming changed at 10.6.2: -linux.tar.gz → -linux-amd64.tar.gz
  local tarball
  for suffix in "linux-amd64.tar.gz" "linux.tar.gz"; do
    tarball="cardano-node-${ver}-${suffix}"
    if gh release download "$ver" --repo "$REPO" \
         --pattern "$tarball" --dir "$dest" 2>/dev/null; then
      break
    fi
    tarball=""
  done

  if [[ -z "$tarball" ]]; then
    echo "  ⚠ no x86_64-linux tarball found" >&2
    rm -rf "$dest"
    return 1
  fi

  tar xzf "$dest/$tarball" -C "$dest" 2>/dev/null
  rm -f "$dest/$tarball"

  if [[ ! -x "$dest/bin/cardano-node" || ! -x "$dest/bin/cardano-cli" ]]; then
    echo "  ⚠ tarball missing cardano-node or cardano-cli" >&2
    rm -rf "$dest"
    return 1
  fi

  export CARDANO_NODE="$dest/bin/cardano-node"
  export CARDANO_CLI="$dest/bin/cardano-cli"
}

# Run cardano-testnet and return 0 on success.
# Saves full output to $LOG_DIR/<version>.log for post-mortem.
# Runs from a fresh temp dir so the default "testnet/" workdir is isolated.
run_testnet() {
  local ver="$1"
  local logfile="$LOG_DIR/${ver}.log"
  local rundir
  rundir=$(mktemp -d "/tmp/testnet-compat-${ver}-XXXXXX")

  {
    echo "=== $ver ==="
    echo "CARDANO_NODE=$CARDANO_NODE"
    echo "CARDANO_CLI=$CARDANO_CLI"
    echo "rundir=$rundir"
    echo "---"
  } > "$logfile"

  local rc=0
  # Run in a subshell with its own process group so we can kill everything
  set -m  # enable job control for process groups
  ( cd "$rundir" && \
    timeout --kill-after=5 "$TIMEOUT" "$CARDANO_TESTNET_BIN" cardano \
  ) >> "$logfile" 2>&1 &
  local pid=$!
  wait "$pid" 2>/dev/null || rc=$?
  set +m

  echo "--- exit code: $rc" >> "$logfile"

  local ok=1
  if [[ $rc -eq 0 || $rc -eq 124 ]]; then
    # 0 = clean exit, 124 = timeout killed a running testnet (started fine)
    ok=0
  fi

  # Kill any cardano-node processes spawned in this rundir
  pgrep -f "cardano-node.*${rundir}" 2>/dev/null | xargs -r kill 2>/dev/null || true
  sleep 1
  pgrep -f "cardano-node.*${rundir}" 2>/dev/null | xargs -r kill -9 2>/dev/null || true
  rm -rf "$rundir"
  return $ok
}

# --- main -------------------------------------------------------------------

LOG_DIR=$(mktemp -d "/tmp/testnet-compat-logs-XXXXXX")

echo "Building cardano-testnet from working tree…"
cabal build cardano-testnet 2>&1 | tail -1
CARDANO_TESTNET_BIN=$(cabal list-bin cardano-testnet 2>/dev/null)
echo "Binary: $CARDANO_TESTNET_BIN"

# Collect version list (semver-shaped tags only)
mapfile -t ALL_VERSIONS < <(
  gh release list --repo "$REPO" --limit 100 --json tagName,isPrerelease \
    --jq '.[] | select(.tagName | test("^[0-9]+\\.[0-9]+\\.[0-9]+$"))
              | select(if '"$SKIP_PRERELEASES"' then .isPrerelease == false else true end)
              | .tagName' \
  | sort -t. -k1,1n -k2,2n -k3,3n
)

VERSIONS=()
for v in "${ALL_VERSIONS[@]}"; do
  version_ge "$v" "$MIN_VERSION" && VERSIONS+=("$v")
done

if [[ ${#VERSIONS[@]} -eq 0 ]]; then
  echo "No releases found >= $MIN_VERSION"
  exit 1
fi

echo ""
echo "Testing ${#VERSIONS[@]} releases (>= $MIN_VERSION, timeout ${TIMEOUT}s each)"
echo ""

# Results table
declare -A RESULTS
declare -A NODE_VERS
declare -A CLI_VERS

for ver in "${VERSIONS[@]}"; do
  printf "%-14s " "$ver"

  if ! fetch_release "$ver"; then
    RESULTS[$ver]="SKIP"
    NODE_VERS[$ver]="—"
    CLI_VERS[$ver]="—"
    printf "SKIP (no binary)\n"
    continue
  fi

  NODE_VERS[$ver]=$("$CARDANO_NODE" --version 2>/dev/null | head -1 | awk '{print $2}' || echo "?")
  CLI_VERS[$ver]=$("$CARDANO_CLI" --version 2>/dev/null | head -1 | awk '{print $2}' || echo "?")

  if run_testnet "$ver"; then
    RESULTS[$ver]="OK"
    printf "OK   (node %s, cli %s)\n" "${NODE_VERS[$ver]}" "${CLI_VERS[$ver]}"
  else
    RESULTS[$ver]="FAIL"
    printf "FAIL (node %s, cli %s)\n" "${NODE_VERS[$ver]}" "${CLI_VERS[$ver]}"
  fi
done

# --- summary table ----------------------------------------------------------

echo ""
echo "============================================================"
echo " Compatibility results (cardano-testnet from working tree)"
echo "============================================================"
printf "%-14s  %-14s  %-14s  %s\n" "Release" "node" "cli" "Result"
printf "%-14s  %-14s  %-14s  %s\n" "-------" "----" "---" "------"
for ver in "${VERSIONS[@]}"; do
  printf "%-14s  %-14s  %-14s  %s\n" \
    "$ver" "${NODE_VERS[$ver]}" "${CLI_VERS[$ver]}" "${RESULTS[$ver]}"
done

# Show failure details
has_failures=false
for ver in "${VERSIONS[@]}"; do
  if [[ "${RESULTS[$ver]}" == "FAIL" ]]; then
    if ! $has_failures; then
      echo ""
      echo "============================================================"
      echo " Failure details (last 20 lines per version)"
      echo "============================================================"
      has_failures=true
    fi
    echo ""
    echo "--- $ver ---"
    tail -20 "$LOG_DIR/${ver}.log" 2>/dev/null | sed 's/^/  /'
  fi
done

echo ""
echo "Full logs: $LOG_DIR/"
