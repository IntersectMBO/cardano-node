#!/usr/bin/env bash
#
# Integration test: submit transactions via Ogmios.
#
# Usage:
#   bash bench/tx-generator/test-ogmios.sh [ogmios-flake-ref]
#
# Binaries are resolved from the ogmios flake:
#   ogmios        ← packages.ogmios-exe  (hsPkgs.ogmios.components.exes.ogmios)
#   cardano-*     ← inputs.cardano-node.packages.{cardano-node,cardano-cli,cardano-testnet}
#   tx-generator  ← locally-built (cabal)
#
set -euo pipefail

OGMIOS_FLAKE="${1:-github:IntersectMBO/ogmios/jmiller/dijkstra-integration}"
TESTNET_MAGIC=42
OGMIOS_PORT=11337

# --- Resolve binaries from the ogmios flake ---
echo "=== Resolving nix packages ==="
echo "  ogmios flake: $OGMIOS_FLAKE"

# ogmios: packages.ogmios-exe
OGMIOS=$(nix build "${OGMIOS_FLAKE}#ogmios" --no-link --print-out-paths)

# cardano-node tools: input ref from the ogmios flake (uses tag, not hash)
CN_FLAKE=$(nix flake metadata "${OGMIOS_FLAKE}" --json \
  | jq -r '.locks.nodes["cardano-node"].original
           | "github:\(.owner)/\(.repo)/\(.ref)"')
echo "  cardano-node flake: $CN_FLAKE"

CARDANO_NODE=$(nix build "${CN_FLAKE}#cardano-node" --no-link --print-out-paths)/bin/cardano-node
CARDANO_CLI=$(nix build "${CN_FLAKE}#cardano-cli" --no-link --print-out-paths)/bin/cardano-cli
CARDANO_TESTNET=$(nix build "${CN_FLAKE}#cardano-testnet" --no-link --print-out-paths)/bin/cardano-testnet

# tx-generator: locally-built (resolve to absolute path)
TX_GENERATOR=$(cabal list-bin tx-generator 2>/dev/null \
  || find "$(pwd)/dist-newstyle" -name tx-generator -type f -perm +111 | head -1)

for bin in OGMIOS CARDANO_TESTNET CARDANO_NODE CARDANO_CLI TX_GENERATOR; do
    path="${!bin}"
    if [ ! -x "$path" ]; then
        echo "ERROR: $bin not found or not executable at: $path"
        exit 1
    fi
    echo "  $bin: $path"
done

# --- Set up work directory ---
WORKDIR=$(mktemp -d /tmp/ogmios-test.XXXXXX)
LOGS_DIR="$WORKDIR/logs"
TESTNET_DIR="$WORKDIR/testnet"
mkdir -p "$LOGS_DIR"

echo ""
echo "=== Ogmios tx-generator integration test ==="
echo "Work directory: $WORKDIR"

cleanup() {
    echo ""
    echo "=== Cleaning up ==="
    [ -n "${OGMIOS_PID:-}" ] && kill "$OGMIOS_PID" 2>/dev/null && echo "Stopped ogmios (PID $OGMIOS_PID)"
    [ -n "${TESTNET_PID:-}" ] && kill "$TESTNET_PID" 2>/dev/null && echo "Stopped cardano-testnet (PID $TESTNET_PID)"
    echo "Logs available at: $LOGS_DIR"
}
trap cleanup EXIT

# --- 1. Start cardano-testnet ---
echo ""
echo "--- Starting cardano-testnet ---"
CARDANO_CLI="$CARDANO_CLI" CARDANO_NODE="$CARDANO_NODE" \
  "$CARDANO_TESTNET" cardano \
    --testnet-magic "$TESTNET_MAGIC" \
    --output-dir "$TESTNET_DIR" \
    > "$LOGS_DIR/cardano-testnet.stdout" 2> "$LOGS_DIR/cardano-testnet.stderr" &
TESTNET_PID=$!
echo "cardano-testnet PID: $TESTNET_PID"

# --- 2. Wait for node socket ---
echo "Waiting for node socket..."
SOCKET_PATH=""
for i in $(seq 1 120); do
    SOCKET_PATH=$(find "$TESTNET_DIR" -name "sock" 2>/dev/null | head -1 || true)
    if [ -n "$SOCKET_PATH" ] && [ -S "$SOCKET_PATH" ]; then
        break
    fi
    SOCKET_PATH=""
    sleep 1
done

if [ -z "$SOCKET_PATH" ]; then
    echo "FAILED: node socket not found after 120s"
    tail -20 "$LOGS_DIR/cardano-testnet.stderr"
    exit 1
fi
echo "Found node socket: $SOCKET_PATH"

# --- 3. Find node config ---
CONFIG_PATH=""
for name in configuration.yaml configuration.json config.json; do
    CONFIG_PATH=$(find "$TESTNET_DIR" -name "$name" 2>/dev/null | head -1 || true)
    [ -n "$CONFIG_PATH" ] && break
done
if [ -z "$CONFIG_PATH" ]; then
    echo "FAILED: node config not found"
    exit 1
fi
echo "Found node config: $CONFIG_PATH"

# --- 4. Start ogmios ---
echo ""
echo "--- Starting ogmios ---"
"$OGMIOS" \
  --node-socket "$SOCKET_PATH" \
  --node-config "$CONFIG_PATH" \
  --port "$OGMIOS_PORT" \
  --log-level error \
  > "$LOGS_DIR/ogmios.stdout" 2> "$LOGS_DIR/ogmios.stderr" &
OGMIOS_PID=$!
echo "ogmios PID: $OGMIOS_PID"

echo "Waiting for ogmios on port $OGMIOS_PORT..."
for i in $(seq 1 60); do
    if nc -z 127.0.0.1 "$OGMIOS_PORT" 2>/dev/null; then break; fi
    sleep 1
done
if ! nc -z 127.0.0.1 "$OGMIOS_PORT" 2>/dev/null; then
    echo "FAILED: ogmios not accepting connections after 60s"
    tail -20 "$LOGS_DIR/ogmios.stderr"
    exit 1
fi
echo "ogmios is ready on port $OGMIOS_PORT"

# --- 5. Create tx-generator config ---
echo ""
echo "--- Creating tx-generator config ---"
SIG_KEY=$(find "$TESTNET_DIR" -name "utxo.skey" -path "*/utxo1/*" 2>/dev/null | head -1 || true)
if [ -z "$SIG_KEY" ]; then
    echo "FAILED: signing key not found"
    exit 1
fi

CONFIG_FILE="$WORKDIR/tx-generator-config.json"
cat > "$CONFIG_FILE" << EOF
{
  "tx_count": 10,
  "tps": 2,
  "inputs_per_tx": 2,
  "outputs_per_tx": 2,
  "tx_fee": 212345,
  "min_utxo_value": 1000000,
  "add_tx_size": 39,
  "init_cooldown": 5,
  "era": "Conway",
  "keepalive": 30,
  "debugMode": true,
  "plutus": null,
  "sigKey": "$SIG_KEY",
  "ogmiosUrl": "ws://127.0.0.1:$OGMIOS_PORT"
}
EOF

BENCH_ADDR="addr_test1vz4qz2ayucp7xvnthrx93uhha7e04gvxttpnuq4e6mx2n5gzfw23z"

query_utxo_count() {
    "$CARDANO_CLI" query utxo \
      --address "$BENCH_ADDR" \
      --testnet-magic "$TESTNET_MAGIC" \
      --socket-path "$SOCKET_PATH" \
      --out-file /dev/stdout | jq 'length'
}

# --- 6. UTxO count before ---
echo ""
echo "--- UTxOs at benchmark address before tx-generator ---"
BEFORE=$(query_utxo_count)
echo "  $BEFORE UTxOs at $BENCH_ADDR"

# --- 7. Run tx-generator via Ogmios ---
echo ""
echo "--- Running tx-generator with Ogmios submission ---"
( cd "$WORKDIR" && "$TX_GENERATOR" json_highlevel "$CONFIG_FILE" \
  --testnet-config-dir "$TESTNET_DIR" \
  > "$LOGS_DIR/tx-generator.stdout" 2> "$LOGS_DIR/tx-generator.stderr" )
TX_EXIT=$?

echo ""
echo "--- tx-generator stdout (last 30 lines) ---"
tail -30 "$LOGS_DIR/tx-generator.stdout"
echo ""
echo "--- tx-generator stderr (last 20 lines) ---"
tail -20 "$LOGS_DIR/tx-generator.stderr"

if [ "$TX_EXIT" -ne 0 ]; then
    echo ""
    echo "=== FAILED: tx-generator exit code $TX_EXIT ==="
    exit "$TX_EXIT"
fi

# --- 8. Wait for txs to land in blocks, then count UTxOs ---
echo ""
echo "--- Waiting for transactions to be included in blocks ---"
for i in $(seq 1 30); do
    AFTER=$(query_utxo_count)
    echo "  [$i] $AFTER UTxOs at benchmark address"
    if [ "$AFTER" -gt "$BEFORE" ]; then
        sleep 5
        AFTER=$(query_utxo_count)
        echo "  [final] $AFTER UTxOs at benchmark address"
        break
    fi
    sleep 2
done

echo ""
echo "=== Results ==="
echo "  UTxOs before: $BEFORE"
echo "  UTxOs after:  $AFTER"
echo "  New UTxOs:    $((AFTER - BEFORE))"

exit "$TX_EXIT"
