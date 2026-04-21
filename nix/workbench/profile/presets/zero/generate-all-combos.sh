#!/usr/bin/env bash

set -euo pipefail

CARDANO_CLI="./result/bin/cardano-cli"
SPEC_DIR="out-dir/zero"
OUT_BASE="out-dir"

TOTAL_SUPPLY=(0 1 1000000)
DELEGATED_SUPPLY=(0 1 500000)
UTXO_KEYS=(0 1 2)
POOLS=(0 1 5)
STAKE=(0 1 10)
DREP_KEYS=(0 1 2)
STUFFED=(0 1 10)

count=0
skipped=0

for total in "${TOTAL_SUPPLY[@]}"; do
  for delegated in "${DELEGATED_SUPPLY[@]}"; do
    if (( delegated > total )); then
      continue
    fi
    for utxo in "${UTXO_KEYS[@]}"; do
      for pools in "${POOLS[@]}"; do
        for stake in "${STAKE[@]}"; do
          for drep in "${DREP_KEYS[@]}"; do
            for stuffed in "${STUFFED[@]}"; do

              name="total-${total}-delegated-${delegated}-utxo-${utxo}-pools-${pools}-stake-${stake}-drep-${drep}-stuffed-${stuffed}"
              outdir="${OUT_BASE}/${name}"

              if [[ -d "$outdir" ]]; then
                skipped=$((skipped + 1))
                continue
              fi

              mkdir -p "$outdir"

              if ! "$CARDANO_CLI" latest genesis create-testnet-data \
                --out-dir "$outdir" \
                --spec-shelley "$SPEC_DIR/shelley-genesis.json" \
                --spec-alonzo "$SPEC_DIR/alonzo-genesis.json" \
                --spec-conway "$SPEC_DIR/conway-genesis.json" \
                --genesis-keys 0 \
                --pools "$pools" \
                --stake-delegators "$stake" \
                --committee-keys 0 \
                --drep-keys "$drep" \
                --stuffed-utxo "$stuffed" \
                --utxo-keys "$utxo" \
                --total-supply "$total" \
                --delegated-supply "$delegated" \
                --testnet-magic 42 \
                --start-time 1970-01-01T00:00:00Z \
                2>"$outdir/_stderr.txt"; then
                echo "FAIL: $name (exit $?)"
                cat "$outdir/_stderr.txt"
              else
                count=$((count + 1))
                echo "OK: $name"
              fi

            done
          done
        done
      done
    done
  done
done

echo ""
echo "Done: $count generated, $skipped skipped (already existed)"

