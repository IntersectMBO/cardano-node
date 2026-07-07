#!/usr/bin/env bash

set -euo pipefail

CARDANO_CLI="./result/bin/cardano-cli"
SPEC_DIR="out-dir/zero"
OUT_BASE="out-dir"

BFTS=(0 3)
POOLS=(0 1 5)
STAKE=(0 1 10)
DREP_KEYS=(0 1 2)
STUFFED=(0 1 10)
UTXO_KEYS=(0 1 2)
TOTAL_SUPPLY=(0 1 1000000)
DELEGATED_SUPPLY=(0 1 500000)

count=0
skipped=0

for bfts in "${BFTS[@]}"; do
  for pools in "${POOLS[@]}"; do
    for stake in "${STAKE[@]}"; do
      for dreps in "${DREP_KEYS[@]}"; do
        for stuffed in "${STUFFED[@]}"; do
          for utxo in "${UTXO_KEYS[@]}"; do
            for total in "${TOTAL_SUPPLY[@]}"; do
              for delegated in "${DELEGATED_SUPPLY[@]}"; do
                if (( delegated > total )); then
                  continue
                fi

                name="bfts-${bfts}-pools-${pools}-stake-${stake}-dreps-${dreps}-stuffed-${stuffed}-utxo-${utxo}-total-${total}-delegated-${delegated}"
                outdir="${OUT_BASE}/${name}"

                if [[ -d "$outdir" ]]; then
                  skipped=$((skipped + 1))
                  continue
                fi

                mkdir -p "$outdir"

                if "$CARDANO_CLI" conway genesis create-testnet-data \
                  --committee-keys 0 \
                  --spec-shelley "$SPEC_DIR/shelley.json" \
                  --spec-alonzo "$SPEC_DIR/alonzo.json" \
                  --spec-conway "$SPEC_DIR/conway.json" \
                  --genesis-keys "$bfts" \
                  --pools "$pools" \
                  --stake-delegators "$stake" \
                  --drep-keys "$dreps" \
                  --stuffed-utxo "$stuffed" \
                  --utxo-keys "$utxo" \
                  --total-supply "$total" \
                  --delegated-supply "$delegated" \
                  --testnet-magic 42 \
                  --start-time 1970-01-01T00:00:00Z \
                  --out-dir "$outdir" \
                  2>"$outdir/_stderr.txt"; then
                  count=$((count + 1))
                  echo "OK: $name"
                else
                  rc=$?
                  echo "FAIL: $name (exit $rc)"
                  cat "$outdir/_stderr.txt"
                fi

              done
            done
          done
        done
      done
    done
  done
done

echo ""
echo "Done: $count generated, $skipped skipped (already existed)"

