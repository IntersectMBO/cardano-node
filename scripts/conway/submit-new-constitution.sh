#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

# Make sure you have run mkfiles.sh first to create the stake pool key with stake

conwaydir=scripts/conway
stakepoolvkey=example/pools/cold1.vkey

# 2. Create new constitution governance action

cardano-cli governance action create-action create-constitution \
  --governance-action-deposit 1000000 \
  --stake-verification-key-file $stakepoolvkey \
  --constitution "Give all the money to Jordan" \
  --out-file $conwaydir/test.constitution
  
# 4. Build tx with proposal


