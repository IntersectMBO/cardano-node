# helper functions
wait-for-mempool() (
  while true; do
      [ "$(cardano-cli latest query tx-mempool info | jq -re .numberOfTxs || true)" = "0" ] && break;
    echo "Waiting for the mempool to settle..."
    sleep 2
  done
  echo "Mempool is clear..."
)

wait-for-tip() (
  TYPE="$1"
  TARGET="$2"

  while true; do
    VALUE=$(jq -re ".$TYPE" <<< "$(cardano-cli latest query tip)")
      [ "$VALUE" = "$TARGET" ] && break;
    echo "Waiting for the tip to reach $TYPE $TARGET (currently $VALUE)..."
    sleep 2
  done
  echo "Tip has reached $TYPE $TARGET."
)

export ENV_DIR="/home/nicolas/tweag/cardano-node/testnet"

export TESTNET_MAGIC=42
export PAYMENT_KEY="$ENV_DIR/utxo-keys/utxo1/utxo"

export CARDANO_NODE_NETWORK_ID="42"
export CARDANO_NODE_SOCKET_PATH="$ENV_DIR/socket/node1/sock"

export GOV_ACTION_DEPOSIT="1000000"

# Switch from Protocol Version 10.0 to 11.0

# Get the current epoch
export CURRENT_EPOCH=$(
  cardano-cli latest query tip \
    | jq -r '.epoch'
)

# It's n+2 for the next era, because the hard fork will be enacted in the next epoch, and the next era will start in the epoch after that.
export NEXT_ERA_EPOCH=$((CURRENT_EPOCH + 2))

# Submit a Dijkstra hard fork
echo "Submitting a Dijkstra hard fork action..."
PROPOSAL_ARGS=("--protocol-major-version" "11" "--protocol-minor-version" "0")
ACTION="create-hardfork" \
  STAKE_KEY="$ENV_DIR/stake-delegators/delegator1/staking" \
  nix run github:input-output-hk/cardano-parts#job-submit-gov-action -- "${PROPOSAL_ARGS[@]}"
wait-for-mempool

export ACTION_TX_ID=$(
  cardano-cli latest query gov-state \
    | jq -r '.proposals | map(select(.proposalProcedure.govAction.tag == "HardForkInitiation")) | .[0].actionId.txId'
)

echo "Submitting the drep-1 vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=drep \
  VOTE_KEY="$ENV_DIR/drep-keys/drep1/drep" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool

echo "Submitting the drep-2 vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=drep \
  VOTE_KEY="$ENV_DIR/drep-keys/drep2/drep" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool

echo "Submitting the drep-3 vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=drep \
  VOTE_KEY="$ENV_DIR/drep-keys/drep3/drep" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool

echo "Submitting the pool vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=spo \
  VOTE_KEY="$ENV_DIR/pools-keys/pool1/cold" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool

# Wait for the next era
wait-for-tip epoch $NEXT_ERA_EPOCH

# Switch from Protocol Version 11.0 to 12.0

export OLD_GOV_ACTION_IX=$(
  cardano-cli latest query gov-state \
    | jq -r '.nextRatifyState.nextEnactState.prevGovActionIds.HardFork.govActionIx'
)

echo "Submitting a Dijkstra hard fork action..."
PROPOSAL_ARGS=(
  "--protocol-major-version" "12"
  "--protocol-minor-version" "0"
  "--prev-governance-action-tx-id" "$ACTION_TX_ID"
  "--prev-governance-action-index" "$OLD_GOV_ACTION_IX"
)
ACTION="create-hardfork" \
  STAKE_KEY="$ENV_DIR/stake-delegators/delegator1/staking" \
  nix run github:input-output-hk/cardano-parts#job-submit-gov-action -- "${PROPOSAL_ARGS[@]}"
wait-for-mempool

export ACTION_TX_ID=$(
  cardano-cli latest query gov-state \
    | jq -r '.proposals | map(select(.proposalProcedure.govAction.tag == "HardForkInitiation")) | .[0].actionId.txId'
)

echo "Submitting the drep-1 vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=drep \
  VOTE_KEY="$ENV_DIR/drep-keys/drep1/drep" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool

echo "Submitting the drep-2 vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=drep \
  VOTE_KEY="$ENV_DIR/drep-keys/drep2/drep" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool

echo "Submitting the drep-3 vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=drep \
  VOTE_KEY="$ENV_DIR/drep-keys/drep3/drep" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool

echo "Submitting the pool vote for the Dijkstra hard fork..."
  DECISION=yes \
  ROLE=spo \
  VOTE_KEY="$ENV_DIR/pools-keys/pool1/cold" \
  nix run github:input-output-hk/cardano-parts#job-submit-vote
wait-for-mempool
