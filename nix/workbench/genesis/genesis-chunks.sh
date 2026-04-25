# shellcheck shell=bash
#
# Dataset-cached genesis backend.
# Used when WB_DATASET_GENESIS=1.
#
# Implements the backend interface:
#   profile-cache-key-input-chunks, profile-cache-key-chunks,
#   spec-chunks, pool-relays-chunks,
#   genesis-create-chunks,
#   derive-from-cache-chunks
#
# Three-level cache:
#
#   Level 1: dataset cache ($cache_dir/genesis/dataset/)
#     Keyed by dataset params (pools, delegators, supply, etc.) + hash.
#     Shared across profiles with the same dataset. Expensive to create.
#     Stores:
#     - byron.dataset.json   {bootStakeholders, heavyDelegation, nonAvvmBalances}
#     - shelley.dataset.json {initialFunds, staking, maxLovelaceSupply} (can be 500MB+)
#     - conway.dataset.json  {initialDReps}                             (can be 200MB+)
#     - byron-gen-command/ drep-keys/ pools-keys/ stake-delegators/ utxo-keys/
#
#   Level 2: protocol cache ($cache_dir/genesis/protocol/)
#     Keyed by hash of genesis.shelley + genesis.alonzo + genesis.conway +
#     genesis.dijkstra from the profile. Cheap to create.
#     Stores the spec-derived parts WITHOUT dataset fields and WITHOUT
#     systemStart (which is per-run):
#     - byron.protocol.json    (spec minus dataset fields)
#     - shelley.protocol.json  (spec minus dataset fields and systemStart)
#     - alonzo.protocol.json   (complete spec)
#     - conway.protocol.json   (spec minus dataset fields)
#     - dijkstra.protocol.json (complete spec)
#
#   Level 3: genesis cache ($cache_dir/genesis/)
#     Keyed by: profile name + protocol hash + dataset hash.
#     Assembled from level 1 + level 2 + systemStart using cat/sed/printf.
#     No `jq` on any large file. No pretty-printing.

# -- Backend interface ---------------------------------------------------------

# The genesis cache key combines profile name, protocol params hash, and
# dataset hash.
profile-cache-key-input-chunks() {
  local profile_json=$1
  local proto_hash dataset_hash
  proto_hash=$(jq -c -S '{
    byron:    ( .genesis.byron    // {} ),
    shelley:  ( .genesis.shelley        ),
    alonzo:   ( .genesis.alonzo         ),
    conway:   ( .genesis.conway   // {} ),
    dijkstra: ( .genesis.dijkstra // {} )
  }' "$profile_json" | sha1sum | cut -c-7)
  dataset_hash=$(dataset-cache-key "$profile_json" | sha1sum | cut -c-7)
  echo "${proto_hash}-${dataset_hash}"
}

profile-cache-key-chunks() {
  local profile_json=$1
  local name
  name=$(jq -r '.name' "$profile_json")
  echo "${name}-$(profile-cache-key-input-chunks "$profile_json")"
}

spec-chunks() {
  spec-jq "$@";
}

pool-relays-chunks() {
  pool-relays-jq "$@";
}

# Ensure sub-caches exist, assemble the genesis cache entry with symlinks
# to the dataset cache for key directories, and pre-assemble the genesis
# JSON files that don't change per run. Only genesis-shelley.json (which
# contains the per-run systemStart) is left for finalise-cache-entry-chunks.
genesis-create-chunks() {
  local profile_json=$1
  local outdir=$2

  mkdir -p "$outdir"

  local dataset_key dataset_entry proto_key proto_entry
  dataset_key=$(dataset-cache-key   "$profile_json")
  protocol_key=$(protocol-cache-key "$profile_json")
  # Resolve $outdir: $cache_dir/genesis/KEY as $cache_dir/genesis/{dataset|protocol}.
  dataset_entry=$(dataset-cache-ensure   "$profile_json" "$outdir/../dataset/$dataset_key")
  protocol_entry=$(protocol-cache-ensure "$profile_json" "$outdir/../protocol/$protocol_key")

  ln -sf "$dataset_entry"  "$outdir/dataset"
  ln -sf "$protocol_entry" "$outdir/protocol"

  # -- genesis JSON files that don't change per run: assemble once -------------

  # Byron and Shelley change per run (startTime/systemStart) so they are
  # assembled in derive-from-cache-chunks instead.

  # Alonzo: protocol parameters only, no dataset fields.
  cat "$protocol_entry/alonzo.protocol.json" > "$outdir/genesis.alonzo.json"

  # Conway: protocol parameters + dataset.
  {
    cat "$protocol_entry/conway.protocol.json" | sed 's/}$//'
    printf ','
    tail -c +2 "$dataset_entry/conway.dataset.json"
  } > "$outdir/genesis.conway.json"

  # Dijkstra: protocol parameters only, no dataset fields.
  cat "$protocol_entry/dijkstra.protocol.json" > "$outdir/genesis.dijkstra.json"

  info genesis "genesis cache entry created in $outdir"
}

# Populate the run directory from the genesis cache entry.
# Everything is symlinked except genesis.shelley.json which is assembled
# fresh per run (because of systemStart). Pure cat/sed/printf for shelley.
derive-from-cache-chunks() {
  local profile_json=${1:?$usage}
  local timing=${2:?$usage}
  local cache_entry=${3:?$usage}
  local outdir=${4:?$usage} # output directory (run dir's genesis/, e.g. run/current/genesis).

  mkdir -p "$outdir"

  local preset
  preset=$(profile preset "$profile_json")
  if [[ -n "$preset" ]]; then
    progress "genesis" "instantiating from preset $(with_color white "$preset"):  $cache_entry"
    cp -f "$cache_entry"/genesis*.json "$outdir"
    return
  fi

  progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

  ln -s "$profile_json"                "$outdir"/profile.json
  ln -s "$cache_entry"                 "$outdir"/cache-entry
  ln -s "$cache_entry"/cache.key       "$outdir"
  ln -s "$cache_entry"/cache.key.input "$outdir"
  ln -s "$cache_entry"/layout.version  "$outdir"

  # Key directories as symlinks from cache (create-testnet-data output names).
  for keydir in byron-gen-command drep-keys pools-keys stake-delegators utxo-keys
  do
    ln -s "$cache_entry/dataset/$keydir" "$outdir/$keydir"
  done

  # Genesis JSON files that don't change per run as symlinks from cache.
  ln -s "$cache_entry"/genesis.alonzo.json   "$outdir"/genesis.alonzo.json
  ln -s "$cache_entry"/genesis.conway.json   "$outdir"/genesis.conway.json
  ln -s "$cache_entry"/genesis.dijkstra.json "$outdir"/genesis.dijkstra.json

  # Byron and Shelley change per run (startTime / systemStart).
  # Assembled from protocol + dataset + per-run timing using cat/sed/printf.
  local system_start_epoch system_start
  system_start_epoch=$(jq -r '.start' <<<"$timing")
  system_start=$(jq -r '.systemStart' <<<"$timing")

  # Byron (always small)
  {
    cat "$cache_entry/protocol/byron.protocol.json" | sed 's/}$//'
    printf ',"startTime":%s' "$system_start_epoch"
    printf ','
    tail -c +2 "$cache_entry/dataset/byron.dataset.json"
  } > "$outdir/genesis.byron.json"

  # Shelley (can be 500MB+)
  {
    cat "$cache_entry/protocol/shelley.protocol.json" | sed 's/}$//'
    printf ',"systemStart":"%s"' "$system_start"
    printf ','
    tail -c +2 "$cache_entry/dataset/shelley.dataset.json"
  } > "$outdir/genesis.shelley.json"
}

# ==============================================================================
# Dataset cache (level 1)
# ==============================================================================

# Build the dataset cache key from a profile JSON.
dataset-cache-key() {
  local profile_json=$1

  local params_key
  params_key=$(jq -r '
      "pools-"             + (.composition.n_pools          | tostring)
    + "-stake-delegators-" + (.derived.delegators_effective | tostring)
    + "-drep-keys-"        + (.genesis.dreps                | tostring)
    + "-stuffed-utxo-"     + (.derived.utxo_stuffed         | tostring)
    + "-utxo-keys-"        + (.genesis.utxo_keys            | tostring)
    + "-total-supply-"     + (.derived.supply_total         | tostring)
    + "-delegated-supply-" + (.derived.supply_delegated     | tostring)
    + "-testnet-magic-"    + (.genesis.network_magic        | tostring)
  ' "$profile_json")

  local hash
  hash=$(echo "$params_key" | sha1sum | cut -c-7)

  echo "${params_key}-${hash}"
}

# Ensure a dataset cache entry exists. If not, create it.
# Returns the cache entry path on stdout.
dataset-cache-ensure() {
  local profile_json=$1
  local outdir=$2
  local zero_dir="$global_basedir/profile/presets/zero/genesis"

  if test -d "$outdir"
  then
    info genesis "dataset cache hit: $outdir"
  else
    info genesis "dataset cache miss: $outdir"
    # Create in a temporary directory and move if no errors.
    local tmpdir
    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT
    # Extract dataset parameters from canonical profile fields
    local era pools delegators dreps stuffed utxo_keys total_supply delegated_supply magic
    era=$(jq              --raw-output '.era'                          "$profile_json")
    pools=$(jq            --raw-output '.composition.n_pools'          "$profile_json")
    delegators=$(jq       --raw-output '.derived.delegators_effective' "$profile_json")
    dreps=$(jq            --raw-output '.genesis.dreps'                "$profile_json")
    stuffed=$(jq          --raw-output '.derived.utxo_stuffed'         "$profile_json")
    utxo_keys=$(jq        --raw-output '.genesis.utxo_keys'            "$profile_json")
    total_supply=$(jq     --raw-output '.derived.supply_total'         "$profile_json")
    delegated_supply=$(jq --raw-output '.derived.supply_delegated'     "$profile_json")
    magic=$(jq            --raw-output '.genesis.network_magic'        "$profile_json")
    # Run `create-testnet-data` with zeroed specs (see profile/presets/zero).
    progress genesis "creating dataset cache: $outdir"
    cardano-cli "$era" genesis create-testnet-data        \
      --spec-shelley     "$zero_dir/shelley-genesis.json" \
      --spec-alonzo      "$zero_dir/alonzo-genesis.json"  \
      --spec-conway      "$zero_dir/conway-genesis.json"  \
      --out-dir          "$tmpdir"                        \
      --start-time       "1970-01-01T00:00:00Z"           \
      --testnet-magic    "$magic"                         \
      --total-supply     "$total_supply"                  \
      --delegated-supply "$delegated_supply"              \
      --utxo-keys        "$utxo_keys"                     \
      --pools            "$pools"                         \
      --stake-delegators "$delegators"                    \
      --drep-keys        "$dreps"                         \
      --stuffed-utxo     "$stuffed"
    # Extract dataset fields as compact JSON (one jq pass per file)
      jq -c '{bootStakeholders, heavyDelegation, nonAvvmBalances}' "$tmpdir/byron-genesis.json"   \
    > "$tmpdir/byron.dataset.json"
      jq -c '{initialFunds, staking, maxLovelaceSupply}'           "$tmpdir/shelley-genesis.json" \
    > "$tmpdir/shelley.dataset.json"
      jq -c '{initialDReps}'                                       "$tmpdir/conway-genesis.json"  \
    > "$tmpdir/conway.dataset.json"
    # Ensure all key directories exist (even if `create-testnet-data` didn't create them)
    for keydir in byron-gen-command drep-keys pools-keys stake-delegators utxo-keys
    do
      mkdir -p "$tmpdir/$keydir"
    done
    # TODO: remove the raw genesis files produced by create-testnet-data ???
    # Move the whole tmpdir to the cache entry
    mkdir -p "$(dirname "$outdir")"
    mv "$tmpdir" "$outdir"
    trap - EXIT
    info genesis "dataset cached: $outdir"
  fi

  echo "$outdir"
}

# ==============================================================================
# Protocol cache (level 2)
# ==============================================================================

# Protocol cache key: slotLength, epochLength, activeSlotsCoeff (human-readable)
# followed by a hash of all protocol parameter fields.
protocol-cache-key() {
  local profile_json=$1
  local slot epoch active hash
  slot=$(jq   -r '.genesis.shelley.slotLength'       "$profile_json")
  epoch=$(jq  -r '.genesis.shelley.epochLength'      "$profile_json")
  active=$(jq -r '.genesis.shelley.activeSlotsCoeff' "$profile_json")
  hash=$(jq -c -S '{
    byron:    ( .genesis.byron    // {} ),
    shelley:  ( .genesis.shelley        ),
    alonzo:   ( .genesis.alonzo         ),
    conway:   ( .genesis.conway   // {} ),
    dijkstra: ( .genesis.dijkstra // {} )
  }' "$profile_json" | sha1sum | cut -c-7)
  echo "slot-${slot}-epoch-${epoch}-active-${active}-${hash}"
}

# Ensure a protocol cache entry exists. If not, create it.
# Returns the cache entry path on stdout.
protocol-cache-ensure() {
  local profile_json=$1
  local outdir=$2

  if test -d "${outdir}"
  then
    info genesis "protocol parameters cache hit: ${outdir}"
  else
    info genesis "protocol parameters cache miss: ${outdir}"
    mkdir -p "${outdir}"
    # Generate specs from profile and strip dataset fields + systemStart.
    # All specs are small (<10KB), using `jq` is fine here.
    # MUST be compact (-c) because the merge step uses sed 's/}$//' which
    # only works on single-line JSON.
      genesis spec byron    "${profile_json}"                                        \
    | jq -c 'del(.bootStakeholders, .heavyDelegation, .nonAvvmBalances, .startTime)' \
    > "${outdir}/byron.protocol.json"
      genesis spec shelley  "${profile_json}"                                \
    | jq -c 'del(.initialFunds, .staking, .maxLovelaceSupply, .systemStart)' \
    > "${outdir}/shelley.protocol.json"
      genesis spec alonzo   "${profile_json}" \
    | jq -c '.'                               \
    > "${outdir}/alonzo.protocol.json"
      genesis spec conway   "${profile_json}" \
    | jq -c 'del(.initialDReps)'              \
    > "${outdir}/conway.protocol.json"
      genesis spec dijkstra "${profile_json}" \
    | jq -c '.'                               \
    > "${outdir}/dijkstra.protocol.json"
    info genesis "protocol parameters cached: ${outdir}"
  fi

  echo "${outdir}"
}

