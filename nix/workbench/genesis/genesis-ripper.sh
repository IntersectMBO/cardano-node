# shellcheck shell=bash
#
# Dataset-cached genesis backend.
# Used when WB_GENESIS_RIPPER=1.
#
# Implements the backend interface:
#   profile-cache-key-input-ripper, profile-cache-key-ripper,
#   genesis-cache-hit-ripper, genesis-create-cache-ripper, derive-from-cache-ripper
#
# Three-level cache:
#
# - Level 1: dataset cache ($cache_dir/genesis/dataset/)
#     Keyed by dataset params (pools, delegators, supply, etc.) + hash.
#     Each entry is unique and independent of any profile: any profile
#     needing this exact dataset combination reuses the entry as-is.
#     Can be expensive to create, which is the main reason the cache exists.
#     Stores:
#     - dataset.byron.json   {bootStakeholders, heavyDelegation, nonAvvmBalances}
#     - dataset.shelley.json {genDelegs, initialFunds, staking, maxLovelaceSupply}
#                            (with our profiles it can be 500MB+)
#     - dataset.conway.json  {initialDReps, delegs}
#                            (with our profiles it can be 200MB+)
#     - spec.{shelley,alonzo,conway}.json
#                            the exact specs fed to create-testnet-data (zero
#                            specs + injected minUTxOValue / dRepDeposit), kept
#                            for reproducibility.
#     - cache.key.input      the dataset-cache-key-input JSON (debugging).
#     - byron-gen-command/genesis-keys.000.key
#     - delegate-keys/delegateN/
#         { kes.skey, kes.vkey, key.skey, key.vkey
#         , opcert.cert, opcert.counter
#         , vrf.skey, vrf.vkey
#         }
#     - drep-keys/
#     - genesis-keys/genesisN/{key.skey,key.vkey}
#     - pools-keys/poolN/
#         { byron-delegate.key, byron-delegation.cert
#         , cold.skey, cold.vkey
#         , kes.skey, kes.vkey
#         , opcert.cert, opcert.counter
#         , staking-reward.skey, staking-reward.vkey
#         , vrf.skey, vrf.vkey
#         }
#     - stake-delegators/delegatorN/
#         { payment.skey, payment.vkey
#         , staking.skey, staking.vkey
#         }
#     - utxo-keys/utxoN/{utxo.skey, utxo.vkey}
#
# - Level 2: protocol cache ($cache_dir/genesis/protocol/)
#     Keyed by hash of genesis.byron + genesis.shelley + genesis.alonzo +
#     genesis.conway + genesis.dijkstra (minus dataset and timing fields) from
#     the profile.
#     Each entry is unique and independent of any profile: any profile with
#     this exact protocol params combination reuses the entry as-is.
#     Cheap to create, the cache exists mostly to make protocol-param
#     sharing across profiles visible.
#     Stores the spec-derived parts WITHOUT dataset fields and WITHOUT
#     startTime / systemStart (which are per-run):
#     - protocol.byron.json    (spec minus dataset fields and startTime)
#     - protocol.shelley.json  (spec minus dataset fields and systemStart)
#     - protocol.alonzo.json   (complete spec)
#     - protocol.conway.json   (spec minus dataset fields)
#     - protocol.dijkstra.json (complete spec)
#     - cache.key.input        the protocol-cache-key-input JSON (debugging).
#
# - Level 3: genesis cache ($cache_dir/genesis/)
#     Keyed by: profile name + protocol hash + dataset hash.
#     Placeholder for easy finding of the dataset and protocol entries used.
#
# - Final level: the genesis dir in the run directory ($run_dir/genesis).
#     Assembled from level 1 + level 2 + startTime / systemStart using only
#     simple and fast cat/sed/printf.
#     Byron, Shelley and Conway are assembled here (not symlinked):
#       - Byron   because it contains startTime.
#       - Shelley because of the dataset payload and contains systemStart.
#       - Conway  because of the dataset payload.
#     No `jq` on any large file. No pretty-printing.
#     NOT cached anywhere: the assembled byron/shelley/conway files live only
#     in the run directory and are not promoted back to the level 3 cache.
#     This applies to Conway too, even though it has no per-run timing field.
#     The user owns the assembled output and decides what to do with it.

# ==============================================================================
# -- Backend interface ---------------------------------------------------------
# ==============================================================================

# The ripper has no "layout.version" file (that is a jq-backend concept). Its
# cache layout/format version is folded into every cache key below, so a layout
# change (renamed files, new split, ...) bumps this and orphans the old entries
# automatically.
# Bump on any change to what/how the ripper caches.
genesis_ripper_layout_version="v0.0"

# The cache-key input combines protocol hash, dataset hash and layout version.
# (The profile name is prepended by profile-cache-key-ripper below.)
profile-cache-key-input-ripper() {
  local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
  local proto_hash dataset_hash
  proto_hash=$(protocol-cache-key-input-hash  "$profile_json")
  dataset_hash=$(dataset-cache-key-input-hash "$profile_json")
  echo "${proto_hash}-${dataset_hash}-${genesis_ripper_layout_version}"
}

profile-cache-key-ripper() {
  local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
  local name
  name=$(jq -r '.name' "$profile_json")
  echo "${name}-$(profile-cache-key-input-ripper "$profile_json")"
}

# Cache hit iff the versioned entry exists AND its dataset/protocol sub-caches
# still resolve. The entry is committed atomically (top-level mv), but its
# dataset/ and protocol/ are symlinks to separate sub-cache entries that may be
# pruned independently, so in that cases we report a miss to let it regenerate.
# The format version is folded into the cache key (see
# genesis_ripper_layout_version), so an old-format entry has a different path
# and won't match. Resolves its own cache key from the profile (via the
# dispatcher).
genesis-cache-hit-ripper() {
  local profile_json=${1:?}
  local genesis_cache_dir=${2:?}
  local entry
  entry="$genesis_cache_dir/$(genesis profile-cache-key "$profile_json")"
  test -d "$entry/dataset" && test -d "$entry/protocol"
}

# Build the level-3 entry in $outdir (a fresh dir; the top-level caller commits
# it with an atomic mv). Ensures the dataset and protocol sub-caches under
# $genesis_cache_dir (each atomic on its own) and links them. No layout.version
# (the ripper's format version is in the cache keys). The shared cache.key /
# cache.key.input are written by `genesis create-cache`. $3 is the genesis cache root.
genesis-create-cache-ripper() {
  local profile_json=$1
  local outdir=$2
  local genesis_cache_dir=${3:-$(envjqr 'cacheDir')/genesis}

  mkdir -p "$outdir"

  # Ensure the dataset and protocol sub-caches exist and link them in.
  local dataset_entry protocol_entry
  dataset_entry=$(dataset-cache-ensure   "$profile_json" "$genesis_cache_dir")
  ln -sf "$dataset_entry"  "$outdir/dataset"
  protocol_entry=$(protocol-cache-ensure "$profile_json" "$genesis_cache_dir")
  ln -sf "$protocol_entry" "$outdir/protocol"

  # - Byron: changes per run (startTime). Assembled in derive-from-cache-ripper.
  # - Shelley: changes per run (systemStart). Assembled in derive-from-cache-ripper.
  # - Alonzo: protocol parameters only, no dataset fields. Can simply link.
  ln -s "protocol/protocol.alonzo.json"   "$outdir/genesis.alonzo.json"
  # - Conway: protocol parameters + dataset. Assembled in derive-from-cache-ripper.
  # - Dijkstra: protocol parameters only, no dataset fields. Can simply link.
  ln -s "protocol/protocol.dijkstra.json" "$outdir/genesis.dijkstra.json"
}

# Populate the run directory from the genesis cache entry.
# Everything is symlinked except genesis.byron.json, genesis.shelley.json and
# genesis.conway.json, which are assembled fresh per run:
#   - Byron   because it contains startTime.
#   - Shelley because of the dataset payload and contains systemStart.
#   - Conway  because of the dataset payload.
# Keep it simple: cat/sed/printf.
derive-from-cache-ripper() {
  local usage="USAGE: wb genesis derive-from-cache PROFILE-JSON CACHE-ENTRY-DIR OUTDIR TIMING-JSON-EXPR"
  local profile_json=${1:?$usage}
  local cache_entry=${2:?$usage}
  local outdir=${3:?$usage} # output directory (run dir's genesis/, e.g. run/current/genesis).
  local timing=${4:?$usage}

  # Preset profiles are handled by the genesis.sh top-level `derive-from-cache`
  # dispatcher and never reach this backend.
  mkdir -p "$outdir"

  progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

  ln -s "$profile_json"                "$outdir"/profile.json
  ln -s "$cache_entry"                 "$outdir"/cache-entry
  ln -s "$cache_entry"/cache.key       "$outdir"
  ln -s "$cache_entry"/cache.key.input "$outdir"
  # No layout.version: the ripper versions its cache via the keys, not a marker.

  # Key directories as symlinks from cache (create-testnet-data output names).
  for keydir in byron-gen-command delegate-keys drep-keys genesis-keys pools-keys stake-delegators utxo-keys
  do
    ln -s "$cache_entry/dataset/$keydir" "$outdir/$keydir"
  done

  # Genesis JSON files that don't change per run as symlinks from cache.
  ln -s "$cache_entry"/genesis.alonzo.json   "$outdir"/genesis.alonzo.json
  ln -s "$cache_entry"/genesis.dijkstra.json "$outdir"/genesis.dijkstra.json

  # Byron and Shelley change per run (startTime / systemStart).
  # Assembled from protocol + dataset + per-run timing using cat/sed/printf.
  # Each protocol.*/dataset.* file is a non-empty compact JSON object, so the splice yields valid JSON.
  local system_start_epoch system_start
  system_start_epoch=$(jq -r '.start' <<<"$timing")
  system_start=$(jq -r '.systemStart' <<<"$timing")
  # Byron (always small).
  {
    sed 's/}$//' "$cache_entry/protocol/protocol.byron.json"
    printf ',"startTime":%s' "$system_start_epoch"
    printf ','
    tail -c +2 "$cache_entry/dataset/dataset.byron.json"
  } > "$outdir/genesis.byron.json"
  # Shelley (can be 500MB+).
  {
    sed 's/}$//' "$cache_entry/protocol/protocol.shelley.json"
    printf ',"systemStart":"%s"' "$system_start"
    printf ','
    tail -c +2 "$cache_entry/dataset/dataset.shelley.json"
  } > "$outdir/genesis.shelley.json"

  # Conway (can be 200MB+).
  # Only tied to a dataset + protocol. No timing.
  # When the profile leaves .genesis.conway null, protocol/protocol.conway.json
  # is the zero stub but we still need to merge the dataset side (initialDReps
  # and delegs) to produce a parseable file for the node config.
  {
    sed 's/}$//' "$cache_entry/protocol/protocol.conway.json"
    printf ','
    tail -c +2 "$cache_entry/dataset/dataset.conway.json"
  } > "$outdir/genesis.conway.json"
}

# ==============================================================================
# Dataset cache (level 1)
# ==============================================================================

# A unique directory name for a dataset cache entry.
dataset-cache-key() {
  local profile_json=$1
  # The human readable prefix.
  local key_text
  key_text=$(dataset-cache-key-text "$profile_json")
  # The hash of the dataset cache-key input (the params that determine the dataset).
  local key_hash
  key_hash=$(dataset-cache-key-input-hash "$profile_json")
  # Append the hash at the end, makes it easier to filter if needed.
  echo "${key_text}-${key_hash}"
}

# Human-readable non-unique string representing a dataset combination.
dataset-cache-key-text() {
  local profile_json=$1
  # A string with some of the most relevant parameters we use.
  # Each of the letters represent one parameter:
  # --pools:            "p"
  # --stake-delegators: "sd"
  # --drep-keys:        "dk"
  # --stuffed-utxo:     "su"
  # --utxo-keys:        "uk"
  # --total-supply:     "ts"
  jq -r                                                   \
    ' "p-"   + (.composition.n_pools          | tostring)
    + "-sd-" + (.derived.delegators_effective | tostring)
    + "-dk-" + (.genesis.dreps                | tostring)
    + "-su-" + (.derived.utxo_stuffed         | tostring)
    + "-uk-" + (.genesis.utxo_keys            | tostring)
    + "-ts-" + (.derived.supply_total         | tostring)
    '                                                     \
    "$profile_json"
}

# The cache-key input: canonical JSON of ALL dataset parameters (fixed literal
# key order, so already deterministic without `-S`). The trailing layout-version
# orphans old entries when the cache format changes (see
# genesis_ripper_layout_version).
# Written verbatim into each dataset cache entry as cache.key.input (debugging).
dataset-cache-key-input() {
  local profile_json=$1
  jq -c                                      \
    --arg v "$genesis_ripper_layout_version" \
    '{ "committee-keys":     0
     , "genesis-keys":       .composition.n_bft_hosts
     , "pools":              .composition.n_pools
     , "stake-delegators":   .derived.delegators_effective
     , "drep-keys":          .genesis.dreps
     , "stuffed-utxo":       .derived.utxo_stuffed
     , "utxo-keys":          .genesis.utxo_keys
     , "total-supply":       .derived.supply_total
     , "delegated-supply":   .derived.supply_delegated
     , "testnet-magic":      .genesis.network_magic
     , "stuffed-utxo-value": (.genesis.shelley.protocolParams.minUTxOValue // 0)
     , "drep-deposit":       (.genesis.conway.dRepDeposit // 0)
     , "layout-version":     $v
     }'                                      \
    "$profile_json"
}

# The hash of the dataset cache-key input (48 bits).
dataset-cache-key-input-hash() {
  local profile_json=$1
  dataset-cache-key-input "$profile_json" | sha1sum | cut -c-12
}

# Ensure a dataset cache entry exists. If not, create it.
# Returns the cache entry path on stdout.
dataset-cache-ensure() {
  local profile_json=$1
  local genesis_cache_dir=${2:-$(envjqr 'cacheDir')/genesis}

  local cache_entry outdir
  cache_entry=$(dataset-cache-key "$profile_json")
  outdir="$genesis_cache_dir"/dataset/"$cache_entry"

  if test -d "$outdir"
  then
    info genesis "dataset cache hit: $outdir"
  else
    info genesis "dataset cache miss: $outdir"
    # Create in a temporary directory and move if no errors.
    local tmpdir
    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT
    # Keep the cache-key input for debugging (like the level-3 genesis cache).
    dataset-cache-key-input "$profile_json" > "$tmpdir/cache.key.input"
    # Extract dataset parameters from canonical profile fields
    local bfts pools delegators dreps stuffed utxo_keys supply_t supply_d magic
    bfts=$(jq       --raw-output '.composition.n_bft_hosts'      "$profile_json")
    pools=$(jq      --raw-output '.composition.n_pools'          "$profile_json")
    delegators=$(jq --raw-output '.derived.delegators_effective' "$profile_json")
    dreps=$(jq      --raw-output '.genesis.dreps'                "$profile_json")
    stuffed=$(jq    --raw-output '.derived.utxo_stuffed'         "$profile_json")
    utxo_keys=$(jq  --raw-output '.genesis.utxo_keys'            "$profile_json")
    supply_t=$(jq   --raw-output '.derived.supply_total'         "$profile_json")
    supply_d=$(jq   --raw-output '.derived.supply_delegated'     "$profile_json")
    magic=$(jq      --raw-output '.genesis.network_magic'        "$profile_json")
    # Feed create-testnet-data the per-era zero specs (genesis zero-spec-ERA):
    # neutral specs that satisfy `--spec-{shelley,alonzo,conway}` without
    # carrying meaningful protocol params, so the output is just the DATASET
    # fields.
    # Exception: create-testnet-data bakes two protocol params into dataset
    # fields, so inject them into the specs:
    # - shelley minUTxOValue -> the lovelace value of each stuffed UTxO.
    # - conway dRepDeposit   -> the deposit recorded on each initialDReps entry.
    # Both are part of dataset-cache-key-input so the entry stays uniquely keyed.
    local min_utxo_value drep_deposit
    min_utxo_value=$(jq -r '.genesis.shelley.protocolParams.minUTxOValue // 0' "$profile_json")
    drep_deposit=$(jq   -r '.genesis.conway.dRepDeposit                  // 0' "$profile_json")
    # Shelley.
      genesis zero-spec-shelley             \
    | jq                                    \
        --argjson v "$min_utxo_value"       \
        '.protocolParams.minUTxOValue = $v' \
    > "$tmpdir/spec.shelley.json"
    # Alonzo.
      genesis zero-spec-alonzo              \
    > "$tmpdir/spec.alonzo.json"
    # Conway.
      genesis zero-spec-conway              \
    | jq                                    \
        --argjson v "$drep_deposit"         \
        '.dRepDeposit = $v'                 \
    > "$tmpdir/spec.conway.json"
    progress genesis "creating dataset cache: $outdir ($tmpdir)"
    # Use `cardano-cli latest` rather than workbench's `eraName`:
    # - The era passed to `cardano-cli genesis create-testnet-data` only selects
    #   which CLI flavour is invoked and the output is the same.
    # - The workbench's `eraName` controls which Test<Era>HardForkAtEpoch flags
    #   the node config sets (see service/nodes.nix), independently from the
    #   spec generation step here.
    cardano-cli latest genesis create-testnet-data \
      --out-dir "$tmpdir"                          \
      --spec-shelley "$tmpdir/spec.shelley.json"   \
      --spec-alonzo  "$tmpdir/spec.alonzo.json"    \
      --spec-conway  "$tmpdir/spec.conway.json"    \
      --committee-keys   0                         \
      --start-time       "1970-01-01T00:00:00Z"    \
      --genesis-keys     "$bfts"                   \
      --pools            "$pools"                  \
      --stake-delegators "$delegators"             \
      --drep-keys        "$dreps"                  \
      --stuffed-utxo     "$stuffed"                \
      --utxo-keys        "$utxo_keys"              \
      --total-supply     "$supply_t"               \
      --delegated-supply "$supply_d"               \
      --testnet-magic    "$magic"
    # Extract dataset fields as compact JSON and in only one jq pass per file.
    # MUST be compact JSON (-c) because the merge step uses `tail -c +2` which
    # only works on single-line JSON.
    # - Byron:
      jq -c                                                     \
        '{bootStakeholders, heavyDelegation, nonAvvmBalances}'  \
        "$tmpdir/byron-genesis.json"                            \
    > "$tmpdir/dataset.byron.json"
    # - Shelley:
      jq -c                                                     \
        '{genDelegs, initialFunds, staking, maxLovelaceSupply}' \
        "$tmpdir/shelley-genesis.json"                          \
    > "$tmpdir/dataset.shelley.json"
    # - Conway: Default to "{}" if cardano-cli omits initialDReps or delegs:
    #   cardano-node's parser accepts {} but rejects null.
      jq -c                                                     \
        '{ initialDReps: (.initialDReps // {})
         , delegs:       (.delegs       // {})
         }'                                                     \
        "$tmpdir/conway-genesis.json"                           \
    > "$tmpdir/dataset.conway.json"
    # Remove the not needed files produced by create-testnet-data.
    rm "$tmpdir/byron-genesis.json"
    rm "$tmpdir/byron.genesis.spec.json"
    rm "$tmpdir/shelley-genesis.json"
    rm "$tmpdir/alonzo-genesis.json"
    rm "$tmpdir/conway-genesis.json"   || true
    rm "$tmpdir/dijkstra-genesis.json" || true
    # Ensure all possible directories we may get always exist.
    # (in case `create-testnet-data` didn't create them).
    for keydir in byron-gen-command delegate-keys drep-keys genesis-keys pools-keys stake-delegators utxo-keys
    do
      mkdir -p "$tmpdir/$keydir"
    done
    # Move the whole tmpdir to the cache entry. No fake data is left behind!
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

# A unique directory name for a protocol cache entry.
protocol-cache-key() {
  local profile_json=$1
  # The human-readable prefix.
  local key_text
  key_text=$(protocol-cache-key-text "$profile_json")
  # The hash of the full resulting protocol parameters.
  local key_hash
  key_hash=$(protocol-cache-key-input-hash "$profile_json")
  # Append the hash at the end, makes it easier to filter if needed.
  echo "${key_text}-${key_hash}"
}

# Human-readable non-unique string representing a protocol parameters combination.
protocol-cache-key-text() {
  local profile_json=$1
  # A string with some of the most relevant protocol parameters.
  jq -r \
    ' .genesis as $g
    | $g.shelley as $s
    | "sl-\($s.slotLength)"
    + "-el-\($s.epochLength)"
    + "-f-\($s.activeSlotsCoeff)"
    + "-k-\($s.securityParam)"
    + "-bs-\($s.protocolParams.maxBlockBodySize)"
    + "-pv-\($s.protocolParams.protocolVersion.major)"
    + "-pe-\($g.pparamsEpoch)"
    + "-po-\( $g.pparamsOverlays
            | join("+")
            | gsub("/"; "_")
            | if . == "" then "none" else . end
            )"
    ' \
    "$profile_json"
}

# The cache-key input: the protocol spec per era MINUS the dataset and timing
# fields (the same deletions protocol-cache-ensure applies to the stored content),
# so the key identifies exactly what is cached and is shared across profiles
# that differ only in dataset/timing. Written into each entry as
# cache.key.input.
# Keep these del lists in sync with protocol-cache-ensure.
protocol-cache-key-input() {
  local profile_json=$1
  # -S sorts keys for a canonical hash, so layout-version's source position is
  # cosmetic; kept last to match dataset-cache-key-input.
  jq -c -S                                   \
    --arg v "$genesis_ripper_layout_version" \
    '{ byron:    ( .genesis.byron    // {}
                 | del(.bootStakeholders, .heavyDelegation, .nonAvvmBalances,   .startTime  )
                 )
     , shelley:  ( .genesis.shelley  // {}
                 | del(.genDelegs, .initialFunds, .staking, .maxLovelaceSupply, .systemStart)
                 )
     , alonzo:   ( .genesis.alonzo   // {} )
     , conway:   ( .genesis.conway   // {}
                 | del(.initialDReps, .delegs)
                 )
     , dijkstra: ( .genesis.dijkstra // {} )
     , "layout-version": $v
     }'                                      \
    "$profile_json"
}

# The hash of the protocol cache-key input (48 bits).
protocol-cache-key-input-hash() {
  local profile_json=$1
  protocol-cache-key-input "$profile_json" | sha1sum | cut -c-12
}

# Ensure a protocol cache entry exists. If not, create it.
# Returns the cache entry path on stdout.
protocol-cache-ensure() {
  local profile_json=$1
  local genesis_cache_dir=${2:-$(envjqr 'cacheDir')/genesis}

  local cache_entry outdir
  cache_entry=$(protocol-cache-key "$profile_json")
  outdir="$genesis_cache_dir"/protocol/"$cache_entry"

  if test -d "$outdir"
  then
    info genesis "protocol parameters cache hit: $outdir"
  else
    info genesis "protocol parameters cache miss: $outdir"
    # Create in a temporary directory and move if no errors.
    local tmpdir
    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT
    # Keep the cache-key input for debugging (like the level-3 genesis cache).
    protocol-cache-key-input "$profile_json" > "$tmpdir/cache.key.input"
    # Generate specs from profile and strip dataset fields and time fields.
    # All specs are small (<10KB), using `jq` is fine here but MUST be compact
    # JSON (-c) because the merge step uses sed 's/}$//' which only works on
    # single-line JSON.
    # - Byron: remove dataset fields and timing.
    if test "$(jq -r '.genesis.byron // "null"' "$profile_json")" != "null"
    then
        jq -c                            \
          ' .genesis.byron
          | del( .bootStakeholders
               , .heavyDelegation
               , .nonAvvmBalances
               , .startTime
               )'                        \
         "$profile_json"                 \
      > "$tmpdir/protocol.byron.json"
    else
      fatal "empty .genesis.byron in profile"
    fi
    # - Shelley: remove dataset fields and timing.
    if test "$(jq -r '.genesis.shelley // "null"' "$profile_json")" != "null"
    then
        jq -c                            \
          ' .genesis.shelley
          | del( .genDelegs
               , .initialFunds
               , .staking
               , .maxLovelaceSupply
               , .systemStart)'          \
          "$profile_json"                \
      > "$tmpdir/protocol.shelley.json"
    else
      fatal "empty .genesis.shelley in profile"
    fi
    # - Alonzo: nothing to do, only compact it.
    if test "$(jq -r '.genesis.alonzo // "null"' "$profile_json")" != "null"
    then
        jq -c                            \
          '.genesis.alonzo'              \
          "$profile_json"                \
      > "$tmpdir/protocol.alonzo.json"
    else
      fatal "empty .genesis.alonzo in profile"
    fi
    # - Conway: always emit a file because cardano-node's config parser requires
    #   ConwayGenesisFile unconditionally. When the profile has a null or empty
    #   ".genesis.conway" (e.g. pre-Chang profiles with "pparamsEpoch" < 507) we
    #   emit a "zero" stub that passes the validation.
    if test "$(jq -r '.genesis.conway // "null"' "$profile_json")" != "null"
    then
        jq -c                            \
          '.genesis.conway
          | del( .initialDReps
               , .delegs)'               \
          "$profile_json"                \
      > "$tmpdir/protocol.conway.json"
    else
        # Stub that is parseable but it should never be activated.
        # Activation check in service/nodes.nix (TestConwayHardForkAtEpoch).
        genesis zero-spec-conway > "$tmpdir/protocol.conway.json"
    fi
    # - Dijkstra: always emit a file because cardano-node's config parser requires
    #   DijkstraGenesisFile unconditionally. When the profile has a null or empty
    #   ".genesis.dijkstra" we emit a "zero" stub that passes the validation.
    if test "$(jq -r '.genesis.dijkstra // "null"' "$profile_json")" != "null"
    then
        jq -c                            \
          '.genesis.dijkstra'            \
          "$profile_json"                \
      > "$tmpdir/protocol.dijkstra.json"
    else
        # Stub that is parseable but it should never be activated.
        # Activation check in service/nodes.nix (TestDijkstraHardForkAtEpoch).
        genesis zero-spec-dijkstra > "$tmpdir/protocol.dijkstra.json"
    fi
    # Move the whole tmpdir to the cache entry. No fake data is left behind!
    mkdir -p "$(dirname "$outdir")"
    mv "$tmpdir" "$outdir"
    trap - EXIT
    info genesis "protocol parameters cached: $outdir"
  fi

  echo "$outdir"
}

