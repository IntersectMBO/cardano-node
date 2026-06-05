# shellcheck shell=bash
# shellcheck disable=SC2154  # global_basedir is set externally by the sourcing script (wb)
#
# Dataset-cached genesis backend.
# Used when WB_GENESIS_RIPPER=1.
#
# Implements the backend interface:
#   profile-cache-key-input-ripper, profile-cache-key-ripper,
#   genesis-create-ripper,
#   derive-from-cache-ripper
#
# Three-level cache:
#
# - Level 1: dataset cache ($cache_dir/genesis/dataset/)
#     Keyed by dataset params (pools, delegators, supply, etc.) + hash.
#     Each entry is unique and independent of any profile: any profile
#     needing this exact dataset combination reuses the entry as-is.
#     Can be expensive to create, which is the main reason the cache exists.
#     Stores:
#     - byron.dataset.json   {bootStakeholders, heavyDelegation, nonAvvmBalances}
#     - shelley.dataset.json {genDelegs, initialFunds, staking, maxLovelaceSupply}
#                            (with our profiles it can be 500MB+)
#     - conway.dataset.json  {initialDReps, delegs}
#                            (with our profiles it can be 200MB+)
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
#     genesis.conway + genesis.dijkstra from the profile.
#     Each entry is unique and independent of any profile: any profile with
#     this exact protocol params combination reuses the entry as-is.
#     Cheap to create, the cache exists mostly to make protocol-param
#     sharing across profiles visible.
#     Stores the spec-derived parts WITHOUT dataset fields and WITHOUT
#     startTime / systemStart (which are per-run):
#     - byron.protocol.json    (spec minus dataset fields and startTime)
#     - shelley.protocol.json  (spec minus dataset fields and systemStart)
#     - alonzo.protocol.json   (complete spec)
#     - conway.protocol.json   (spec minus dataset fields)
#     - dijkstra.protocol.json (complete spec)
#
# - Level 3: genesis cache ($cache_dir/genesis/)
#     Keyed by: profile name + protocol hash + dataset hash.
#     Placeholder for easy finding of the dataset and protocol entries used.
#
# - Final level: the genesis dir in the run directory ($run_dir/genesis).
#     Assembled from level 1 + level 2 + startTime / systemStart using cat/sed/printf.
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

# The cache-key input combines protocol params hash and dataset hash.
# (The profile name is prepended by profile-cache-key-ripper below.)
profile-cache-key-input-ripper() {
  local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
  local proto_hash dataset_hash
  proto_hash=$(protocol-cache-key-hash  "$profile_json")
  dataset_hash=$(dataset-cache-key-hash "$profile_json")
  echo "${proto_hash}-${dataset_hash}"
}

profile-cache-key-ripper() {
  local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
  local name
  name=$(jq -r '.name' "$profile_json")
  echo "${name}-$(profile-cache-key-input-ripper "$profile_json")"
}

# Ensure sub-caches exist.
# Creates symlinks to the dataset cache entry and protocol cache entry and
# pre-assembles the genesis JSON files as far as possible: those that are small
# and don't change per run (no dataset fields and no time fields).
# What is missing here is left for derive-from-cache-ripper.
genesis-create-ripper() {
  local profile_json=$1
  local outdir=$2

  mkdir -p "$outdir"

  # Ensure the dataset cache and the protocol cache exist and add symlinks.
  if ! test -d "$outdir/dataset"
  then
    local dataset_entry
    dataset_entry=$(dataset-cache-ensure   "$profile_json")
    # If the link is broken (-d above is false) it is replaced here (-f below).
    ln -sf "$dataset_entry" "$outdir/dataset"
  fi
  if ! test -d "$outdir/protocol"
  then
    local protocol_entry
    protocol_entry=$(protocol-cache-ensure "$profile_json")
    # If the link is broken (-d above is false) it is replaced here (-f below).
    ln -sf "$protocol_entry" "$outdir/protocol"
  fi

  # - Byron: changes with every run (startTime field). Ignored here.
  # - Shelley: changes with every run (systemStart field). Ignored here.
  # - Alonzo: protocol parameters only, no dataset fields. Can simply link.
  ln -s "protocol/alonzo.protocol.json"   "$outdir/genesis.alonzo.json"
  # - Conway: protocol parameters + dataset. Potentially big file. Ignored here.
  # - Dijkstra: protocol parameters only, no dataset fields. Can simply link.
  ln -s "protocol/dijkstra.protocol.json" "$outdir/genesis.dijkstra.json"

  info genesis "genesis cache entry created in $outdir"
}

# Populate the run directory from the genesis cache entry.
# Everything is symlinked except genesis.byron.json, genesis.shelley.json and
# genesis.conway.json, which are assembled fresh per run:
#   - Byron   because it contains startTime.
#   - Shelley because of the dataset payload and contains systemStart.
#   - Conway  because of the dataset payload.
# Keep it simple: cat/sed/printf.
derive-from-cache-ripper() {
  local usage="USAGE: wb genesis derive-from-cache PROFILE-OUT TIMING-JSON-EXPR CACHE-ENTRY-DIR OUTDIR"
  local profile_json=${1:?$usage}
  local timing=${2:?$usage}
  local cache_entry=${3:?$usage}
  local outdir=${4:?$usage} # output directory (run dir's genesis/, e.g. run/current/genesis).

  # Preset profiles are handled by the genesis.sh top-level `derive-from-cache`
  # dispatcher and never reach this backend.
  mkdir -p "$outdir"

  progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

  ln -s "$profile_json"                "$outdir"/profile.json
  ln -s "$cache_entry"                 "$outdir"/cache-entry
  ln -s "$cache_entry"/cache.key       "$outdir"
  ln -s "$cache_entry"/cache.key.input "$outdir"
  ln -s "$cache_entry"/layout.version  "$outdir"

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
  local system_start_epoch system_start
  system_start_epoch=$(jq -r '.start' <<<"$timing")
  system_start=$(jq -r '.systemStart' <<<"$timing")
  # Byron (always small).
  {
    sed 's/}$//' "$cache_entry/protocol/byron.protocol.json"
    printf ',"startTime":%s' "$system_start_epoch"
    printf ','
    tail -c +2 "$cache_entry/dataset/byron.dataset.json"
  } > "$outdir/genesis.byron.json"
  # Shelley (can be 500MB+).
  {
    sed 's/}$//' "$cache_entry/protocol/shelley.protocol.json"
    printf ',"systemStart":"%s"' "$system_start"
    printf ','
    tail -c +2 "$cache_entry/dataset/shelley.dataset.json"
  } > "$outdir/genesis.shelley.json"

  # Conway (can be 200MB+).
  # Only tied to a dataset + protocol. No timing.
  # When the profile leaves .genesis.conway null, protocol/conway.protocol.json
  # is the zero stub but we still need to merge the dataset side (initialDReps
  # and delegs) to produce a parseable file for the node config.
  {
    sed 's/}$//' "$cache_entry/protocol/conway.protocol.json"
    printf ','
    tail -c +2 "$cache_entry/dataset/conway.dataset.json"
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
  # The hash of the full resulting dataset.
  local key_hash
  key_hash=$(dataset-cache-key-hash "$profile_json")
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

# The hash of a unique dataset combination (48 bits).
dataset-cache-key-hash() {
  local profile_json=$1
    # A hash of ALL dataset parameters.
    jq -c                                                  \
      '{ "committee-keys":   0
       , "genesis-keys":     .composition.n_bft_hosts
       , "pools":            .composition.n_pools
       , "stake-delegators": .derived.delegators_effective
       , "drep-keys":        .genesis.dreps
       , "stuffed-utxo":     .derived.utxo_stuffed
       , "utxo-keys":        .genesis.utxo_keys
       , "total-supply":     .derived.supply_total
       , "delegated-supply": .derived.supply_delegated
       , "testnet-magic":    .genesis.network_magic
       }'                                                  \
      "$profile_json"                                      \
  | sha1sum                                                \
  | cut -c-12
}

# Ensure a dataset cache entry exists. If not, create it.
# Returns the cache entry path on stdout.
dataset-cache-ensure() {
  local profile_json=$1
  local cache_dir=${2:-$(envjqr 'cacheDir')}
  local zero_dir="$global_basedir/genesis/zero/genesis"

  local cache_entry outdir
  cache_entry=$(dataset-cache-key "$profile_json")
  outdir="$cache_dir"/genesis/dataset/"$cache_entry"

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
    local era bfts pools delegators dreps stuffed utxo_keys supply_t supply_d magic
    era=$(jq        --raw-output '.era'                          "$profile_json")
    bfts=$(jq       --raw-output '.composition.n_bft_hosts'      "$profile_json")
    pools=$(jq      --raw-output '.composition.n_pools'          "$profile_json")
    delegators=$(jq --raw-output '.derived.delegators_effective' "$profile_json")
    dreps=$(jq      --raw-output '.genesis.dreps'                "$profile_json")
    stuffed=$(jq    --raw-output '.derived.utxo_stuffed'         "$profile_json")
    utxo_keys=$(jq  --raw-output '.genesis.utxo_keys'            "$profile_json")
    supply_t=$(jq   --raw-output '.derived.supply_total'         "$profile_json")
    supply_d=$(jq   --raw-output '.derived.supply_delegated'     "$profile_json")
    magic=$(jq      --raw-output '.genesis.network_magic'        "$profile_json")
    # Run `create-testnet-data` with the "zero" preset specs from
    # profile/presets/zero/genesis/ (see its README for details).
    #
    # These are minimal/neutral spec files: they satisfy cardano-cli's
    # required --spec-{shelley,alonzo,conway} arguments without carrying
    # meaningful protocol parameters. We only want create-testnet-data to
    # materialize the DATASET fields (driven by --pools, --stake-delegators,
    # --total-supply, etc.); the protocol parameters live in a separate cache
    # (level 2), so we don't want them entangled here.
    #
    # Using zeroed specs is what makes the dataset cache independent of the
    # protocol: the same dataset params always produce byte-identical dataset
    # fields regardless of which era / protocol-params the profile uses.
    progress genesis "creating dataset cache: $outdir ($tmpdir)"
    cardano-cli "$era" genesis create-testnet-data \
      --out-dir "$tmpdir"                          \
      --spec-shelley "$zero_dir/shelley.json"      \
      --spec-alonzo  "$zero_dir/alonzo.json"       \
      --spec-conway  "$zero_dir/conway.json"       \
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
    > "$tmpdir/byron.dataset.json"
    # - Shelley:
      jq -c                                                     \
        '{genDelegs, initialFunds, staking, maxLovelaceSupply}' \
        "$tmpdir/shelley-genesis.json"                          \
    > "$tmpdir/shelley.dataset.json"
    # - Conway: Default to "{}" if cardano-cli omits initialDReps or delegs:
    #   cardano-node's parser accepts {} but rejects null.
      jq -c                                                     \
        '{ initialDReps: (.initialDReps // {})
         , delegs:       (.delegs       // {})
         }'                                                     \
        "$tmpdir/conway-genesis.json"                           \
    > "$tmpdir/conway.dataset.json"
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
  key_hash=$(protocol-cache-key-hash "$profile_json")
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

# The hash of a unique protocol parameters combination (48 bits).
protocol-cache-key-hash() {
  local profile_json=$1
  # A hash of ALL protocol parameters.
    jq -c -S                                    \
      '{ byron:    ( .genesis.byron    // {} ),
         shelley:  ( .genesis.shelley  // {} ),
         alonzo:   ( .genesis.alonzo   // {} ),
         conway:   ( .genesis.conway   // {} ),
         dijkstra: ( .genesis.dijkstra // {} )
       }'                                       \
      "$profile_json"                           \
  | sha1sum                                     \
  | cut -c-12
}

# Ensure a protocol cache entry exists. If not, create it.
# Returns the cache entry path on stdout.
protocol-cache-ensure() {
  local profile_json=$1
  local cache_dir=${2:-$(envjqr 'cacheDir')}

  local cache_entry outdir
  cache_entry=$(protocol-cache-key "$profile_json")
  outdir="$cache_dir"/genesis/protocol/"$cache_entry"

  if test -d "${outdir}"
  then
    info genesis "protocol parameters cache hit: ${outdir}"
  else
    info genesis "protocol parameters cache miss: ${outdir}"
    mkdir -p "${outdir}"
    # Generate specs from profile and strip dataset fields and time fields.
    # All specs are small (<10KB), using `jq` is fine here but MUST be compact
    # JSON (-c) because the merge step uses sed 's/}$//' which only works on
    # single-line JSON.
    # - Byron: remove dataset fields and timing.
    if test "$(jq -r '.genesis.byron // "null"' "${profile_json}")" != "null"
    then
        jq -c                                   \
          ' .genesis.byron
          | del( .bootStakeholders
               , .heavyDelegation
               , .nonAvvmBalances
               , .startTime
               )'                               \
         "${profile_json}"                      \
      > "${outdir}/byron.protocol.json"
    else
      fatal "empty .genesis.byron in profile"
    fi
    # - Shelley: remove dataset fields and timing.
    if test "$(jq -r '.genesis.shelley // "null"' "${profile_json}")" != "null"
    then
        jq -c                                   \
          ' .genesis.shelley
          | del( .genDelegs
               , .initialFunds
               , .staking
               , .maxLovelaceSupply
               , .systemStart)'                 \
          "${profile_json}"                     \
      > "${outdir}/shelley.protocol.json"
    else
      fatal "empty .genesis.shelley in profile"
    fi
    # - Alonzo: nothing to do, only compact it.
    if test "$(jq -r '.genesis.alonzo // "null"' "${profile_json}")" != "null"
    then
        jq -c                                   \
          '.genesis.alonzo'                     \
          "${profile_json}"                     \
      > "${outdir}/alonzo.protocol.json"
    else
      fatal "empty .genesis.alonzo in profile"
    fi
    # - Conway: always emit a file because cardano-node's config parser requires
    #   ConwayGenesisFile unconditionally. When the profile has a null or empty
    #   ".genesis.conway" (e.g. pre-Chang profiles with "pparamsEpoch" < 507) we
    #   emit a "zero" stub that passes the validation.
    if test "$(jq -r '.genesis.conway // "null"' "${profile_json}")" != "null"
    then
        jq -c                                   \
          '.genesis.conway
          | del( .initialDReps
               , .delegs)'                      \
          "${profile_json}"                     \
      > "${outdir}/conway.protocol.json"
    else
        # Stub that is parseable but it should never be activated.
        # Activation check in service/nodes.nix (TestConwayHardForkAtEpoch).
        genesis conway-stub-spec > "${outdir}/conway.protocol.json"
    fi
    # - Dijkstra: always emit a file because cardano-node's config parser requires
    #   DijkstraGenesisFile unconditionally. When the profile has a null or empty
    #   ".genesis.dijkstra" we emit a "zero" stub that passes the validation.
    if test "$(jq -r '.genesis.dijkstra // "null"' "${profile_json}")" != "null"
    then
        jq -c                                    \
	  '.genesis.dijkstra'                    \
          "${profile_json}"                      \
      > "${outdir}/dijkstra.protocol.json"
    else
        # Stub that is parseable but it should never be activated.
        # Activation check in service/nodes.nix (TestDijkstraHardForkAtEpoch).
        genesis dijkstra-stub-spec > "${outdir}/dijkstra.protocol.json"
    fi
  fi

  echo "${outdir}"
}

