# shellcheck shell=bash
#
# Legacy (jq-based) genesis backend.
# Used when WB_MODULAR_GENESIS=0 and WB_DATASET_GENESIS=0 (the default).
#
# Implements the backend interface:
#   profile-cache-key-input-jq, profile-cache-key-jq,
#   spec-jq, pool-relays-jq,
#   genesis-create-jq, derive-from-cache-jq

profile-cache-key-input-jq() {
    set -euo pipefail
    local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}

    local args=(
        --slurpfile profile "$profile_json"
        --arg profile_json "$profile_json"
        --sort-keys
        --null-input
        -L "$global_basedir/profile"
        -L "$global_basedir/genesis"
    )
    jq 'include "genesis"; profile_genesis_cache_key($profile[0]; $profile_json)' "${args[@]}"
}

profile-cache-key-jq() {
    set -euo pipefail
    local usage="USAGE: wb genesis profile-cache-key PROFILE-JSON"
    local profile_json=${1:?$usage}
    local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}

    local args=(
        --arg params_hash "$(genesis profile-cache-key-input "$profile_json" | sha1sum | cut -c-7)"
        --slurpfile profile "$profile_json"
        --raw-output
        -L "$global_basedir/profile"
        -L "$global_basedir/genesis"
    )
    jq 'include "genesis"; profile_genesis_cache_entry_name($profile[0]; $params_hash)' "${args[@]}" "$profile_json"
}

spec-jq() {
    set -euo pipefail
    local era=${1:?missing era}
    local profile_json=${2:?missing profile_json}
    # not needed in the jq version
    # local node_specs=${3:?missing node_specs}

    case "$1" in
    byron)
        # Byron base matches what master's genesis-byron-jq passed to
        # `cardano-cli byron genesis genesis` as protocol parameters,
        # plus the --k and --protocol-magic from the profile.
        jq --null-input --slurpfile prof "$profile_json" '
          $prof[0] as $p | {
            avvmDistr: {},
            blockVersionData: {
              heavyDelThd:     "300000",
              maxBlockSize:    "641000",
              maxHeaderSize:   "200000",
              maxProposalSize: "700",
              maxTxSize:       "4096",
              mpcThd:          "200000",
              scriptVersion:   0,
              slotDuration:    "20000",
              softforkRule: {
                initThd:      "900000",
                minThd:       "600000",
                thdDecrement: "100000"
              },
              txFeePolicy: {
                multiplier: "439460",
                summand:    "0"
              },
              unlockStakeEpoch: "184467",
              updateImplicit:    "10000",
              updateProposalThd: "100000",
              updateVoteThd:     "100000"
            },
            protocolConsts: {
              k:             $p.genesis.parameter_k,
              protocolMagic: $p.genesis.network_magic
            },
            startTime: 0
          }
        '
        ;;
    shelley)
        # We pass the network magic as an argument to cardano-cli, but, despite being required,
        # it is only used to amend a default template while we are bringing our own. So we put it into
        # the template too.
        jq '$prof[0] as $p
            | . * ($p.genesis.shelley // {})
            | .networkMagic |= $p.genesis.network_magic
            ' --slurpfile prof "$profile_json" \
            "$global_basedir"/profile/presets/mainnet/genesis/genesis-shelley.json
        ;;
    alonzo)
        jq '$prof[0] as $p | . * ($p.genesis.alonzo // {})' \
            --slurpfile prof "$profile_json" \
            "$global_basedir"/profile/presets/mainnet/genesis/genesis.alonzo.json
        ;;
    conway)
        jq '$prof[0] as $p | . * ($p.genesis.conway // {})' \
            --slurpfile prof "$profile_json" \
            "$global_basedir"/profile/presets/mainnet/genesis/genesis.conway.json
        ;;
    dijkstra)
        jq '$prof[0] as $p | . * ($p.genesis.dijkstra // {})' \
            --slurpfile prof "$profile_json" \
            "$global_basedir"/profile/presets/zero/genesis/dijkstra-genesis.json
        ;;
    *)
        echo unknown era
        exit 1
        ;;
    esac
}

pool-relays-jq() {
    set -euo pipefail
    local profile_json=${1:?missing profile_json}
    # not needed in the jq version
    # local node_specs=${2:?missing node_specs}

    jq ' to_entries
       | map( select(.value.kind == "pool") )
       | map
         ({ key:   (.value.i | tostring)
          , value:
            [{ "single host name":
               { dnsName: .key
               , port:    .value.port
               }
             }
            ]
          })
       | from_entries
       ' "$node_specs"
}

# Entry point for genesis creation.
genesis-create-jq() {
    if [[ $WB_CREATE_TESTNET_DATA -eq 1 ]]; then
        genesis-create-testnet-data "$@"
    else
        genesis-create-staked "$@"
    fi
}

genesis-create-staked() {
    local profile_json=$1
    local dir=$2

    rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
    mkdir -p "$dir"

    genesis spec shelley "$profile_json" >"$dir/genesis.spec.json"
    genesis spec alonzo  "$profile_json" >"$dir/genesis.alonzo.spec.json"
    genesis spec conway  "$profile_json" >"$dir/genesis.conway.spec.json"
    genesis pool-relays  "$profile_json" >"$dir/pool-relays.json"

    read -r -a args <<<"$(jq --raw-output '.cli_args.createStakedArgs | join(" ")' "$profile_json")"
    create_staked_args=(
        --genesis-dir "$dir"
        --relay-specification-file "$dir/pool-relays.json"
        "${args[@]}"
    )
    progress "genesis" "$(colorise cardano-cli genesis create-staked "${create_staked_args[@]}")"
    cardano-cli genesis create-staked "${create_staked_args[@]}"

    # Normalize create-staked output to match create-testnet-data layout,
    # so the rest of the workbench uses a single format.
    normalize-create-staked-output "$dir"
}

# Rearrange create-staked output to match create-testnet-data layout.
#
# create-staked produces:
#   pools/cold1.skey, pools/kes1.skey, ...  (flat, 1-indexed)
#   delegate-keys/delegate1.skey, ...       (BFT keys)
#   stake-delegator-keys/                   (different name)
#   genesis.json                            (different name)
#
# create-testnet-data produces:
#   pools-keys/pool1/cold.skey, ...         (subdirs, 1-indexed)
#   stake-delegators/delegator1/            (different name)
#   shelley-genesis.json                    (different name)
#   drep-keys/ byron-gen-command/           (not in create-staked)
#
normalize-create-staked-output() {
    local dir=$1

    # Genesis JSON naming
    mv "$dir"/genesis.json      "$dir"/genesis.shelley.json
    mv "$dir"/genesis.spec.json "$dir"/genesis.shelley.spec.json

    # Pool keys: pools/cold1.skey → pools-keys/pool1/cold.skey
    if [[ -d "$dir"/pools ]]; then
      mkdir -p "$dir"/pools-keys
      # Find the highest pool index
      local max_pool=0
      for f in "$dir"/pools/cold*.skey; do
        [[ -f "$f" ]] || continue
        local n=${f##*cold}; n=${n%.skey}
        (( n > max_pool )) && max_pool=$n
      done
      for n in $(seq 1 "$max_pool"); do
        local pooldir="$dir/pools-keys/pool${n}"
        mkdir -p "$pooldir"
        # Move each key type, matching create-testnet-data naming
        [[ -f "$dir/pools/cold${n}.skey"    ]] && mv "$dir/pools/cold${n}.skey"    "$pooldir/cold.skey"
        [[ -f "$dir/pools/cold${n}.vkey"    ]] && mv "$dir/pools/cold${n}.vkey"    "$pooldir/cold.vkey"
        [[ -f "$dir/pools/kes${n}.skey"     ]] && mv "$dir/pools/kes${n}.skey"     "$pooldir/kes.skey"
        [[ -f "$dir/pools/kes${n}.vkey"     ]] && mv "$dir/pools/kes${n}.vkey"     "$pooldir/kes.vkey"
        [[ -f "$dir/pools/vrf${n}.skey"     ]] && mv "$dir/pools/vrf${n}.skey"     "$pooldir/vrf.skey"
        [[ -f "$dir/pools/vrf${n}.vkey"     ]] && mv "$dir/pools/vrf${n}.vkey"     "$pooldir/vrf.vkey"
        [[ -f "$dir/pools/opcert${n}.cert"  ]] && mv "$dir/pools/opcert${n}.cert"  "$pooldir/opcert.cert"
        [[ -f "$dir/pools/opcert${n}.counter" ]] && mv "$dir/pools/opcert${n}.counter" "$pooldir/opcert.counter"
      done
      rm -rf "$dir"/pools
    fi

    # Stake delegator keys: stake-delegator-keys/ → stake-delegators/
    if [[ -d "$dir"/stake-delegator-keys ]]; then
      mv "$dir"/stake-delegator-keys "$dir"/stake-delegators
    fi

    # create-staked does not produce a byron genesis; generate from spec.
    genesis spec byron "$profile_json" > "$dir"/genesis.byron.json

    # Ensure directories that create-testnet-data always creates
    mkdir -p "$dir"/{byron-gen-command,drep-keys,pools-keys,stake-delegators,utxo-keys}

    info genesis "normalized create-staked output to create-testnet-data layout"
}

genesis-create-testnet-data() {
    local profile_json=$1
    local dir=$2

    mkdir -p "$dir"

    genesis spec shelley "$profile_json" >"$dir/shelley-genesis.spec.json"
    genesis spec alonzo  "$profile_json" >"$dir/alonzo-genesis.spec.json"
    genesis spec conway  "$profile_json" >"$dir/conway-genesis.spec.json"
    genesis pool-relays  "$profile_json" >"$dir/pool-relays.json"

    local era
    era=$(jq --raw-output '.era' "$profile_json")

    # TODO if profile_json.composition.dense_pool_density != 1 -> create-testnet-data does not support dense pools
    read -r -a args <<<"$(jq --raw-output '.cli_args.createTestnetDataArgs | join(" ")' "$profile_json")"
    create_testnet_data_args=(
        --spec-shelley "$dir/shelley-genesis.spec.json"
        --spec-alonzo  "$dir/alonzo-genesis.spec.json"
        --spec-conway  "$dir/conway-genesis.spec.json"
        --relays       "$dir/pool-relays.json"
        --out-dir      "$dir"
        "${args[@]}"
    )
    progress genesis "$(colorise cardano-cli "$era" create-testnet-data "${create_testnet_data_args[@]}")"
    cardano-cli "$era" genesis create-testnet-data "${create_testnet_data_args[@]}"

    # Normalize create-testnet-data output names to genesis.<era>.json
    mv "$dir"/byron-genesis.json    "$dir"/genesis.byron.json
    mv "$dir"/shelley-genesis.json  "$dir"/genesis.shelley.json
    mv "$dir"/alonzo-genesis.json   "$dir"/genesis.alonzo.json
    mv "$dir"/conway-genesis.json   "$dir"/genesis.conway.json
    mv "$dir"/dijkstra-genesis.json "$dir"/genesis.dijkstra.json

    local is_voting
    is_voting=$(jq --raw-output '.workloads | any( .name == "voting")' "$profile_json")
    if [[ "$is_voting" == "true" ]];
    then
        info genesis "voting workload specified - keeping one stake key per producer"
        mv "$dir/stake-delegators" "$dir/stake-delegators.bak"
        mkdir "$dir/stake-delegators"
        local pools
        pools="$(jq --raw-output '.composition.n_pools' "${profile_json}")"
        for i in $(seq 1 "$pools")
        do
          if test -d "$dir/stake-delegators.bak/delegator${i}"
          then
            local from_dir to_dir
            from_dir="$dir/stake-delegators.bak/delegator${i}"
            to_dir="$dir/stake-delegators/delegator$((i - 1))"
            mkdir "$to_dir"
            cp "$from_dir"/{payment,staking}.{skey,vkey} "$to_dir"/
          fi
        done
        rm "$dir/stake-delegators.bak" -rf
        info genesis "voting workload specified - skipping deletion of DRep keys"
    else
      info genesis "removing delegator keys."
      rm "$dir/stake-delegators" -rf
      info genesis "removing dreps keys."
      rm "$dir"/drep-keys -rf
    fi

    info genesis "create-testnet-data genesis available in $dir"
}

derive-from-cache-jq() {
    local usage="USAGE: wb genesis derive-from-cache PROFILE-OUT TIMING-JSON-EXPR CACHE-ENTRY-DIR OUTDIR"
    local profile=${1:?$usage}
    local timing=${2:?$usage}
    local cache_entry=${3:?$usage}
    local outdir=${4:?$usage} # output directory (run dir's genesis/, e.g. run/current/genesis).

    mkdir -p "$outdir"

    local preset
    preset=$(profile preset "$profile")
    if [[ -n "$preset" ]]; then
      progress "genesis" "instantiating from preset $(with_color white "$preset"):  $cache_entry"
      cp -f "$cache_entry"/genesis*.json "$outdir"
      return
    fi

    progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

    ln -s "$profile"                          "$outdir"/profile.json
    ln -s "$cache_entry"                      "$outdir"/cache-entry
    ln -s "$cache_entry"/cache.key            "$outdir"
    ln -s "$cache_entry"/cache.key.input      "$outdir"
    ln -s "$cache_entry"/layout.version       "$outdir"

    # Key directories as symlinks using create-testnet-data output names.
    for keydir in byron-gen-command drep-keys pools-keys stake-delegators utxo-keys
    do
      if [[ -d "$cache_entry/$keydir" ]]; then
        ln -s "$cache_entry/$keydir" "$outdir/$keydir"
      else
        mkdir -p "$outdir/$keydir"
      fi
    done

    ## genesis
    cp    "$cache_entry"/genesis*.json        "$outdir"
    chmod u+w                                 "$outdir"/genesis*.json

    local dir="$outdir"

    ## -- finalise: patch genesis files with per-run values ---------------------

    local profile_json=$profile
    local dir=$outdir

    progress "genesis" "finalizing retrieved cache entry in:  $outdir"

    local system_start_epoch
    system_start_epoch="$(jq '.start' -r <<<"$timing")"

    # Byron: patch startTime (same approach as shelley's systemStart below)
    jq --argjson start "$system_start_epoch" '.startTime = $start' \
      "$dir"/genesis.byron.json |
      sponge "$dir"/genesis.byron.json

    # The genesis cache entry in $outdir needs post-processing:
    # * the system start date might get an adjustment offset into the future
    # * some workbench profile content not captured by the genesis cache key will be patched in

    # For devs: this should cover all fields modified by
    # * nix/workbench/profile/pparams/delta-*.jq (workbench)
    # * bench/cardano-profile/data/genesis/overlays/*.json (cardano-profile)
    # These modifications are small deltas to base profiles, which do not, and should
    # not, result in recreation of the entire staked genesis, as that is large on-disk
    # and takes long to create.

    # Shelley: startTime, protocolVersion, maxBlockBodySize
    jq '$prof[0].genesis.shelley as $shey
         | $shey.protocolParams.protocolVersion   as $pver
         | $shey.protocolParams.maxBlockBodySize  as $bsize
         | . * { systemStart: $timing.systemStart }
         | if $pver  != null then . * { protocolParams: { protocolVersion:  $pver  } } else . end
         | if $bsize != null then . * { protocolParams: { maxBlockBodySize: $bsize } } else . end' \
      --argjson timing "$timing" \
      --slurpfile prof "$profile_json" \
      "$dir"/genesis.shelley.json |
      sponge "$dir"/genesis.shelley.json

    # Alonzo: Execution budgets
    # NB. PlutusV1 and PlutusV2 cost models are *NOT* covered here; they're encoded
    #     as key-value-map in the profile, but need to be [Integer] in genesis.
    jq '$prof[0].genesis.alonzo as $alzo
         | $alzo.maxBlockExUnits.exUnitsMem   as $bl_mem
         | $alzo.maxBlockExUnits.exUnitsSteps as $bl_steps
         | $alzo.maxTxExUnits.exUnitsMem      as $tx_mem
         | $alzo.maxTxExUnits.exUnitsSteps    as $tx_steps
         | if $bl_mem   != null then . * { maxBlockExUnits: { memory: $bl_mem}   } else . end
         | if $bl_steps != null then . * { maxBlockExUnits: { steps:  $bl_steps} } else . end
         | if $tx_mem   != null then . * { maxTxExUnits:    { memory: $tx_mem}   } else . end
         | if $tx_steps != null then . * { maxTxExUnits:    { steps:  $tx_steps} } else . end' \
      --slurpfile prof "$profile_json" \
      "$dir"/genesis.alonzo.json |
      sponge "$dir"/genesis.alonzo.json

    # Conway: plutusV3CostModel
    jq '$prof[0].genesis.conway as $coay
         | $coay.plutusV3CostModel as $pv3cost
         | if $pv3cost != null then . * { plutusV3CostModel: $pv3cost } else . end' \
      --slurpfile prof "$profile_json" \
      "$dir"/genesis.conway.json |
      sponge "$dir"/genesis.conway.json

}

