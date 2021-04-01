{ lib
, runCommand
, writeText
##
, cacheDir
, stateDir
, basePort
##
, profile
, profileJSONFile
## As derived from profile:
, topologyNixopsFile
}:

with profile;

let
  decideSystemStart =
    ''
    system_start_epoch=$(date +%s --date="${genesis.genesis_future_offset}")
    system_start_human=$(date --utc +"%Y-%m-%dT%H:%M:%SZ" --date=@$system_start_epoch)
    '';

  byronGenesis =
    ''
    cli_args=(
        --genesis-output-dir         "${stateDir}/byron"
        --protocol-parameters-file   "${stateDir}/byron-protocol-params.json"
        --start-time                 $system_start_epoch
        --k                          2160
        --protocol-magic             42
        --n-poor-addresses           10
        --n-delegate-addresses       10
        --total-balance              300000
        --delegate-share             0.9
        --avvm-entry-count           0
        --avvm-entry-balance         0
        )
    jq '
      { heavyDelThd:       "300000"
      , maxBlockSize:      "641000"
      , maxHeaderSize:     "200000"
      , maxProposalSize:   "700"
      , maxTxSize:         "4096"
      , mpcThd:            "200000"
      , scriptVersion:     0
      , slotDuration:      "20000"
      , softforkRule:
        { initThd:         "900000"
        , minThd:          "600000"
        , thdDecrement:    "100000"
        }
      , txFeePolicy:
        { multiplier:      "439460"
        , summand:         "155381"
        }
      , unlockStakeEpoch:  "184467"
      , updateImplicit:    "10000"
      , updateProposalThd: "100000"
      , updateVoteThd:     "100000"
      }
    ' --null-input > ${stateDir}/byron-protocol-params.json
    cli byron genesis genesis ''${cli_args[@]}
    '';

  genesisCacheDir = "${cacheDir}/cardano-genesis";

  shelleyGenesisCommon =
    path:
    ''
    msg() {
        echo "$*" >&2
    }

    fail() {
	      msg "$*"
	      exit 1
    }
    jqtest() {
        jq --exit-status "$@" > /dev/null
    }
    __KEY_ROOT=
    key_depl() {
        local type=$1 kind=$2 id=$3
        case "$kind" in
                bulk )     suffix='.creds';;
                cert )     suffix='.cert';;
                count )    suffix='.counter';;
                none )     suffix=;;
                sig )      suffix='.skey';;
                ver )      suffix='.vkey';;
                * )        fail "key_depl: unknown key kind: '$kind'";; esac
        case "$type" in
                bulk )     stem=node-keys/bulk''${id};;
                cold )     stem=node-keys/cold/operator''${id};;
                opcert )   stem=node-keys/node''${id}.opcert;;
                KES )      stem=node-keys/node-kes''${id};;
                VRF )      stem=node-keys/node-vrf''${id};;
                * )        fail "key_depl: unknown key type: '$type'";; esac
        echo "$__KEY_ROOT"/$stem$suffix
    }
    key_genesis() {
        local type=$1 kind=$2 id=$3
        case "$kind" in
                bulk )     suffix='.creds';;
                cert )     suffix='.cert';;
                count )    suffix='.counter';;
                none )     suffix=;;
                sig )      suffix='.skey';;
                ver )      suffix='.vkey';;
                * )        fail "key_genesis: unknown key kind: '$kind'";; esac
        case "$type" in
                bulk )     stem=pools/bulk''${id};;
                cold )     stem=pools/cold''${id};;
                opcert )   stem=pools/opcert''${id};;
                KES )      stem=pools/kes''${id};;
                VRF )      stem=pools/vrf''${id};;
                deleg )    stem=delegate-keys/delegate''${id};;
                delegCert )stem=delegate-keys/opcert''${id};;
                delegKES ) stem=delegate-keys/delegate''${id}.kes;;
                delegVRF ) stem=delegate-keys/delegate''${id}.vrf;;
                * )        fail "key_genesis: unknown key type: '$type'";; esac
        echo "$__KEY_ROOT"/$stem$suffix
    }
    genesis_remap_key_names() {
        local ids_pool_map=$1
        local ids

        set -e

        __KEY_ROOT=${path}

        ids=($(jq 'keys
                  | join(" ")
                  ' -cr <<<$ids_pool_map))
        local bid=1 pid=1 did=1 ## (B)FT, (P)ool, (D)ense pool
        for id in ''${ids[*]}
        do
            mkdir -p "${path}"/node-keys/cold

            #### cold keys (do not copy to production system)
            if   jqtest ".genesis.dense_pool_density > 1" ${profileJSONFile} &&
                 jqtest ".[\"$id\"]  > 1" <<<$ids_pool_map
            then ## Dense/bulk pool
               echo "genesis:  bulk pool $did -> node-$id"
               cp -f $(key_genesis bulk      bulk $did) $(key_depl bulk   bulk $id)
               did=$((did + 1))
            elif jqtest ".[\"$id\"] != 0" <<<$ids_pool_map
            then ## Singular pool
               echo "genesis:  pool $pid -> node-$id"
               cp -f $(key_genesis cold       sig $pid) $(key_depl cold    sig $id)
               cp -f $(key_genesis cold       ver $pid) $(key_depl cold    ver $id)
               cp -f $(key_genesis opcert    cert $pid) $(key_depl opcert none $id)
               cp -f $(key_genesis opcert   count $pid) $(key_depl cold  count $id)
               cp -f $(key_genesis KES        sig $pid) $(key_depl KES     sig $id)
               cp -f $(key_genesis KES        ver $pid) $(key_depl KES     ver $id)
               cp -f $(key_genesis VRF        sig $pid) $(key_depl VRF     sig $id)
               cp -f $(key_genesis VRF        ver $pid) $(key_depl VRF     ver $id)
               pid=$((pid + 1))
            else ## BFT node
               echo "genesis:  BFT $bid -> node-$id"
               cp -f $(key_genesis deleg      sig $bid) $(key_depl cold    sig $id)
               cp -f $(key_genesis deleg      ver $bid) $(key_depl cold    ver $id)
               cp -f $(key_genesis delegCert cert $bid) $(key_depl opcert none $id)
               cp -f $(key_genesis deleg    count $bid) $(key_depl cold  count $id)
               cp -f $(key_genesis delegKES   sig $bid) $(key_depl KES     sig $id)
               cp -f $(key_genesis delegKES   ver $bid) $(key_depl KES     ver $id)
               cp -f $(key_genesis delegVRF   sig $bid) $(key_depl VRF     sig $id)
               cp -f $(key_genesis delegVRF   ver $bid) $(key_depl VRF     ver $id)
               bid=$((bid + 1))
            fi
        done
    }

    topology_id_pool_density_map() {
            local topology_file=''${1:-}

            nix-instantiate --strict --eval \
              -E '__toJSON (__listToAttrs
                            (map (x: { name = toString x.nodeId;
                                      value = if (x.pools or 0) == null then 0 else x.pools or 0; })
                                 (__fromJSON (__readFile '"''${topology_file}"')).coreNodes))' |
              sed 's_\\__g; s_^"__; s_"$__'
    }

    ids_pool_map=$(topology_id_pool_density_map "${topologyNixopsFile}")
    echo "genesis: id-pool map:  $ids_pool_map"

    cli genesis create --genesis-dir ${path}/ \
        ${toString cli_args.createSpec}

    jq -r --argjson genesisVerb '${__toJSON genesis.verbatim}' '
        . * $genesisVerb
        '  ${path}/genesis.spec.json |
    sponge ${path}/genesis.spec.json
    '';

  shelleyGenesis =
    ''
    ## Determine the genesis cache entry:

    profile_json='${__toJSON profile}'

    genesis_cache_params=$(jq '.genesis * .composition |
                              del(.active_slots_coeff) |
                              del(.byron) |
                              del(.epoch_length) |
                              del(.era) |
                              del(.genesis_future_offset) |
                              del(.locations) |
                              del(.max_block_size) |
                              del(.max_tx_size) |
                              del(.parameter_k) |
                              del(.slot_duration) |
                              del(.with_observer)
                             ' --sort-keys <<<$profile_json)

    genesis_params_hash=$(echo "$genesis_cache_params" | sha1sum | cut -c-7)
    genesis_cache_id=$(jq <<<$profile_json \
       '"k\(.composition.n_pools)-d\(.composition.dense_pool_density)-\(.genesis.delegators / 1000)kD-\(.genesis.utxo / 1000)kU-\($params_hash)"
       ' --arg params_hash "$genesis_params_hash" --raw-output)
    eval genesis_cache_path="${genesisCacheDir}/$genesis_cache_id"

    ## Handle genesis cache hit/miss:

    if test -f "$genesis_cache_path"/cache.params.id
    then genesis_cache_hit=t; genesis_cache_hit_desc='hit'
    else genesis_cache_hit=;  genesis_cache_hit_desc='miss'; fi
    echo "Genesis cache $genesis_cache_hit_desc:  $genesis_cache_id"

    regenesis_causes=()
    if   test -z "$genesis_cache_hit"
    then regenesis_causes+=(cache-miss)
    elif test -v __REWRITE_GENESIS_CACHE
    then regenesis_causes+=(__REWRITE_GENESIS_CACHE-env-var-defined)
    fi

    if test -n "''${regenesis_causes[*]}"
    then echo "..generating genesis due to ''${regenesis_causes[*]}:  $genesis_cache_id @$genesis_cache_path"
         rm -rf "$genesis_cache_path"/{*-keys,pools,nodes,*.json,*.params}
         mkdir -p "$genesis_cache_path"

         ${shelleyGenesisCommon   "$genesis_cache_path"}

         ${if genesis.single_shot
           then
             ''
             ${shelleyGenesisSingleshot "$genesis_cache_path"}
             genesis_remap_key_names "$ids_pool_map"
             ''
           else
             ''
             cat <<EOF
             Incremental (non single-shot) genesis is no longer suppored.
             EOF
             exit 1
             ''}

         cat <<<$genesis_cache_params > "$genesis_cache_path"/cache.params
         cat <<<$genesis_cache_id     > "$genesis_cache_path"/cache.params.id
    fi

    ## Switch to the genesis cache entry:

    rm -f                                     ${stateDir}/shelley
    ln -s "$(realpath "$genesis_cache_path")" ${stateDir}/shelley

    ${updateGenesisCacheEntry "${stateDir}/shelley/genesis.json" profileJSONFile}
    '';

  updateGenesisCacheEntry =
    genesisFile: profileFile:
    ''
    ## Update start time:

    start_time=$(date --iso-8601=s --date="now + ${genesis.genesis_future_offset}" --utc | cut -c-19)

    ## TODO: duplication
    jq ' $prof[0] as $p
       | . *
       { systemStart:                $start_time
       , activeSlotsCoeff:           $p.genesis.active_slots_coeff
       , epochLength:                $p.genesis.epoch_length
       , maxTxSize:                  $p.genesis.max_tx_size
       , securityParam:              $p.genesis.parameter_k
       , slotLength:                 $p.genesis.slot_duration
       , protocolParams:
         { "decentralisationParam":  $p.genesis.decentralisation_param
         , "maxBlockBodySize":       $p.genesis.max_block_size
         }
       }' --slurpfile prof          ${profileFile}  \
          --arg       start_time "''${start_time}Z" \
           ${genesisFile} |
    sponge ${genesisFile}
    '';

  shelleyGenesisSingleshot =
    dir:
    ''
    params=(--genesis-dir      "${dir}"
            ${toString cli_args.createFinalBulk}
           )
    ## update genesis from template
    cli genesis create-staked "''${params[@]}"
    '';

  hardcodedDefaultUtxoCredentials =
    dir:
    ''
    cp ${./genesis-utxo.vkey} ${dir}/genesis-utxo.vkey
    cp ${./genesis-utxo.skey} ${dir}/genesis-utxo.skey
    '';
in
''
    mkdir -p ${genesisCacheDir}

    ${decideSystemStart}

    ${byronGenesis}

    ${shelleyGenesis}

    echo "Generated genesis for ${name} (cache id $genesis_cache_id)"
''
