global_genesis_format_version=September-10-2021

usage_genesis() {
     usage "genesis" "Genesis" <<EOF
    prepare [--force] CACHEDIR PROFILE-JSON TOPO-DIR OUTDIR
                     Prepare a genesis cache entry profile.
                       Cache entry regeneration can be --force 'd

    profile-cache-key-input PROFILE-JSON
                     Output effective profile parameters factoring into the
                       genesis cache key

    profile-cache-key PROFILE-JSON
                     Output a genesis cache key for the specified profile

    actually-genesis PROFILE-JSON TOPO-DIR DIR
                     (DEV) Internal procedure to actually generate genesis

    finalise-cache-entry PROFILE-JSON DIR
                     (DEV) Update a genesis cache entry to the given profile
EOF
}

genesis() {
local op=${1:-$(usage_genesis)}; shift

case "$op" in
    prepare )
        local usage="USAGE:  wb genesis $op [--force] CACHEDIR PROFILE-JSON TOPO-DIR OUTDIR"

        local regenesis_causes=()
        while test $# -gt 0
        do case "$1" in
               --force ) regenesis_causes+=('has--force');;
               * ) break;; esac; shift; done

        local cachedir=${1:?$usage}
        local profile_json=${2:?$usage}
        local topo_dir=${3:?$usage}
        local outdir=${4:?$usage}

        local cache_key_input=$(genesis profile-cache-key-input "$profile_json")
        local cache_key=$(genesis profile-cache-key "$profile_json")
        local cache_path=$cachedir/$cache_key

        if genesis cache-test "$cache_path"
        then cache_hit=t; cache_hit_desc='hit'
        else cache_hit=;  cache_hit_desc='miss'; fi
        msg "genesis: preparing cache entry $cache_key:  $cache_hit_desc ($cache_path)"

        if   test -z "$cache_hit"
        then regenesis_causes+=('cache-miss')
        elif test -v __REWRITE_GENESIS_CACHE
        then regenesis_causes+=('__REWRITE_GENESIS_CACHE-env-var-defined')
        fi

        if test -n "${regenesis_causes[*]}"
        then msg "genesis: generating due to ${regenesis_causes[*]}:  $cache_key @$cache_path"
             jqtest .genesis.single_shot "$profile_json" ||
                 fail "Incremental (non single-shot) genesis is not suppored."

             if profile has-preset "$profile_json"
             then local preset=$(jq .preset "$profile_json" -r)
                  genesis genesis-from-preset "$preset" "$cache_path"
             else genesis actually-genesis "$profile_json" "$topo_dir" "$cache_path"
             fi

             cat <<<$cache_key_input > "$cache_path"/cache.key.input
             cat <<<$cache_key       > "$cache_path"/cache.key
             cat <<<$global_genesis_format_version > "$cache_path"/layout.version
        fi

        genesis finalise-cache-entry "$profile_json" "$cache_path"

        ## Install the cache entry:
        rm -f $outdir ## Must be a symlink for this to succeed
        ln -s "$(realpath "$cache_path")" "$outdir";;

    cache-test )
        local usage="USAGE:  wb genesis $op GENESIS-CACHE-DIR"
        local cache_dir=${1:?$usage}
        local version=$(cat "$cache_dir"/layout.version 2>/dev/null || echo "unknown")

        test "$version" == $global_genesis_format_version
        ret=$?
        if test $ret != 0
        then msg "genesis:  cache entry at $cache_dir is incompatible:  layout version '$version' does not match current: $global_genesis_format_version"
        fi
        return $ret;;

    profile-cache-key-input )
        local usage="USAGE:  wb genesis $op PROFILE-JSON"
        local profile_json=${1:?$usage}

        args=(--slurpfile profile $profile_json
              --sort-keys
              --null-input
             )
        ## Remove parts of profile that don't invalidate
        ## the cryptographic material in genesis.  Note the conservative approach.
        ##
        ## Note that the genesis cache entry itself must still be updated
        ## to match these parameters, hence the distinction between parameters:
        jq '
           $profile[0].genesis * $profile[0].composition

           ## Genesis-relevant -- must be updated in finalise-cache-entry:
           | del(.active_slots_coeff)
           | del(.epoch_length)
           | del(.max_block_size)
           | del(.max_tx_size)
           | del(.parameter_k)
           | del(.slot_duration)

           ## Genesis-irrelevant -- safe to ignore in finalise-cache-entry:
           | del(.byron)
           | del(.era)
           | del(.genesis_future_offset)
           | del(.locations)
           | del(.with_proxy)
           | del(.with_observer)
           ' ${args[*]};;

    profile-cache-key )
        local usage="USAGE:  wb genesis $op PROFILE-JSON"
        local profile_json=${1:?$usage}

        local args=(
            --arg params_hash $(genesis profile-cache-key-input $profile_json |
                                    sha1sum | cut -c-7)
            --slurpfile profile $profile_json
            --raw-output
        )
        jq 'if $profile[0].preset == null
            then [ "k\(.composition.n_pools)"
                 , "d\(.composition.dense_pool_density)"
                 , "\(.genesis.delegators / 1000)kD"
                 , "\(.genesis.utxo / 1000)kU"
                 , "\($params_hash)" ]
            else [ "preset"
                 , $profile[0].preset ]
            end
            | join("-")
            ' ${args[*]} $profile_json;;

    genesis-from-preset )
        local usage="USAGE:  wb genesis $op PRESET GENESIS-DIR"
        local preset=${1:?$usage}
        local dir=${2:?$usage}

        msg "genesis: profile uses preset genesis: $preset"

        rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
        mkdir -p "$dir/byron"
        local fs=(
            byron/genesis.json
            genesis.json
            alonzo-genesis.json
        )
        for f in ${fs[*]}
        do cp -f "$(profile preset-get-file $preset 'genesis file' genesis/$f)" "$dir/$f"
        done;;

    actually-genesis )
        local usage="USAGE:  wb genesis $op PROFILE-JSON TOPO-DIR DIR"
        local profile_json=${1:?$usage}
        local topo_dir=${2:?$usage}
        local dir=${3:?$usage}

        rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
        mkdir -p "$dir"

        cardano-cli genesis create --genesis-dir "$dir"/ \
            $(jq '.cli_args.createSpec | join(" ")' "$profile_json" --raw-output)

        ## Overlay the verbatim genesis part into the profile spec:
        local params=(
            --slurpfile profile "$profile_json"
            --raw-output
        )
        jq "${params[@]}" '
           . * $profile[0].genesis.verbatim
           '   "$dir"/genesis.spec.json |
        sponge "$dir"/genesis.spec.json

        params=(--genesis-dir "$dir"
                $(jq '.cli_args.createFinalBulk | join(" ")' "$profile_json" --raw-output)
               )
        cardano-cli genesis create-staked "${params[@]}"

        ## TODO: temporary step for Alonzo
        jq '
	  {
	      "lovelacePerUTxOWord": 0,
	      "executionPrices": {
		  "prSteps":
		  {
		      "numerator" :   721,
		      "denominator" : 10000000
			  },
		  "prMem":
		  {
		      "numerator" :   577,
		      "denominator" : 10000
		  }
	      },
	      "maxTxExUnits": {
		  "exUnitsMem":   10000000,
		  "exUnitsSteps": 10000000000
	      },
	      "maxBlockExUnits": {
		  "exUnitsMem":   50000000,
		  "exUnitsSteps": 40000000000
	      },
	      "maxValueSize": 5000,
	      "collateralPercentage": 150,
	      "maxCollateralInputs": 3,
	      "costModels": {
		  "PlutusV1": {
		      "sha2_256-memory-arguments": 4,
		      "equalsString-cpu-arguments-constant": 1000,
		      "cekDelayCost-exBudgetMemory": 100,
		      "lessThanEqualsByteString-cpu-arguments-intercept": 103599,
		      "divideInteger-memory-arguments-minimum": 1,
		      "appendByteString-cpu-arguments-slope": 621,
		      "blake2b-cpu-arguments-slope": 29175,
		      "iData-cpu-arguments": 150000,
		      "encodeUtf8-cpu-arguments-slope": 1000,
		      "unBData-cpu-arguments": 150000,
		      "multiplyInteger-cpu-arguments-intercept": 61516,
		      "cekConstCost-exBudgetMemory": 100,
		      "nullList-cpu-arguments": 150000,
		      "equalsString-cpu-arguments-intercept": 150000,
		      "trace-cpu-arguments": 150000,
		      "mkNilData-memory-arguments": 32,
		      "lengthOfByteString-cpu-arguments": 150000,
		      "cekBuiltinCost-exBudgetCPU": 29773,
		      "bData-cpu-arguments": 150000,
		      "subtractInteger-cpu-arguments-slope": 0,
		      "unIData-cpu-arguments": 150000,
		      "consByteString-memory-arguments-intercept": 0,
		      "divideInteger-memory-arguments-slope": 1,
		      "divideInteger-cpu-arguments-model-arguments-slope": 118,
		      "listData-cpu-arguments": 150000,
		      "headList-cpu-arguments": 150000,
		      "chooseData-memory-arguments": 32,
		      "equalsInteger-cpu-arguments-intercept": 136542,
		      "sha3_256-cpu-arguments-slope": 82363,
		      "sliceByteString-cpu-arguments-slope": 5000,
		      "unMapData-cpu-arguments": 150000,
		      "lessThanInteger-cpu-arguments-intercept": 179690,
		      "mkCons-cpu-arguments": 150000,
		      "appendString-memory-arguments-intercept": 0,
		      "modInteger-cpu-arguments-model-arguments-slope": 118,
		      "ifThenElse-cpu-arguments": 1,
		      "mkNilPairData-cpu-arguments": 150000,
		      "lessThanEqualsInteger-cpu-arguments-intercept": 145276,
		      "addInteger-memory-arguments-slope": 1,
		      "chooseList-memory-arguments": 32,
		      "constrData-memory-arguments": 32,
		      "decodeUtf8-cpu-arguments-intercept": 150000,
		      "equalsData-memory-arguments": 1,
		      "subtractInteger-memory-arguments-slope": 1,
		      "appendByteString-memory-arguments-intercept": 0,
		      "lengthOfByteString-memory-arguments": 4,
		      "headList-memory-arguments": 32,
		      "listData-memory-arguments": 32,
		      "consByteString-cpu-arguments-intercept": 150000,
		      "unIData-memory-arguments": 32,
		      "remainderInteger-memory-arguments-minimum": 1,
		      "bData-memory-arguments": 32,
		      "lessThanByteString-cpu-arguments-slope": 248,
		      "encodeUtf8-memory-arguments-intercept": 0,
		      "cekStartupCost-exBudgetCPU": 100,
		      "multiplyInteger-memory-arguments-intercept": 0,
		      "unListData-memory-arguments": 32,
		      "remainderInteger-cpu-arguments-model-arguments-slope": 118,
		      "cekVarCost-exBudgetCPU": 29773,
		      "remainderInteger-memory-arguments-slope": 1,
		      "cekForceCost-exBudgetCPU": 29773,
		      "sha2_256-cpu-arguments-slope": 29175,
		      "equalsInteger-memory-arguments": 1,
		      "indexByteString-memory-arguments": 1,
		      "addInteger-memory-arguments-intercept": 1,
		      "chooseUnit-cpu-arguments": 150000,
		      "sndPair-cpu-arguments": 150000,
		      "cekLamCost-exBudgetCPU": 29773,
		      "fstPair-cpu-arguments": 150000,
		      "quotientInteger-memory-arguments-minimum": 1,
		      "decodeUtf8-cpu-arguments-slope": 1000,
		      "lessThanInteger-memory-arguments": 1,
		      "lessThanEqualsInteger-cpu-arguments-slope": 1366,
		      "fstPair-memory-arguments": 32,
		      "modInteger-memory-arguments-intercept": 0,
		      "unConstrData-cpu-arguments": 150000,
		      "lessThanEqualsInteger-memory-arguments": 1,
		      "chooseUnit-memory-arguments": 32,
		      "sndPair-memory-arguments": 32,
		      "addInteger-cpu-arguments-intercept": 197209,
		      "decodeUtf8-memory-arguments-slope": 8,
		      "equalsData-cpu-arguments-intercept": 150000,
		      "mapData-cpu-arguments": 150000,
		      "mkPairData-cpu-arguments": 150000,
		      "quotientInteger-cpu-arguments-constant": 148000,
		      "consByteString-memory-arguments-slope": 1,
		      "cekVarCost-exBudgetMemory": 100,
		      "indexByteString-cpu-arguments": 150000,
		      "unListData-cpu-arguments": 150000,
		      "equalsInteger-cpu-arguments-slope": 1326,
		      "cekStartupCost-exBudgetMemory": 100,
		      "subtractInteger-cpu-arguments-intercept": 197209,
		      "divideInteger-cpu-arguments-model-arguments-intercept": 425507,
		      "divideInteger-memory-arguments-intercept": 0,
		      "cekForceCost-exBudgetMemory": 100,
		      "blake2b-cpu-arguments-intercept": 2477736,
		      "remainderInteger-cpu-arguments-constant": 148000,
		      "tailList-cpu-arguments": 150000,
		      "encodeUtf8-cpu-arguments-intercept": 150000,
		      "equalsString-cpu-arguments-slope": 1000,
		      "lessThanByteString-memory-arguments": 1,
		      "multiplyInteger-cpu-arguments-slope": 11218,
		      "appendByteString-cpu-arguments-intercept": 396231,
		      "lessThanEqualsByteString-cpu-arguments-slope": 248,
		      "modInteger-memory-arguments-slope": 1,
		      "addInteger-cpu-arguments-slope": 0,
		      "equalsData-cpu-arguments-slope": 10000,
		      "decodeUtf8-memory-arguments-intercept": 0,
		      "chooseList-cpu-arguments": 150000,
		      "constrData-cpu-arguments": 150000,
		      "equalsByteString-memory-arguments": 1,
		      "cekApplyCost-exBudgetCPU": 29773,
		      "quotientInteger-memory-arguments-slope": 1,
		      "verifySignature-cpu-arguments-intercept": 3345831,
		      "unMapData-memory-arguments": 32,
		      "mkCons-memory-arguments": 32,
		      "sliceByteString-memory-arguments-slope": 1,
		      "sha3_256-memory-arguments": 4,
		      "ifThenElse-memory-arguments": 1,
		      "mkNilPairData-memory-arguments": 32,
		      "equalsByteString-cpu-arguments-slope": 247,
		      "appendString-cpu-arguments-intercept": 150000,
		      "quotientInteger-cpu-arguments-model-arguments-slope": 118,
		      "cekApplyCost-exBudgetMemory": 100,
		      "equalsString-memory-arguments": 1,
		      "multiplyInteger-memory-arguments-slope": 1,
		      "cekBuiltinCost-exBudgetMemory": 100,
		      "remainderInteger-memory-arguments-intercept": 0,
		      "sha2_256-cpu-arguments-intercept": 2477736,
		      "remainderInteger-cpu-arguments-model-arguments-intercept": 425507,
		      "lessThanEqualsByteString-memory-arguments": 1,
		      "tailList-memory-arguments": 32,
		      "mkNilData-cpu-arguments": 150000,
		      "chooseData-cpu-arguments": 150000,
		      "unBData-memory-arguments": 32,
		      "blake2b-memory-arguments": 4,
		      "iData-memory-arguments": 32,
		      "nullList-memory-arguments": 32,
		      "cekDelayCost-exBudgetCPU": 29773,
		      "subtractInteger-memory-arguments-intercept": 1,
		      "lessThanByteString-cpu-arguments-intercept": 103599,
		      "consByteString-cpu-arguments-slope": 1000,
		      "appendByteString-memory-arguments-slope": 1,
		      "trace-memory-arguments": 32,
		      "divideInteger-cpu-arguments-constant": 148000,
		      "cekConstCost-exBudgetCPU": 29773,
		      "encodeUtf8-memory-arguments-slope": 8,
		      "quotientInteger-cpu-arguments-model-arguments-intercept": 425507,
		      "mapData-memory-arguments": 32,
		      "appendString-cpu-arguments-slope": 1000,
		      "modInteger-cpu-arguments-constant": 148000,
		      "verifySignature-cpu-arguments-slope": 1,
		      "unConstrData-memory-arguments": 32,
		      "quotientInteger-memory-arguments-intercept": 0,
		      "equalsByteString-cpu-arguments-constant": 150000,
		      "sliceByteString-memory-arguments-intercept": 0,
		      "mkPairData-memory-arguments": 32,
		      "equalsByteString-cpu-arguments-intercept": 112536,
		      "appendString-memory-arguments-slope": 1,
		      "lessThanInteger-cpu-arguments-slope": 497,
		      "modInteger-cpu-arguments-model-arguments-intercept": 425507,
		      "modInteger-memory-arguments-minimum": 1,
		      "sha3_256-cpu-arguments-intercept": 0,
		      "verifySignature-memory-arguments": 1,
		      "cekLamCost-exBudgetMemory": 100,
		      "sliceByteString-cpu-arguments-intercept": 150000
		  }
	      }
	  }' --null-input > "$dir"/alonzo-genesis.json

        ## TODO: try to get rid of this step:
        Massage_the_key_file_layout_to_match_AWS "$profile_json" "$topo_dir" "$dir";;

    finalise-cache-entry )
        local usage="USAGE:  wb genesis $op PROFILE-JSON DIR"
        local profile_json=${1:?$usage}
        local dir=${2:?$usage}

        if profile has-preset "$profile_json"
        then return; fi

        ## Decide start time:
        local system_start_epoch=$(date '+%s' --date="now + $(jq .genesis.genesis_future_offset "$profile_json" --raw-output)")
        local system_start_human=$(date --date=@$system_start_epoch --utc +"%Y-%m-%dT%H:%M:%SZ")
        local start_time=$(date --date=@$system_start_epoch --utc --iso-8601=s |
                           cut -c-19)

        genesis_byron "$system_start_epoch" "$dir"

        jq ' $prof[0] as $p
           | . *
           { systemStart:                $start_time
           ##
           ## IMPORTANT: keep the rest in sync with the genesis-relevant bits
           ##            filtered out by "profile-cache-key-input".
           ##
           , activeSlotsCoeff:           $p.genesis.active_slots_coeff
           , epochLength:                $p.genesis.epoch_length
           , maxTxSize:                  $p.genesis.max_tx_size
           , securityParam:              $p.genesis.parameter_k
           , slotLength:                 $p.genesis.slot_duration
           , protocolParams:
             { "decentralisationParam":  $p.genesis.decentralisation_param
             , "maxBlockBodySize":       $p.genesis.max_block_size
             }
           }' --slurpfile prof       "$profile_json"  \
              --arg       start_time "${start_time}Z" \
               "$dir"/genesis.json |
        sponge "$dir"/genesis.json;;

    * ) usage_genesis;; esac
}

__KEY_ROOT=
Massage_the_key_file_layout_to_match_AWS() {
    local profile_json=${1:?$usage}
    local topo_dir=${2:?$usage}
    local dir=${3:?$usage}
    local ids

    set -euo pipefail

    local pool_density_map=$(topology density-map "$profile_json" "$topo_dir")
    msg "genesis: pool density map:  $pool_density_map"

    __KEY_ROOT=$dir

    ids=($(jq 'keys
              | join(" ")
              ' -cr <<<$pool_density_map))
    local bid=1 pid=1 did=1 ## (B)FT, (P)ool, (D)ense pool
    for id in ${ids[*]}
    do
        mkdir -p "$dir"/node-keys/cold

        #### cold keys (do not copy to production system)
        if   jqtest ".genesis.dense_pool_density > 1" $profile_json &&
             jqtest ".[\"$id\"]  > 1" <<<$pool_density_map
        then ## Dense/bulk pool
           msg "genesis:  bulk pool $did -> node-$id"
           cp -f $(key_genesis bulk      bulk $did) $(key_depl bulk   bulk $id)
           did=$((did + 1))
        elif jqtest ".[\"$id\"] != 0" <<<$pool_density_map
        then ## Singular pool
           msg "genesis:  pool $pid -> node-$id"
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
           msg "genesis:  BFT $bid -> node-$id"
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
            bulk )     stem=node-keys/bulk$id;;
            cold )     stem=node-keys/cold/operator$id;;
            opcert )   stem=node-keys/node$id.opcert;;
            KES )      stem=node-keys/node-kes$id;;
            VRF )      stem=node-keys/node-vrf$id;;
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
            bulk )     stem=pools/bulk$id;;
            cold )     stem=pools/cold$id;;
            opcert )   stem=pools/opcert$id;;
            KES )      stem=pools/kes$id;;
            VRF )      stem=pools/vrf$id;;
            deleg )    stem=delegate-keys/delegate$id;;
            delegCert )stem=delegate-keys/opcert$id;;
            delegKES ) stem=delegate-keys/delegate$id.kes;;
            delegVRF ) stem=delegate-keys/delegate$id.vrf;;
            * )        fail "key_genesis: unknown key type: '$type'";; esac
    echo "$__KEY_ROOT"/$stem$suffix
}

genesis_byron()
{
    local system_start_epoch=$1
    local dir=$2

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
    ' --null-input > "$dir"/byron-protocol-params.json
    cli_args=(
        ## Note that these parameters are irrelevant by now.
        --genesis-output-dir         "$dir"/byron
        --protocol-parameters-file   "$dir"/byron-protocol-params.json
        --start-time                 $system_start_epoch
        --k                          2160
        --protocol-magic             42
        --n-poor-addresses           1
        --n-delegate-addresses       1
        --total-balance              300000
        --delegate-share             0.9
        --avvm-entry-count           0
        --avvm-entry-balance         0
        )
    rm -rf "$dir"/byron
    cardano-cli byron genesis genesis "${cli_args[@]}"
}
