{ lib
, runCommand
, writeText
##
, cacheDir
, stateDir
, baseEnvConfig
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
    echo "genesis cache $genesis_cache_hit_desc:  $genesis_cache_id"

    regenesis_causes=()
    if   test -z "$genesis_cache_hit"
    then regenesis_causes+=(cache-miss)
    elif test -v __REWRITE_GENESIS_CACHE
    then regenesis_causes+=(__REWRITE_GENESIS_CACHE-env-var-defined)
    fi

    if test -n "''${regenesis_causes[*]}"
    then echo "generating genesis due to ''${regenesis_causes[*]}:  $genesis_cache_id @$genesis_cache_path"
         rm -rf "$genesis_cache_path"/{*-keys,pools,nodes,*.json,*.params}
         mkdir -p "$genesis_cache_path"

         ${shelleyGenesisCommon   "$genesis_cache_path"}

         ${if genesis.single_shot
           then
             ''
             ${shelleyGenesisSingleshot "$genesis_cache_path"}
             genesis_remap_key_names "$ids_pool_map"
             ''
           else if genesis.utxo > 0
                   || genesis.delegators > composition.n_pools
                   || composition.n_dense_hosts > 0
           then
             ''
             cat <<EOF
             Incremental (non single-shot) genesis does not support advanced features like:
               - stuffed UTxO
               - dense pools, and
               - extra delegators

             Please use a profile with single_shot = true.
             EOF
             exit 1
             ''
           else
             ''
             ${shelleyGenesisIncremental          "$genesis_cache_path"}
             ${bftCredentialsIncremental          "$genesis_cache_path"}
             ${poolCredentialsIncremental         "$genesis_cache_path"}
             ${hardcodedDefaultUtxoCredentials    "$genesis_cache_path"}

             genesis_remap_key_names "$ids_pool_map"

             ${buildIncrementalPoolRegistrationTx "$genesis_cache_path"}
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

  shelleyGenesisIncremental =
    dir:
    ''
    cli genesis create --genesis-dir ${dir} \
      ${toString
        (__trace "creating genesis for profile \"${name}\""
          cli_args.createSpec)}
    jq -r --arg     systemStart $system_start_human \
          --slurpfile genesisSpec ${dir}/genesis.spec.json \
           '.systemStart = $systemStart |
            .initialFunds = $genesisSpec[0].initialFunds |
            .updateQuorum = ${toString composition.n_bft_hosts}' \
           ${dir}/genesis.json |
    sponge ${dir}/genesis.json
    '';

  bftCredentialsIncremental =
    dir:
    ''
    mkdir -p "${dir}/nodes" "${dir}/node-keys"

    for i in {0..${toString (composition.n_bft_hosts - 1)}}
    do
      cli node key-gen-KES \
        --verification-key-file     "${dir}/node-keys/node-kes$i.vkey" \
        --signing-key-file          "${dir}/node-keys/node-kes$i.skey"
      cli node issue-op-cert \
        --kes-period 0 \
        --cold-signing-key-file     "${dir}/delegate-keys/delegate$((i+1)).skey" \
        --kes-verification-key-file "${dir}/node-keys/node-kes$i.vkey" \
        --operational-certificate-issue-counter-file \
                                    "${dir}/delegate-keys/delegate$((i+1)).counter" \
        --out-file                  "${dir}/node-keys/node$i.opcert"
      BFT_PORT=$(("${toString basePort}" + $i))
      echo "$BFT_PORT" >            "${dir}/nodes/node-$i.port"
    done
    '';
  poolCredentialsIncremental =
    dir:
    ''
    mkdir -p "${dir}/nodes" "${dir}/node-keys"

    for i in {${toString composition.n_bft_hosts}..${toString (composition.n_bft_hosts + composition.n_pool_hosts - 1)}}
    do
      echo "Generating Pool $i Secrets"
      cli address key-gen \
        --signing-key-file              "${dir}/node-keys/owner-utxo$i.skey" \
        --verification-key-file         "${dir}/node-keys/owner-utxo$i.vkey"
      cli stake-address key-gen \
        --signing-key-file              "${dir}/node-keys/owner-stake$i.skey" \
        --verification-key-file         "${dir}/node-keys/owner-stake$i.vkey"
      # Payment addresses
      cli address build \
        --payment-verification-key-file "${dir}/node-keys/owner-utxo$i.vkey" \
        --stake-verification-key-file   "${dir}/node-keys/owner-stake$i.vkey" \
        --testnet-magic                  ${toString genesis.network_magic} \
        --out-file                      "${dir}/node-keys/owner$i.addr"
      # Stake addresses
      cli stake-address build \
        --stake-verification-key-file   "${dir}/node-keys/owner-stake$i.vkey" \
        --testnet-magic                  ${toString genesis.network_magic} \
        --out-file                      "${dir}/node-keys/owner-stake$i.addr"
      # Stake addresses registration certs
      cli stake-address registration-certificate \
        --stake-verification-key-file   "${dir}/node-keys/owner-stake$i.vkey" \
        --out-file                      "${dir}/node-keys/stake$i.reg.cert"

      cli stake-address key-gen \
        --signing-key-file              "${dir}/node-keys/reward$i.skey" \
        --verification-key-file         "${dir}/node-keys/reward$i.vkey"
      # Stake reward addresses registration certs
      cli stake-address registration-certificate \
        --stake-verification-key-file   "${dir}/node-keys/reward$i.vkey" \
        --out-file                      "${dir}/node-keys/stake-reward$i.reg.cert"
      cli node key-gen \
        --cold-verification-key-file    "${dir}/node-keys/cold$i.vkey" \
        --cold-signing-key-file         "${dir}/node-keys/cold$i.skey" \
        --operational-certificate-issue-counter-file \
                                        "${dir}/node-keys/cold$i.counter"
      cli node key-gen-KES \
        --verification-key-file         "${dir}/node-keys/kes$i.vkey" \
        --signing-key-file              "${dir}/node-keys/kes$i.skey"
      cli node key-gen-VRF \
        --verification-key-file         "${dir}/node-keys/vrf$i.vkey" \
        --signing-key-file              "${dir}/node-keys/vrf$i.skey"

      # Stake address delegation certs
      cli stake-address delegation-certificate \
        --stake-verification-key-file   "${dir}/node-keys/owner-stake$i.vkey" \
        --cold-verification-key-file    "${dir}/node-keys/cold$i.vkey" \
        --out-file                      "${dir}/node-keys/owner-stake$i.deleg.cert"

      cli node issue-op-cert \
        --kes-period 0 \
        --cold-signing-key-file         "${dir}/node-keys/cold$i.skey" \
        --kes-verification-key-file     "${dir}/node-keys/kes$i.vkey" \
        --operational-certificate-issue-counter-file \
                                        "${dir}/node-keys/cold$i.counter" \
        --out-file                      "${dir}/node-keys/op$i.cert"

      echo "Generating Pool $i Metadata"
      mkdir -p "${dir}/webserver"
      jq -n \
         --arg name "TestPool$i" \
         --arg description "Test Pool $i" \
         --arg ticker "TEST$i" \
         --arg homepage "http://localhost:${toString basePort}/pool$i.html" \
         '{"name": $name, "description": $description, "ticker": $ticker, "homepage": $homepage}' \
                                      > "${dir}/webserver/pool$i.json"

      METADATA_URL="http://localhost:${toString basePort}/pool$i.json"
      METADATA_HASH=$(cli stake-pool metadata-hash \
                   --pool-metadata-file "${dir}/webserver/pool$i.json")
      POOL_IP="127.0.0.1"
      POOL_PORT=$(("${toString basePort}" + "${toString composition.n_bft_hosts}" + $i))
      echo "$POOL_PORT"               > "${dir}/nodes/node-$i.port"
      POOL_PLEDGE=${toString genesis.pool_coin}
      echo $POOL_PLEDGE               > "${dir}/nodes/node-$i.pledge"
      POOL_MARGIN_NUM=$(( $RANDOM % 10 + 1))

      cli stake-pool registration-certificate \
        --cold-verification-key-file    "${dir}/node-keys/cold$i.vkey" \
        --vrf-verification-key-file     "${dir}/node-keys/vrf$i.vkey" \
        --pool-pledge "$POOL_PLEDGE" \
        --pool-margin "$(jq -n $POOL_MARGIN_NUM/10)" \
        --pool-cost "$(($RANDOM % 100000000))" \
        --pool-reward-account-verification-key-file \
                                        "${dir}/node-keys/reward$i.vkey" \
        --pool-owner-stake-verification-key-file \
                                        "${dir}/node-keys/owner-stake$i.vkey" \
        --metadata-url "$METADATA_URL" \
        --metadata-hash "$METADATA_HASH" \
        --pool-relay-port "$POOL_PORT" \
        --pool-relay-ipv4 "127.0.0.1" \
        --testnet-magic ${toString genesis.network_magic} \
        --out-file                      "${dir}/node-keys/register$i.cert"

    done
    '';
  hardcodedDefaultUtxoCredentials =
    dir:
    ''
    cp ${./genesis-utxo.vkey} ${dir}/genesis-utxo.vkey
    cp ${./genesis-utxo.skey} ${dir}/genesis-utxo.skey
    '';
  buildIncrementalPoolRegistrationTx =
    dir:
    ''
    jq .protocolParams < ${dir}/genesis.json > ${dir}/pparams.json

    TXIN_ADDR=$(cli genesis initial-addr \
                    --testnet-magic ${toString genesis.network_magic} \
                    --verification-key-file ${dir}/genesis-utxo.vkey)

    cli transaction build-raw \
        --invalid-hereafter 1000 \
        --fee 0 \
        --tx-in $(cli genesis initial-txin \
                    --testnet-magic ${toString genesis.network_magic} \
                    --verification-key-file ${dir}/genesis-utxo.vkey) \
        --tx-out "$TXIN_ADDR+0" \
        ${lib.concatMapStringsSep "" (i: ''
          --tx-out $(cat "${dir}/nodes/node-${toString i}/owner.addr")+${toString genesis.delegator_coin} \
          --certificate-file "${dir}/node-keys/stake${toString i}.reg.cert" \
          --certificate-file "${dir}/node-keys/stake-reward${toString i}.reg.cert" \
          --certificate-file "${dir}/node-keys/register${toString i}.cert" \
          --certificate-file "${dir}/node-keys/owner-stake${toString i}.deleg.cert" \'') (lib.genList (i: i + composition.n_bft_hosts) composition.n_pool_hosts)}
        --out-file "${dir}/transfer-register-delegate-tx.txbody"
    FEE=$(cli transaction calculate-min-fee \
                --testnet-magic ${toString genesis.network_magic} \
                --genesis ${dir}/genesis.json \
                --tx-in-count 1 \
                --tx-out-count ${toString (composition.n_pools + 1)} \
                --witness-count ${toString (3 * composition.n_pools + 1)} \
                --byron-witness-count 0 \
                --tx-body-file "${dir}/transfer-register-delegate-tx.txbody" |
                cut -d' ' -f1)

    TXIN_ADDR_HEX=$(bech32 <<< "$TXIN_ADDR")

    TXOUT_AMOUNT=$(jq --arg addr "$TXIN_ADDR_HEX" \
                      --arg fee "$FEE" \
    '(if .initialFunds | has($addr)
      then .initialFunds[$addr]
      else error("initialFunds has no address corresponding to the genesis key: \($addr)")
      end) as $funds
    | $funds - ($fee|tonumber) - (.protocolParams.poolDeposit + (2 * .protocolParams.keyDeposit) + ${toString genesis.delegator_coin}) * ${toString composition.n_pools}' < ${dir}/genesis.json)
    cli transaction build-raw \
        --${era}-era \
        --invalid-hereafter 1000 \
        --fee "$FEE" \
        --tx-in $(cli genesis initial-txin \
                    --testnet-magic ${toString genesis.network_magic} \
                    --verification-key-file ${dir}/genesis-utxo.vkey) \
        --tx-out "$TXIN_ADDR+$TXOUT_AMOUNT" \
        ${lib.concatMapStringsSep "" (i: ''
          --tx-out $(cat "${dir}/nodes/node-${toString i}/owner.addr")+${toString genesis.delegator_coin} \
          --certificate-file "${dir}/node-keys/stake${toString i}.reg.cert" \
          --certificate-file "${dir}/node-keys/stake-reward${toString i}.reg.cert" \
          --certificate-file "${dir}/node-keys/register${toString i}.cert" \
          --certificate-file "${dir}/node-keys/owner-stake${toString i}.deleg.cert" \'') (lib.genList (i: i + 1) composition.n_pools)}
        --out-file "${dir}/transfer-register-delegate-tx.txbody"

    cli transaction sign \
      --signing-key-file ${dir}/genesis-utxo.skey \
      ${lib.concatMapStringsSep "" (i: ''
        --signing-key-file "${dir}/node-keys/owner-stake${toString i}.skey" \
        --signing-key-file "${dir}/node-keys/reward${toString i}.skey" \
        --signing-key-file "${dir}/node-keys/cold${toString i}.skey" \'') (lib.genList (i: i + 1) composition.n_pools)}
      --testnet-magic ${toString genesis.network_magic} \
      --tx-body-file  "${dir}/transfer-register-delegate-tx.txbody" \
      --out-file      "${dir}/transfer-register-delegate-tx.tx"
    '';
in
''
    mkdir -p ${genesisCacheDir}

    ${decideSystemStart}

    ${byronGenesis}

    ${shelleyGenesis}

    cat <<EOF
    Generated genesis for ${name} (cache id $genesis_cache_id):
      - BFT hosts:        ${toString composition.n_bft_hosts}
      - pool hosts:       ${toString (composition.n_hosts - composition.n_bft_hosts)}
      - pools:            ${toString composition.n_pools}, of them:
        - regular:          ${toString composition.n_singular_pools}
        - dense:            ${toString composition.n_dense_pools}, at ${toString composition.dense_pool_density} density in ${toString composition.n_dense_hosts} hosts
    EOF
''
