{ lib
, runCommand
, writeText
##
, path
, cacheDir
, stateDir
, baseEnvConfig
, basePort
##
, profile
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

  genesisCacheDir = "${cacheDir}/genesis";

  shelleyGenesis =
    p: ## profile
    ''
    ## Determine the genesis cache entry:

    genesis_cache_params=$(jq '.genesis * .composition |
                              del(.active_slots_coeff) |
                              del(.epoch_length) |
                              del(.parameter_k) |
                              del(.slot_duration) |
                              del(.max_block_size) |
                              del(.max_tx_size) |
                              del(.verbatim) |
                              del(.era) |
                              del(.genesis_future_offset) |
                              del(.byron) |
                              del(.locations)
                             ' --sort-keys <<<'${__toJSON p}')

    genesis_params_hash=$(echo "$genesis_cache_params" | sha1sum | cut -c-7)
    genesis_cache_id=$(jq <<<'${__toJSON p}' \
       '"k\(.composition.n_pools)-d\(.composition.dense_pool_density)-\(.genesis.delegators / 1000)kD-\(.genesis.utxo / 1000)kU-\($params_hash)"
       ' --arg params_hash "$genesis_params_hash" --raw-output)
    genesis_cache_path="${genesisCacheDir}/$genesis_cache_id"

    ## Handle genesis cache hit/miss:

    if test -f "$genesis_cache_path"/genesis.json
    then genesis_cache_hit=t; genesis_cache_hit_desc='hit'
    else genesis_cache_hit=;  genesis_cache_hit_desc='miss'; fi
    echo "genesis cache $genesis_cache_hit_desc:  $genesis_cache_id"

    if test -z "$genesis_cache_hit"
    then echo "generating genesis due to cache miss:  $genesis_cache_id @$genesis_cache_path"
         mkdir -p "$genesis_cache_path"

         ${shelleyGenesisSpec                 "$genesis_cache_path"}
         ${shelleyGenesisVerbatim             "$genesis_cache_path"}

         ${if genesis.single_shot
           then
             ''
             echo "Single-shot genesis not supported yet."
             exit 1
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
             ${buildIncrementalPoolRegistrationTx "$genesis_cache_path"}
             ''}

         cat <<<$genesis_cache_params > "$genesis_cache_path"/cache.params
         cat <<<$genesis_cache_id     > "$genesis_cache_path"/cache.params.id
    fi

    ## Switch to the genesis cache entry:

    rm -f                                     ${stateDir}/shelley
    ln -s "$(realpath "$genesis_cache_path")" ${stateDir}/shelley

    ## Update start time:

    start_time=$(date --iso-8601=s --date="now + ${genesis.genesis_future_offset}" --utc | cut -c-19)
    jq '. // { systemStart: "''${start_time}Z" }
       ' ${stateDir}/shelley/genesis.json |
    sponge ${stateDir}/genesis.json
    '';

  shelleyGenesisSpec =
    dir:
    ''
    cli genesis create --genesis-dir ${dir}/ ${toString cli_args.createSpec}
    '';

  shelleyGenesisVerbatim =
    dir:
    ''
    jq -r --argjson genesisVerb '${__toJSON genesis.verbatim}' \
           '. * $genesisVerb' \
           ${dir}/genesis.spec.json |
    sponge ${dir}/genesis.spec.json
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
    for i in {0..${toString (composition.n_bft_hosts - 1)}}
    do
      mkdir -p "${dir}/nodes/node-$i"
      ln -s "../../delegate-keys/delegate$((i+1)).vrf.skey" "${dir}/nodes/node-$i/vrf.skey"
      ln -s "../../delegate-keys/delegate$((i+1)).vrf.vkey" "${dir}/nodes/node-$i/vrf.vkey"
      cli node key-gen-KES \
        --verification-key-file "${dir}/nodes/node-$i/kes.vkey" \
        --signing-key-file "${dir}/nodes/node-$i/kes.skey"
      cli node issue-op-cert \
        --kes-period 0 \
        --cold-signing-key-file "${dir}/delegate-keys/delegate$((i+1)).skey" \
        --kes-verification-key-file "${dir}/nodes/node-$i/kes.vkey" \
        --operational-certificate-issue-counter-file "${dir}/delegate-keys/delegate$((i+1)).counter" \
        --out-file "${dir}/nodes/node-$i/op.cert"
      BFT_PORT=$(("${toString basePort}" + $i))
      echo "$BFT_PORT" > "${dir}/nodes/node-$i/port"
    done
    '';
  poolCredentialsIncremental =
    dir:
    ''
    for i in {${toString composition.n_bft_hosts}..${toString (composition.n_bft_hosts + composition.n_pool_hosts - 1)}}
    do
      mkdir -p "${dir}/nodes/node-$i"
      echo "Generating Pool $i Secrets"
      cli address key-gen \
        --signing-key-file "${dir}/nodes/node-$i/owner-utxo.skey" \
        --verification-key-file "${dir}/nodes/node-$i/owner-utxo.vkey"
      cli stake-address key-gen \
        --signing-key-file "${dir}/nodes/node-$i/owner-stake.skey" \
        --verification-key-file "${dir}/nodes/node-$i/owner-stake.vkey"
      # Payment addresses
      cli address build \
        --payment-verification-key-file "${dir}/nodes/node-$i/owner-utxo.vkey" \
        --stake-verification-key-file "${dir}/nodes/node-$i/owner-stake.vkey" \
        --testnet-magic ${toString genesis.network_magic} \
        --out-file "${dir}/nodes/node-$i/owner.addr"
      # Stake addresses
      cli stake-address build \
        --stake-verification-key-file "${dir}/nodes/node-$i/owner-stake.vkey" \
        --testnet-magic ${toString genesis.network_magic} \
        --out-file "${dir}/nodes/node-$i/owner-stake.addr"
      # Stake addresses registration certs
      cli stake-address registration-certificate \
        --stake-verification-key-file "${dir}/nodes/node-$i/owner-stake.vkey" \
        --out-file "${dir}/nodes/node-$i/stake.reg.cert"

      cli stake-address key-gen \
        --signing-key-file "${dir}/nodes/node-$i/reward.skey" \
        --verification-key-file "${dir}/nodes/node-$i/reward.vkey"
      # Stake reward addresses registration certs
      cli stake-address registration-certificate \
        --stake-verification-key-file "${dir}/nodes/node-$i/reward.vkey" \
        --out-file "${dir}/nodes/node-$i/stake-reward.reg.cert"
      cli node key-gen \
        --cold-verification-key-file "${dir}/nodes/node-$i/cold.vkey" \
        --cold-signing-key-file "${dir}/nodes/node-$i/cold.skey" \
        --operational-certificate-issue-counter-file "${dir}/nodes/node-$i/cold.counter"
      cli node key-gen-KES \
        --verification-key-file "${dir}/nodes/node-$i/kes.vkey" \
        --signing-key-file "${dir}/nodes/node-$i/kes.skey"
      cli node key-gen-VRF \
        --verification-key-file "${dir}/nodes/node-$i/vrf.vkey" \
        --signing-key-file "${dir}/nodes/node-$i/vrf.skey"

      # Stake address delegation certs
      cli stake-address delegation-certificate \
        --stake-verification-key-file "${dir}/nodes/node-$i/owner-stake.vkey" \
        --cold-verification-key-file  "${dir}/nodes/node-$i/cold.vkey" \
        --out-file "${dir}/nodes/node-$i/owner-stake.deleg.cert"

      cli node issue-op-cert \
        --kes-period 0 \
        --cold-signing-key-file "${dir}/nodes/node-$i/cold.skey" \
        --kes-verification-key-file "${dir}/nodes/node-$i/kes.vkey" \
        --operational-certificate-issue-counter-file "${dir}/nodes/node-$i/cold.counter" \
        --out-file "${dir}/nodes/node-$i/op.cert"

      echo "Generating Pool $i Metadata"
      mkdir -p "${dir}/webserver"
      jq -n \
         --arg name "TestPool$i" \
         --arg description "Test Pool $i" \
         --arg ticker "TEST$i" \
         --arg homepage "http://localhost:${toString basePort}/pool$i.html" \
         '{"name": $name, "description": $description, "ticker": $ticker, "homepage": $homepage}' > "${dir}/webserver/pool$i.json"

      METADATA_URL="http://localhost:${toString basePort}/pool$i.json"
      METADATA_HASH=$(cli stake-pool metadata-hash --pool-metadata-file "${dir}/webserver/pool$i.json")
      POOL_IP="127.0.0.1"
      POOL_PORT=$(("${toString basePort}" + "${toString composition.n_bft_hosts}" + $i))
      echo "$POOL_PORT" > "${dir}/nodes/node-$i/port"
      POOL_PLEDGE=${toString genesis.pool_coin}
      echo $POOL_PLEDGE > "${dir}/nodes/node-$i/pledge"
      POOL_MARGIN_NUM=$(( $RANDOM % 10 + 1))

      cli stake-pool registration-certificate \
        --cold-verification-key-file "${dir}/nodes/node-$i/cold.vkey" \
        --vrf-verification-key-file "${dir}/nodes/node-$i/vrf.vkey" \
        --pool-pledge "$POOL_PLEDGE" \
        --pool-margin "$(jq -n $POOL_MARGIN_NUM/10)" \
        --pool-cost "$(($RANDOM % 100000000))" \
        --pool-reward-account-verification-key-file "${dir}/nodes/node-$i/reward.vkey" \
        --pool-owner-stake-verification-key-file "${dir}/nodes/node-$i/owner-stake.vkey" \
        --metadata-url "$METADATA_URL" \
        --metadata-hash "$METADATA_HASH" \
        --pool-relay-port "$POOL_PORT" \
        --pool-relay-ipv4 "127.0.0.1" \
        --testnet-magic ${toString genesis.network_magic} \
        --out-file "${dir}/nodes/node-$i/register.cert"

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
          --certificate-file "${dir}/nodes/node-${toString i}/stake.reg.cert" \
          --certificate-file "${dir}/nodes/node-${toString i}/stake-reward.reg.cert" \
          --certificate-file "${dir}/nodes/node-${toString i}/register.cert" \
          --certificate-file "${dir}/nodes/node-${toString i}/owner-stake.deleg.cert" \'') (lib.genList (i: i + composition.n_bft_hosts) composition.n_pool_hosts)}
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
          --certificate-file "${dir}/nodes/node-${toString i}/stake.reg.cert" \
          --certificate-file "${dir}/nodes/node-${toString i}/stake-reward.reg.cert" \
          --certificate-file "${dir}/nodes/node-${toString i}/register.cert" \
          --certificate-file "${dir}/nodes/node-${toString i}/owner-stake.deleg.cert" \'') (lib.genList (i: i + 1) composition.n_pools)}
        --out-file "${dir}/transfer-register-delegate-tx.txbody"

    cli transaction sign \
      --signing-key-file ${dir}/genesis-utxo.skey \
      ${lib.concatMapStringsSep "" (i: ''
        --signing-key-file "${dir}/nodes/node-${toString i}/owner-stake.skey" \
        --signing-key-file "${dir}/nodes/node-${toString i}/reward.skey" \
        --signing-key-file "${dir}/nodes/node-${toString i}/cold.skey" \'') (lib.genList (i: i + 1) composition.n_pools)}
      --testnet-magic ${toString genesis.network_magic} \
      --tx-body-file  "${dir}/transfer-register-delegate-tx.txbody" \
      --out-file      "${dir}/transfer-register-delegate-tx.tx"
    '';
in
''
    PATH=$PATH:${path}
    mkdir -p ${genesisCacheDir}

    ${decideSystemStart}

    ${byronGenesis}

    ${shelleyGenesis profile}

    cat <<EOF
    Generated genesis for ${name} (cache id $genesis_cache_id):
      - BFT hosts:        ${toString composition.n_bft_hosts}
      - pool hosts:       ${toString (composition.n_hosts - composition.n_bft_hosts)}
      - pools:            ${toString composition.n_pools}, of them:
        - regular:          ${toString composition.n_singular_pools}
        - dense:            ${toString composition.n_dense_pools}, at ${toString composition.dense_pool_density} density in ${toString composition.n_dense_hosts} hosts
    EOF
''
