{ pkgs
, haskellProject
, profile
, nodeSpecs
, workload
}:

let

  # Packages
  ##########

  bashInteractive    = pkgs.bashInteractive;
  coreutils          = pkgs.coreutils;
  wget               = pkgs.wget;
  jq                 = pkgs.jq;
  # Avoid rebuilding on every commit because of `set-git-rev`.
  cardano-cli        = haskellProject.hsPkgs.cardano-cli.components.exes.cardano-cli;

  # Script params!
  ################

  testnet_magic = profile.genesis.network_magic;
  gov_action_deposit =
    if __hasAttr "conway" profile.genesis
    then profile.genesis.conway.govActionDeposit
    else throw "Conway genesis needed!"
  ;
  # Where to obtain the genesis funds from.
  genesis_funds_vkey = "../../genesis/cache-entry/utxo-keys/utxo2.vkey";
  genesis_funds_skey = "../../genesis/cache-entry/utxo-keys/utxo2.skey";
  # Initial donation from genesis funds to make "valid" withdrawal proposals.
  treasury_donation = 500000;

  # Filter producers from "node-specs.json".
  producers =
    builtins.filter
      (nodeSpec: nodeSpec.isProducer)
      (builtins.attrValues nodeSpecs)
  ;
  # Construct an "array" with node producers to use in BASH `for` loops.
  producers_bash_array =
      "("
    + (builtins.concatStringsSep
        " "
        (builtins.map
          (x: "\"" + x.name + "\"")
          producers
        )
      )
    + ")"
  ;

  # How many constitutions to create with the genesis funds (explorer node).
  constitutions_from_genesis = 1;
  # To calculate how much funds to leave on nodes' addresses (Prop 0 / DRep 0)
  # for the node to create withdrawal proposals (`--governance-action-deposit`).
  withdrawal_proposals_per_producer = 1;

  # UTxO preparation / creation / splitting phase.
  # When splitting the genesis funds, we first move from `genesis_funds_vkey` to
  # a "node address", called proposal-0 / DRep-0, for each producer, and then to
  # a "node-proposal address", called proposal-i / DRep-0, for each proposal and
  # last split is from each of this last node-proposal addressed to one
  # "node-proposal-drep address", called node-i-prop-j-drep-k, for each DRep the
  # node has assigned and will use to vote on each proposal.
  #                                   Genesis                                   
  #                                      |                                      
  #                  ----------------------------------------                   
  #                  |                                      |                   
  #                node-0                                 node-1                
  #                  |                                      |                   
  #        ---------------------                  ---------------------         
  #        |                   |                  |                   |         
  #    proposal-1          proposal-2         proposal-1          proposal-2    
  #        |                   |                  |                   |         
  #  -------------       -------------      -------------       -------------   
  #  |     |     |       |     |     |      |     |     |       |     |     |   
  # drep1 drep2 drep3   drep1 drep2 drep3  drep1 drep2 drep3   drep1 drep2 drep3
  #
  # Helper with the total number of producers.
  producers_count = builtins.length producers;
  # Helper with the total number of proposals created.
  proposals_count =
      constitutions_from_genesis
    + producers_count * withdrawal_proposals_per_producer
  ;
  dreps_per_producer = builtins.floor (profile.genesis.dreps / producers_count);
  # Max number of '--tx-out' when splitting funds.
  outs_per_split_transaction =
    workload.parameters.outs_per_split_transaction or 100
  ;

  # Sleeps.
  # Used when splitting funds to wait for funds to arrive, as this initial funds
  # are sent from a different process (not genesis) this works as a semaphore!
  wait_any_utxo_tries = 30;
  wait_any_utxo_sleep =  10; # 5 minutes in 10s steps.
  # Used when splitting funds, it waits for the expected UTxO to arrive to the
  # change-address and re-submits the transaction if necessary!
  wait_utxo_id_tries = 18;
  wait_utxo_id_sleep = 10; # 3 minutes in 10s steps.
  funds_submit_tries = 3;  # Submit the transaction up to this times on timeout.
  # Used when waiting for the recently created proposal.
  wait_proposal_id_tries = 30;
  wait_proposal_id_sleep = 10; # 5 minutes
  # Use to wait for all proposals to be available before we start voting.
  # As nodes will end their splitting phases at different times, this parameters
  # work as a formation lap before race start =).
  # No tries, waits forever!
  wait_proposals_count_sleep = 10;

  # No decimals also needed because of how the Bash script treats this number.
  votes_per_tx = builtins.ceil (workload.parameters.votes_per_tx or 1);
  # The most important one. To calculate and achieve a predictable TPS.
  # For reference:
  ### A local 2-node cluster with no tx-generator, max TPS was:
  ###### node-0: 5.356377670184436
  ###### node-1: 5.384256440453143
  ###### cluster: 10.443500635764893
  ### A 52-node Nomad cluster with value+voting:
  ###### One third of the nodes couldn't achieve 0.08 TPS (per node), this is
  ###### ~4 TPS cluster wide, when the requested TPS was 0.096 per node.
  desired_cluster_average_tps = 3.0; # Add decimals!!!!!
  # VOTES = TIME * TPS
  desired_producer_tps = desired_cluster_average_tps / producers_count;
  desired_producer_sleep = 1.0 / desired_producer_tps; # Add decimals!!!!!

  # Script behavior
  create_proposals = true;
  build_vote       = true; use_build_raw = true;
  sign_vote        = true;
  submit_vote      = workload.parameters.submit_vote or true;
  wait_submit      = workload.parameters.submit_vote or false;

in ''

# producers_count: ${toString producers_count}
# proposals_count: ${toString proposals_count}
# dreps_per_producer: ${toString dreps_per_producer}
# votes_per_tx: ${toString votes_per_tx}
# desired_cluster_average_tps: ${toString desired_cluster_average_tps}
# desired_producer_tps: ${toString desired_producer_tps}
# desired_producer_sleep: ${toString desired_producer_sleep}

${import ./utils/keys.nix
  { inherit coreutils jq cardano-cli testnet_magic;
  }
}

${import ./utils/utxo.nix
  { inherit coreutils cardano-cli jq testnet_magic;
    inherit outs_per_split_transaction funds_submit_tries;
    inherit wait_any_utxo_tries wait_any_utxo_sleep;
    inherit wait_utxo_id_tries wait_utxo_id_sleep;
    inherit wait_proposal_id_tries wait_proposal_id_sleep;
    inherit wait_proposals_count_sleep;
  }
}

################################################################################
# Evenly distribute the "utxo_*key" genesis funds to all producer nodes.
# To be called before `governance_funds_producer`.
# See above for a better explanation.
# Not to be run during the benchmarking phase: waits for funds to arrive!
################################################################################
function governance_funds_genesis {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local utxo_vkey=$2      # tx-in
  local utxo_skey=$3      # tx-in
  local producers=${toString producers_bash_array}

  # Send funds to each node (using DRep ID 0 as a special logical separation).
  ${coreutils}/bin/echo "governance_funds_genesis: Node(s) splitting phase! (''${node_str})"

  local action_deposit constitution_reminder
  action_deposit="${toString gov_action_deposit}"
  # HACK: Plus a fee estimate ("Estimated transaction fee: 172585 Lovelace").
  #       Plus "Minimum UTxO threshold: 105986 Lovelace"
  #       Not more than one split transaction from here (no profile has more than 52 nodes, no batch).
  constitution_reminder="$(( (action_deposit + 2000000) * ${toString constitutions_from_genesis} + 200000 ))"

  local producers_addrs_array=()
  for producer_name in ''${producers[*]}
  do
    local producer_i
    producer_i="$( \
      ${jq}/bin/jq --raw-output            \
        --arg keyName "''${producer_name}" \
        '.[$keyName].i'                    \
        ../../node-specs.json              \
    )"
    local producer_addr
    # Drep 0 is No DRep (funds for the node).
    producer_addr="$(build_x_y_z_address "''${producer_name}" "''${producer_i}" 0 0)"
    producers_addrs_array+=("''${producer_addr}")
    ${coreutils}/bin/echo "governance_funds_genesis: Splitting to: ''${producer_name} - ''${producer_i} - 0 - (''${producer_addr})"
  done

  # Split (no need to wait for the funds or re-submit, function takes care)!
  funds_from_to \
    "''${node_str}"                   \
    "''${utxo_vkey}" "''${utxo_skey}" \
    "''${constitution_reminder}"      \
    ${toString treasury_donation}     \
    "''${producers_addrs_array[@]}"
}

################################################################################
# Evenly distribute, for each proposal, producer funds to all producer DReps.
# To be called after `governance_funds_genesis`.
# See above for a better explanation.
# Not to be run during the benchmarking phase: waits for funds to arrive!
################################################################################
function governance_funds_producer {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local producer_name=$2

  # Send funds to each node-proposal combination.
  ${coreutils}/bin/echo "governance_funds_producer: Node(s)-Prop(s) splitting phase! (''${node_str})"

  local producer_i
  producer_i="$( \
    ${jq}/bin/jq --raw-output            \
      --arg keyName "''${producer_name}" \
      '.[$keyName].i'                    \
      ../../node-specs.json              \
  )"
  local producer_addr producer_vkey producer_skey
  producer_addr="$(build_x_y_z_address    "''${producer_name}" "''${producer_i}" 0 0)"
  producer_vkey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" 0 0)".vkey
  producer_skey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" 0 0)".skey

  # Wait for initial funds to arrive!
  ${coreutils}/bin/echo "governance_funds_producer: Wait for funds:  $(${coreutils}/bin/date --rfc-3339=seconds)"
  wait_any_utxo "''${node_str}" "''${producer_addr}"
  ${coreutils}/bin/echo "governance_funds_producer: Funds available: $(${coreutils}/bin/date --rfc-3339=seconds)"

  ###############################
  # Producer -> Proposals split #
  ###############################

  local action_deposit proposals_reminder
  action_deposit="${toString gov_action_deposit}"
  # HACK: Plus a fee estimate ("Estimated transaction fee: 374457 Lovelace").
  #       Plus "Minimum UTxO threshold: 105986 Lovelace"
  proposals_reminder="$(( (action_deposit + 4000000) * ${toString withdrawal_proposals_per_producer} + 200000 ))"

  local producer_prop_addr_array=()
  for prop_i in {1..${toString proposals_count}}
  do
    local producer_prop_addr
    producer_prop_addr="$(build_x_y_z_address "''${producer_name}" "''${producer_i}" "''${prop_i}" 0)"
    producer_prop_addr_array+=("''${producer_prop_addr}")
    ${coreutils}/bin/echo  "governance_funds_producer: Splitting to: ''${producer_name} - ''${producer_i} - ''${prop_i} - ''${producer_prop_addr}"
  done

  # Split (no need to wait for the funds or re-submit, function takes care)!
  funds_from_to \
    "''${node_str}"                           \
    "''${producer_vkey}" "''${producer_skey}" \
    "''${proposals_reminder}"                 \
    0                                         \
    "''${producer_prop_addr_array[@]}"

  ############################
  # Proposals -> DReps split #
  ############################

  local dreps_reminder
  # HACK: Plus "Minimum UTxO threshold: 105986 Lovelace"
  dreps_reminder="$(( 200000 ))"

  for prop_i in {1..${toString proposals_count}}
  do

    local producer_prop_vkey producer_prop_skey
    producer_prop_vkey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" "''${prop_i}" 0)".vkey
    producer_prop_skey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" "''${prop_i}" 0)".skey

    local producer_dreps_addrs_array=()
    local drep_step=0
    drep_step="$((producer_i * ${toString dreps_per_producer}))"
    local actual_drep
    for i in {1..${toString dreps_per_producer}}
    do
      local producer_drep_addr
      actual_drep="$((drep_step + i))"
      producer_drep_addr="$(build_x_y_z_address "''${producer_name}" "''${producer_i}" "''${prop_i}" "''${actual_drep}")"
      producer_dreps_addrs_array+=("''${producer_drep_addr}")
      ${coreutils}/bin/echo  "governance_funds_producer: Splitting to: ''${producer_name} - ''${producer_i} - ''${prop_i} - ''${actual_drep} - ''${producer_drep_addr}"
    done
  
    # Split (no need to wait for the funds or re-submit, function takes care)!
    funds_from_to \
      "''${node_str}"                                     \
      "''${producer_prop_vkey}" "''${producer_prop_skey}" \
      "''${dreps_reminder}"                               \
      0                                                   \
      "''${producer_dreps_addrs_array[@]}"

  done
}

################################################################################
# Create constitution proposal and wait for it in the `gov-state` query.
# Not to be run during the benchmarking phase: Waits for the expected UTxO and
# if timeout uses the first one available.
# Not to be run during the benchmarking phase: waits for proposal to appear!
################################################################################
function governance_create_constitution {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local utxo_vkey=$2
  local utxo_skey=$3

  ${coreutils}/bin/echo "governance_create_constitution: ''${node_str} - ''${utxo_vkey}"

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local node_addr
  node_addr="$( \
    ${cardano-cli}/bin/cardano-cli address build  \
      --testnet-magic ${toString testnet_magic}   \
      --payment-verification-key-file "''${utxo_vkey}"
  )"

  # Funds needed for this governance action ?
  local action_deposit
  action_deposit="${toString gov_action_deposit}"
  # Funds address.
  # The input is calculated from the last transaction submitted.
  # No waiting! But, if last submitted transaction fails (function
  # `governance_funds_genesis` in current workflow), everything else fails.
  local funds_tx
  funds_tx="$(get_address_utxo_expected_id "''${node_addr}")"

  # Show current gov-state.
    ${cardano-cli}/bin/cardano-cli conway query gov-state            \
      --testnet-magic ${toString testnet_magic}                      \
      --socket-path "''${socket_path}"                               \
  | ${jq}/bin/jq -r                                                  \
      '.nextRatifyState.nextEnactState.prevGovActionIds'

  # Copy base-line anchor.
  ${wget}/bin/wget                                                   \
    --output-document ./constitution.anchor.json                     \
    "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/cip-0100.common.schema.json"
  # Calculate anchor hash.
  ${cardano-cli}/bin/cardano-cli hash anchor-data                    \
    --file-text ./constitution.anchor.json                           \
    --out-file  ./constitution.anchor.hash
  # Create dummy constitution.
    ${coreutils}/bin/echo "My Constitution: free mate and asado"     \
  > ./constitution.txt
  # Calculate constitution hash.
  ${cardano-cli}/bin/cardano-cli hash anchor-data                    \
    --file-text ./constitution.txt                                   \
    --out-file  ./constitution.hash
  # Copy guardrails-script.
  ${coreutils}/bin/cp                                                \
    ../../genesis/guardrails-script.plutus                           \
    ./guardrails-script.plutus
  # Calculate guardrails-script hash.
  ${cardano-cli}/bin/cardano-cli hash script                         \
    --script-file ./guardrails-script.plutus                         \
    --out-file    ./guardrails-script.hash

  # Create action.
  local tx_filename=./create-constitution
  ${cardano-cli}/bin/cardano-cli conway governance action create-constitution \
    --testnet \
    --anchor-url "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/cip-0100.common.schema.json" \
    --anchor-data-hash "$(${coreutils}/bin/cat ./constitution.anchor.hash)" \
    --constitution-url "https://ipfs.io/ipfs/Qmdo2J5vkGKVu2ur43PuTrM7FdaeyfeFav8fhovT6C2tto" \
    --constitution-hash        "$(${coreutils}/bin/cat ./constitution.hash)" \
    --constitution-script-hash "$(${coreutils}/bin/cat ./guardrails-script.hash)" \
    --governance-action-deposit "''${action_deposit}" \
    --deposit-return-stake-verification-key-file ../../genesis/cache-entry/stake-delegators/delegator0/staking.vkey \
    --out-file "''${tx_filename}".action
  # Build transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction build            \
    --testnet-magic ${toString testnet_magic}                        \
    --socket-path "''${socket_path}"                                 \
    --tx-in "''${funds_tx}"                                          \
    --change-address "''${node_addr}"                                \
    --proposal-file "''${tx_filename}".action                        \
    --out-file      "''${tx_filename}".raw                           \
  > /dev/null
  # Sign transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction sign             \
    --testnet-magic ${toString testnet_magic}                        \
    --signing-key-file "''${utxo_skey}"                              \
    --tx-body-file "''${tx_filename}".raw                            \
    --out-file     "''${tx_filename}".signed
  # Submit transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction submit           \
    --testnet-magic ${toString testnet_magic}                        \
    --socket-path "''${socket_path}"                                 \
    --tx-file     "''${tx_filename}".signed                          \
  > /dev/null

  # Wait for the proposal without releasing the local socket.
  wait_proposal_id "''${node_str}" "''${tx_filename}".signed >/dev/null

  store_address_utxo_expected \
    "''${tx_filename}.signed" \
    "''${node_addr}"
}

################################################################################
# Create withdrawal proposal and wait for it in the `gov-state` query.
# Not to be run during the benchmarking phase: Waits for the expected UTxO and
# if timeout uses the first one available.
# Not to be run during the benchmarking phase: waits for proposal to appear!
################################################################################
function governance_create_withdrawal {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local node_i=$2         # This "i" is part of the node name ("node-i").
  local drep_i=$3

  ${coreutils}/bin/echo "governance_create_withdrawal: ''${node_str} - ''${drep_i}"

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local node_drep_skey node_drep_addr
  node_drep_skey="$(create_x_y_z_key_files "''${node_str}" "''${node_i}" 0 "''${drep_i}")".skey
  node_drep_addr="$(build_x_y_z_address    "''${node_str}" "''${node_i}" 0 "''${drep_i}")"

  # Funds needed for this governance action ?
  local action_deposit
  action_deposit="${toString gov_action_deposit}"
  # Funds address.
  # The input is calculated from the last transaction submitted.
  # No waiting! But, if last submitted transaction fails (function
  # `governance_funds_producer` current workflow), everything else fails.
  local funds_tx
  funds_tx="$(get_address_utxo_expected_id "''${node_drep_addr}")"

  # Copy base-line anchor.
  ${wget}/bin/wget                                                   \
    --output-document ./treasury-withdrawal.json                     \
    "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0108/examples/treasury-withdrawal.jsonld"
  # Calculate anchor hash.
  ${cardano-cli}/bin/cardano-cli hash anchor-data                    \
    --file-text   ./treasury-withdrawal.json                         \
    --out-file    ./treasury-withdrawal.hash
 
  local tx_filename=./create-withdrawal."''${node_str}"."''${drep_i}"
  # Create action.
  ${cardano-cli}/bin/cardano-cli conway governance action create-treasury-withdrawal \
    --testnet                                                                                                                    \
    --anchor-url "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0108/examples/treasury-withdrawal.jsonld" \
    --anchor-data-hash "$(${coreutils}/bin/cat ./treasury-withdrawal.hash)"                                                      \
    --governance-action-deposit "''${action_deposit}"                                                                            \
    --transfer 50                                                                                                                \
    --deposit-return-stake-verification-key-file  ../../genesis/cache-entry/stake-delegators/"delegator''${node_i}"/staking.vkey \
    --funds-receiving-stake-verification-key-file ../../genesis/cache-entry/stake-delegators/"delegator''${node_i}"/staking.vkey \
    --out-file "''${tx_filename}".action
  # Build transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction build            \
    --testnet-magic ${toString testnet_magic}                        \
    --socket-path "''${socket_path}"                                 \
    --tx-in "''${funds_tx}"                                          \
    --change-address "''${node_drep_addr}"                           \
    --proposal-file "''${tx_filename}".action                        \
    --out-file      "''${tx_filename}".raw                           \
  > /dev/null
  # Sign transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction sign             \
    --testnet-magic ${toString testnet_magic}                        \
    --signing-key-file "''${node_drep_skey}"                         \
    --tx-body-file "''${tx_filename}".raw                            \
    --out-file     "''${tx_filename}".signed
  # Submit transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction submit           \
    --testnet-magic ${toString testnet_magic}                        \
    --socket-path "''${socket_path}"                                 \
    --tx-file     "''${tx_filename}".signed                          \
  > /dev/null

  # Wait for the proposal without releasing the local socket.
  wait_proposal_id "''${node_str}" "''${tx_filename}".signed >/dev/null

  store_address_utxo_expected \
    "''${tx_filename}.signed" \
    "''${node_drep_addr}"
}

################################################################################
# Sleeps the node between votes submissions to achieve the desired TPS.
################################################################################

function vote_tps_throttle() {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local txs_count=$2      # Actual number of total txs already submitted.

  local filepath_first=./first_vote_time
  local filepath_last=./last_vote_time
  if ! test -f "''${filepath_first}"
  then
    local start_time
    start_time="$(${coreutils}/bin/date +"%s.%3N")" # With milliseconds / decimals.
    ${coreutils}/bin/echo "''${start_time}" > "''${filepath_first}"
    ${coreutils}/bin/echo "''${start_time}" > "''${filepath_last}"
  else
    local start_time last_time
    start_time="$(${coreutils}/bin/cat "''${filepath_first}")"
    last_time="$(${coreutils}/bin/cat  "''${filepath_last}")"
    local current_time
    current_time="$(${coreutils}/bin/date +"%s.%3N")" # With milliseconds / decimals.
    ${coreutils}/bin/echo "''${current_time}" > "''${filepath_last}"
    local sleep_arg_number
    sleep_arg_number="$(                               \
      ${jq}/bin/jq --null-input --raw-output           \
        --argjson start_time   "''${start_time}"       \
        --argjson last_time    "''${last_time}"        \
        --argjson current_time "''${current_time}"     \
        --argjson txs_count    "''${txs_count}"      \
        '
          if $txs_count < 3
          then 0
          else (
              ($current_time - $start_time  )    as $cluster_seconds
            | ($txs_count  / $cluster_seconds) as $current_tps
            | if $current_tps < ${toString desired_producer_tps}
              then 0
              else (
                  (($txs_count + 1) / ${toString desired_producer_tps}) as $next_cluster_seconds
                | ($start_time + $next_cluster_seconds - $current_time)
              )
              end
          )
          end
        '                                              \
    )"
    ${coreutils}/bin/echo "vote_tps_throttle: ''${node_str}: ''${sleep_arg_number}"
    if ! test "''${sleep_arg_number}" = "0"
    then
      ${coreutils}/bin/sleep "''${sleep_arg_number}"
    fi
  fi
}

################################################################################
# Benchmarking phase function!
# Fetch all proposals and call `governance_vote_proposal` with each proposal ID
# so the node votes that proposal with all the DReps it controls.
################################################################################
function governance_vote_all {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local node_i=$2         # This "i" is part of the node name ("node-i").

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  # Don't query the node while voting!
  local txIdsJSON proposal_tx_ids_array
  txIdsJSON="$( \
      ${cardano-cli}/bin/cardano-cli conway query gov-state          \
        --testnet-magic ${toString testnet_magic}                    \
        --socket-path "''${socket_path}"                             \
    | ${jq}/bin/jq '.proposals | map(.actionId.txId)'                \
  )"
  proposal_tx_ids_array=$(echo "''${txIdsJSON}" | ${jq}/bin/jq --raw-output '. | join (" ")')

  # Keep a TXs counter for TPS calculation.
  local txs_count=0 # Actual number of total txs already submitted.
  # To calculate DReps assigned to this node.
  local drep_step
  drep_step=$(( node_i * ${toString dreps_per_producer} ))

  # Cycle proposals.
  local prop_i=0
  for proposal_tx_id in ''${proposal_tx_ids_array[*]}
  do
    # Prop key to use.
    prop_i=$(( prop_i + 1 ))
    # Dreps to use (a voting transaction can carry more than 1 vote).
    local dreps_array=()
    for i in {1..${toString dreps_per_producer}}
    do
      local actual_drep
      actual_drep="$((drep_step + i))"
      dreps_array+=("''${actual_drep}")
      if test "''${#dreps_array[@]}" -ge ${toString votes_per_tx}
      then
        vote_tps_throttle "''${node_str}" "''${txs_count}"
        governance_vote_proposal \
          "''${node_str}"        \
          "''${node_i}"          \
          "''${prop_i}"          \
          "''${proposal_tx_id}"  \
          "''${dreps_array[@]}"
        txs_count=$(( txs_count + 1 ))
        dreps_array=()
      fi
    done
    local proposal_flag="./proposal.''${proposal_tx_id}.voted"
    ${coreutils}/bin/touch "''${proposal_flag}"
  done
}

################################################################################
# Benchmarking phase function!
# The node votes the proposal with all the DReps it controls.
################################################################################
function governance_vote_proposal {

  # Function arguments.
  local node_str=''${1};       shift # node name / folder to find the socket to use.
  local node_i=''${1};         shift # This "i" is part of the node name ("node-i").
  local prop_i=''${1};         shift
  local proposal_tx_id=''${1}; shift # Proposal key/address to use.
  local dreps_array=("$@")           # DReps to use in this voting transaction.

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  ${coreutils}/bin/echo "governance_vote_proposal: ''${proposal_tx_id} - ''${node_str} - ''${dreps_array[0]}-''${dreps_array[-1]}"

  local funds_addr funds_tx funds_value
  local vote_file_params_array=()
  local signing_key_file_params_array=()
  for drep_i in ''${dreps_array[*]}
  do
    local node_drep_skey node_drep_addr
    node_drep_skey="$(create_x_y_z_key_files "''${node_str}" "''${node_i}" "''${prop_i}" "''${drep_i}")".skey
    node_drep_addr="$(build_x_y_z_address    "''${node_str}" "''${node_i}" "''${prop_i}" "''${drep_i}")"
    # UTxO are created for 1 vote per transaction so all runs have the same
    # number of UTxOs. We grab the funds from the first address/UTxO.
    if test -z "''${funds_tx-}"
    then
      # Funds address.
      funds_addr="''${node_drep_addr}"
      # The input is calculated from the last transaction submitted.
      # No waiting! But, if last submitted transaction fails (function
      # `governance_funds_producer` or `governance_vote_proposal` in current
      # workflow), everything else fails.
      funds_tx="$(get_address_utxo_expected_id "''${node_drep_addr}")"
      # A next UTxO must be cached by `funds_from_to` when splitting the funds,
      # we don't check the response to be sure there is an expected UTxO to be
      # really sure we are not querying the node unnecessarily.
      funds_value="$(get_address_utxo_expected_value "''${node_drep_addr}")"
      # Need to be used twice to sign
      signing_key_file_params_array+=("--signing-key-file ''${node_drep_skey}")
    fi
    local vote_filename=./proposal."''${proposal_tx_id}"."''${drep_i}"
    ${cardano-cli}/bin/cardano-cli conway governance vote create   \
      --yes                                                        \
      --governance-action-tx-id "''${proposal_tx_id}"              \
      --governance-action-index "0"                                \
      --drep-verification-key-file ../../genesis/cache-entry/drep-keys/drep"''${drep_i}"/drep.vkey \
      --out-file "''${vote_filename}".action
    vote_file_params_array+=("--vote-file ''${vote_filename}.action")
    signing_key_file_params_array+=("--signing-key-file ../../genesis/cache-entry/drep-keys/drep''${drep_i}/drep.skey")
  done

  local tx_filename=./proposal."''${proposal_tx_id}"."''${dreps_array[0]}"-"''${dreps_array[-1]}"
  # Build the transaction.
  ${if build_vote
  then (
  if use_build_raw
  then ''
  local change
  change=$(( $funds_value - 250000 ))
  ${cardano-cli}/bin/cardano-cli conway transaction build-raw    \
    --tx-in "''${funds_tx}"                                      \
    --fee 250000                                                 \
    --tx-out "''${funds_addr}"+"''${change}"                     \
    ''${vote_file_params_array[@]}                               \
    --out-file "''${tx_filename}".raw
  ''
  else ''
  ${cardano-cli}/bin/cardano-cli conway transaction build        \
    --testnet-magic ${toString testnet_magic}                    \
    --socket-path "''${socket_path}"                             \
    --tx-in "''${funds_tx}"                                      \
    --change-address "''${funds_addr}"                           \
    --witness-override "$(( 1 + ''${#dreps_array[@]} ))"         \
    ''${vote_file_params_array[@]}                               \
    --out-file "''${tx_filename}".raw                            \
  > /dev/null
  ''
  )
  else ''
  ${coreutils}/bin/echo "transaction build off!"
  ''
  }
  ${if build_vote && sign_vote
  then ''
  # Sign it with the DRep key:
  ${cardano-cli}/bin/cardano-cli conway transaction sign         \
    --testnet-magic ${toString testnet_magic}                    \
    ''${signing_key_file_params_array[@]}                        \
    --tx-body-file "''${tx_filename}".raw                        \
    --out-file "''${tx_filename}".signed
  ''
  else ''
  ${coreutils}/bin/echo "transaction sign off!"
  ''
  }
  ${if build_vote && sign_vote && submit_vote
  then ''
  # Submit the transaction:
  ${cardano-cli}/bin/cardano-cli conway transaction submit       \
    --testnet-magic ${toString testnet_magic}                    \
    --socket-path "''${socket_path}"                             \
    --tx-file "''${tx_filename}".signed                          \
  >/dev/null ${if wait_submit then "&" else ""}
  ''
  else ''
  ${coreutils}/bin/echo "transaction submit off!"
  ''
  }
  ${coreutils}/bin/touch "''${tx_filename}".voted

  # No `store_address_utxo_expected`, all UTxOs are created before voting!

}

################################################################################
# Entrypoints.
################################################################################

function workflow_generator_log_proposals {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"
  while true
  do
        ${cardano-cli}/bin/cardano-cli conway query gov-state                \
          --testnet-magic ${toString testnet_magic}                          \
          --socket-path "''${socket_path}"                                   \
      | ${jq}/bin/jq '.proposals'                                            \
    > ./proposals."''$(${coreutils}/bin/date +"%Y-%m-%d-%H-%M-%S-%3N")".json
    ${coreutils}/bin/sleep 60
  done
}

function workflow_generator {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.

  #- Splitting 0 --------------------------------------------------------------#
  ${coreutils}/bin/echo "governance_funds_genesis:        Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_funds_genesis       \
    "''${node_str}"                \
    ${toString genesis_funds_vkey} \
    ${toString genesis_funds_skey}
  ${coreutils}/bin/echo "governance_funds_genesis:        End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #- Preparing ----------------------------------------------------------------#
  ${if create_proposals
  then ''
  ${coreutils}/bin/echo "governance_create_constitution:  Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_create_constitution   \
    "''${node_str}"                \
    ${toString genesis_funds_vkey} \
    ${toString genesis_funds_skey}
  ${coreutils}/bin/echo "governance_create_constitution:  End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  ''
  else ''
  ${coreutils}/bin/echo "No governance_create_constitution today!"
  ''
  }
  #- Waiting ------------------------------------------------------------------#
  ${coreutils}/bin/echo "wait_proposals_count:            Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  wait_proposals_count "''${node_str}" ${toString proposals_count}
  ${coreutils}/bin/echo "wait_proposals_count:            End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #- Log ----------------------------------------------------------------------#
  # Keep a job that periodically stores the proposals from the gov-state.
  workflow_generator_log_proposals "''${node_str}" &

}

function workflow_producer {
  # Run the producer workflow for each deployed producer.
  local producers=${toString producers_bash_array}
  for producer_name in ''${producers[*]}
  do
    if test -d "../../''${producer_name}"
    then
      workflow_producer_deployed "''${producer_name}"
    fi
  done
}

function workflow_producer_deployed {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.

  local producer_i
  producer_i="$( \
    ${jq}/bin/jq --raw-output            \
      --arg keyName "''${node_str}"      \
      '.[$keyName].i'                    \
      ../../node-specs.json              \
  )"

  #- Splitting 1 --------------------------------------------------------------#
  ${coreutils}/bin/echo "governance_funds_producer:       Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_funds_producer "''${node_str}" "''${node_str}"
  ${coreutils}/bin/echo "governance_funds_producer:       End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #- Preparing ----------------------------------------------------------------#
  ${if create_proposals
  then ''
  ${coreutils}/bin/echo "governance_create_withdrawal(s): Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  for i in {1..${toString withdrawal_proposals_per_producer}}
  do
    governance_create_withdrawal "''${node_str}" "''${producer_i}" 0
  done
  ${coreutils}/bin/echo "governance_create_withdrawal(s): End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #- Waiting ------------------------------------------------------------------#
  ${coreutils}/bin/echo "wait_proposals_count:            Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  wait_proposals_count "''${node_str}" ${toString proposals_count}
  ${coreutils}/bin/echo "wait_proposals_count:            End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #- Benchmarking -------------------------------------------------------------#
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"
  # Store actual gov-state
    ${cardano-cli}/bin/cardano-cli conway query gov-state            \
      --testnet-magic ${toString testnet_magic}                      \
      --socket-path "''${socket_path}"                               \
  > "./gov-state.start.json"
  ${coreutils}/bin/echo "governance_vote_all:             Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_vote_all "''${node_str}" "''${producer_i}"
  ${coreutils}/bin/echo "governance_vote_all:             End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  socket_path="$(get_socket_path "''${node_str}")"
  # Store actual gov-state
    ${cardano-cli}/bin/cardano-cli conway query gov-state            \
      --testnet-magic ${toString testnet_magic}                      \
      --socket-path "''${socket_path}"                               \
  > "./gov-state.end.json"
  #----------------------------------------------------------------------------#
  ''
  else ''
  ${coreutils}/bin/echo "No governance_create_withdrawal(s) today!"
  ''
  }

}

''
