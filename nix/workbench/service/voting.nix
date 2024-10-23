{ pkgs
, profile
, nodeSpecs
}:

let

  # Packages
  ##########

  bashInteractive    = pkgs.bashInteractive;
  coreutils          = pkgs.coreutils;
  jq                 = pkgs.jq;
  cardano-cli        = pkgs.cardanoNodePackages.cardano-cli;

  # Script params!
  ################

  testnetMagic = profile.genesis.network_magic;
  gov_action_deposit = profile.genesis.conway.govActionDeposit;
  # Where to obtain the genesis funds from.
  genesis_funds_vkey = "../genesis/cache-entry/utxo-keys/utxo2.vkey";
  genesis_funds_skey = "../genesis/cache-entry/utxo-keys/utxo2.skey";
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
  # DUCT TAPE: Split 10000 DReps to 52 nodes in 1 TX per node.
  #            Signed tx file is 16337 bytes of a tx maximum of 16384 bytes!!!
  outs_per_transaction = 193;

  # Sleeps.
  # Used when splitting funds to wait for them to arrive, a pretty high number!
  # Any UTxO is considered a success in this phase.
  wait_any_utxo_tries = 30;
  wait_any_utxo_sleep =  10; # 5 minutes in 10s steps.
  # Used when sending funds if they need to be batched, it waits for the
  # expected UTxO to arrive to the change-address. More than this is a failure!
  wait_utxo_id_tries = 24;
  wait_utxo_id_sleep = 10; # 4 minutes in 10s steps.
  # Used when waiting for the recently created proposal.
  wait_proposal_id_tries = 18;
  wait_proposal_id_sleep = 10; # 3 minutes
  # Use to wait for all proposals to be available before we start voting.
  # As nodes will end their splitting phases at different times, this parameters
  # works as a formation lap before race start =).
  wait_proposals_count_tries = 120;
  wait_proposals_count_sleep = 10; # 20 minutes in 10s steps.

  # The most important one. To calculate and achieve a predictable TPS.
  desired_cluster_average_tps = 5.0; # Add decimals!!!!!
  desired_producer_tps = desired_cluster_average_tps / producers_count;
  desired_producer_sleep = 1.0 / desired_producer_tps; # Add decimals!!!!!

in ''

# producers_count: ${toString producers_count}
# proposals_count: ${toString proposals_count}
# dreps_per_producer: ${toString dreps_per_producer}
# desired_cluster_average_tps: ${toString desired_cluster_average_tps}
# desired_producer_tps: ${toString desired_producer_tps}
# desired_producer_sleep: ${toString desired_producer_sleep}

################################################################################
# Returns the node's socket path.
################################################################################
function get_socket_path {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.

  local socket_path="../''${node_str}/node.socket"
  ${coreutils}/bin/echo "''${socket_path}"
}

################################################################################
# Given a tx.signed return TxHash#TxIx of the FIRST occurrence of an address in
# its "outputs". If NO ADDRESS IS SUPPLIED as argument to this function we
# use/assume the last output is the change address (--change-address) and you
# want to use that one to calculate a future expected UTxO.
# Returns UTxO formatted string to use as a "--tx-in" argument.
# Fails if the address in the function argument was not present.
################################################################################
function calculate_next_utxo {

  # Function arguments.
  local tx_signed=$1
  local addr=''${2:-null}

  local tx_id tx_ix
  # Prints a transaction identifier.
  tx_id="$( \
    ${cardano-cli}/bin/cardano-cli conway transaction txid  \
      --tx-file "''${tx_signed}"                            \
  )"
  # Format transaction view output to JSON and get FIRST index of "$addr".
  tx_ix="$(                                                 \
      ${cardano-cli}/bin/cardano-cli debug transaction view \
        --output-json                                       \
        --tx-file "''${tx_signed}"                          \
    |                                                       \
      ${jq}/bin/jq -r                                       \
        --argjson addr "\"''${addr}\""                      \
        '
          if $addr == null
          then
            (.outputs | length - 1)
          else
            (
              .outputs
            | map(.address == $addr)
            | index(true)
            )
          end
        '                                                   \
  )"

  if test "''${tx_ix}" = "null" || test -z "''${tx_ix}"
  then
    # Fail
    ${coreutils}/bin/echo "calculate_next_utxo: ''${tx_signed} - ''${addr}"
    exit 1
  else
    ${coreutils}/bin/echo "''${tx_id}#''${tx_ix}"
  fi
}

################################################################################
# Store the pre-calculated "cached" future UTxO of this address.
# (This only works if an address is always used from the same node/socket/path).
################################################################################
function store_address_utxo_expected {

  # Function arguments.
  local node_str=$1       # node name / folder where to store the files.
  local tx_signed=$2
  local addr=$3

  local utxo_file=../"''${node_str}"/addr."''${addr}".utxo
    calculate_next_utxo                       \
      "''${tx_signed}"                        \
      "''${addr}"                             \
  > "''${utxo_file}"
}

################################################################################
# Get pre-calculated "cached" future UTxO or nothing (an "empty" string).
# (This only works if an address is always used from the same node/socket/path).
################################################################################
function get_address_utxo_expected {

  # Function arguments.
  local node_str=$1       # node name / folder where to store the files.
  local addr=$2

  local utxo_file=../"''${node_str}"/addr."''${addr}".utxo
  if test -f "''${utxo_file}"
  then
    ${coreutils}/bin/cat "''${utxo_file}"
  fi
}

################################################################################
# Evenly split the first UTxO of this key to the addresses in the array!
# Does it in batchs so we don't exceed "maxTxSize" of 16384.
# Stores the future UTxOs of all addresses in files for later references.
# Not to be run during the benchmarking phase: waits for funds between batchs!
################################################################################
function funds_from_to {

  # Function arguments.
  local node_str=''${1};  shift  # node name / folder to find the socket to use.
  local utxo_vkey=''${1}; shift  # In
  local utxo_skey=''${1}; shift  # In
  local reminder=''${1};  shift  # Funds to keep in the origin address.
  local donation=''${1};  shift  # To treasury.
  local addrs_array=("$@")       # Outs

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  # Get the "in" address and its first UTxO only once we have the lock.
  local funds_addr
  funds_addr="$( \
    ${cardano-cli}/bin/cardano-cli address build       \
      --testnet-magic ${toString testnetMagic}         \
      --payment-verification-key-file "''${utxo_vkey}" \
  )"
  # This three only needed for the first batch and to calculate funds per node.
  local funds_json funds_tx funds_lovelace
  funds_json="$( \
    ${cardano-cli}/bin/cardano-cli conway query utxo \
      --testnet-magic ${toString testnetMagic}       \
      --socket-path "''${socket_path}"               \
      --address "''${funds_addr}"                    \
      --output-json                                  \
  )"
  funds_tx="$( \
      ${coreutils}/bin/echo "''${funds_json}" \
    | ${jq}/bin/jq -r                         \
        'keys[0]'                             \
  )"
  funds_lovelace="$( \
      ${coreutils}/bin/echo "''${funds_json}" \
    | ${jq}/bin/jq -r                         \
      --arg keyName "''${funds_tx}"           \
      '.[$keyName].value.lovelace'            \
  )"

  # Calculate how much lovelace for each output address.
  local outs_count per_out_lovelace
  outs_count="''${#addrs_array[@]}"
  ### HACK: Fees! Always using 550000!!!
  ### With   2 outputs: "Estimated transaction fee: 172233 Lovelace"
  ### With  10 outputs: "Estimated transaction fee: 186665 Lovelace"
  ### With  53 outputs: "Estimated transaction fee: 264281 Lovelace"
  ### With 150 outputs: "Estimated transaction fee: 439357 Lovelace"
  ### With 193 outputs: "Estimated transaction fee: 516929 Lovelace"
  per_out_lovelace="$(                                                         \
    ${jq}/bin/jq -r --null-input                                               \
      --argjson numerator    "''${funds_lovelace}"                             \
      --argjson denominator  "''${outs_count}"                                 \
      --argjson reminder     "''${reminder}"                                   \
      --argjson donation     "''${donation}"                                   \
      '(
             ( $numerator
             - $reminder
             - $donation
             - ( 550000
               * ( ($denominator / ${toString outs_per_transaction}) | ceil )
               )
             )
           / $denominator
         | round
       )'                                                                      \
  )"

  # Split the funds in batchs (donations only happen in the first batch).
  local i=0
  local txOuts_args_array=() txOuts_addrs_array=()
  local batch=${toString outs_per_transaction}
  local tx_in tx_filename
  local treasury_donation_args_array=()
  for addr in "''${addrs_array[@]}"
  do
    i="$((i + 1))"
    # Build the "--tx-out" arguments array of this batch.
    txOuts_args_array+=("--tx-out")
    txOuts_args_array+=("''${addr}+''${per_out_lovelace}")
    txOuts_addrs_array+=("''${addr}")

    # We send if last addr in the for loop or batch max exceeded.
    if test "$i" -ge "''${#addrs_array[@]}" || test "$i" -ge "$batch"
    then
      if test "$batch" -eq ${toString outs_per_transaction}
      then
        # First transaction.
        # The input comes from the function arguments.
        tx_in="''${funds_tx}"
        # Treasury donation happens only once.
        if ! test "''${donation}" = "0"
        then
          treasury_donation_args_array=("--treasury-donation" "''${donation}")
        fi
      else
        # Not the first batch.
        # The input comes from the last transaction submitted. Wait for it!!!
        tx_in="$(get_address_utxo_expected "''${node_str}" "''${funds_addr}")"
        ${coreutils}/bin/echo "funds_from_to: Wait for funds:  $(${coreutils}/bin/date --rfc-3339=seconds)"
        wait_utxo_id "''${node_str}" "''${funds_addr}" "''${tx_in}" >/dev/null
        ${coreutils}/bin/echo "funds_from_to: Funds available: $(${coreutils}/bin/date --rfc-3339=seconds)"
        # Treasury donation happens only once.
        treasury_donation_args_array=()
      fi

      # Some debugging!
      ${coreutils}/bin/echo "funds_from_to: ''${utxo_vkey} (''${funds_addr}): --tx-in ''${tx_in}" # Remove from output ''${txOuts_args_array[*]}

      # Send this batch to each node!
      # Build transaction.
      tx_filename=../"''${node_str}"/funds_from_to."''${funds_addr}"."''${i}"
      ${cardano-cli}/bin/cardano-cli conway transaction build               \
        --testnet-magic         ${toString testnetMagic}                    \
        --socket-path           "''${socket_path}"                          \
        --tx-in                 "''${tx_in}"                                \
        ''${txOuts_args_array[@]}                                           \
        ''${treasury_donation_args_array[@]}                                \
        --change-address        "''${funds_addr}"                           \
        --out-file              "''${tx_filename}.raw"
      # Sign transaction.
      ${cardano-cli}/bin/cardano-cli conway transaction sign                \
        --testnet-magic         ${toString testnetMagic}                    \
        --signing-key-file      "''${utxo_skey}"                            \
        --tx-body-file          "''${tx_filename}.raw"                      \
        --out-file              "''${tx_filename}.signed"
      # Submit transaction.
      ${cardano-cli}/bin/cardano-cli conway transaction submit              \
        --testnet-magic         ${toString testnetMagic}                    \
        --socket-path           "''${socket_path}"                          \
        --tx-file               "''${tx_filename}.signed"

      # Store outs/addresses next UTxO, including the change address.
      # Without the change address we can't wait for the funds if a next batch!
      txOuts_addrs_array+=("''${funds_addr}")
      for addr_cache in "''${txOuts_addrs_array[@]}"
      do
        store_address_utxo_expected \
          "''${node_str}"           \
          "''${tx_filename}.signed" \
          "''${addr_cache}"
      done

      # Reset variables for next batch iteration.
      txOuts_args_array=() txOuts_addrs_array=()
      batch="$((batch + ${toString outs_per_transaction}))"
    fi
  done

  # Use the last transaction to store the UTxO
  store_address_utxo_expected \
    "''${node_str}"           \
    "''${tx_filename}.signed" \
    "''${funds_addr}"
}

################################################################################
# Waits until the UTxOs of this address are not empty (errors on timeout).
# Not to be run during the benchmarking phase: lots of queries!
################################################################################
function wait_any_utxo {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket to use.
  local addr=$2

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local tries=${toString wait_any_utxo_tries}
  local utxos_json="{}"
  while test "''${utxos_json}" = "{}"
  do
    if test "''${tries}" -le 0
    then
      # Time's up!
      ${coreutils}/bin/echo "wait_any_utxo: Timeout waiting for: ''${addr}"
      exit 1
    fi
    utxos_json="$( \
      ${cardano-cli}/bin/cardano-cli conway query utxo     \
        --testnet-magic     ${toString testnetMagic}       \
        --socket-path       "''${socket_path}"             \
        --address           "''${addr}"                    \
        --output-json                                      \
    )"
    if ! test "''${tries}" = ${toString wait_any_utxo_tries}
    then
      ${coreutils}/bin/sleep ${toString wait_any_utxo_sleep}
    fi
    tries="$((tries - 1))"
  done
}

################################################################################
# Waits until an specific UTxO of this address appears (errors on timeout).
# Not to be run during the benchmarking phase: lots of queries!
################################################################################
function wait_utxo_id {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket to use.
  local addr=$2
  local utxo_id=$3

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local contains_addr="false"
  local tries=${toString wait_utxo_id_tries}
  while test "''${contains_addr}" = "false"
  do
    if test "''${tries}" -le 0
    then
      # Time's up!
      ${coreutils}/bin/echo "wait_utxo_id: Timeout waiting for: ''${addr} - ''${utxo_id}"
      exit 1
    else
      contains_addr="$(                                      \
          ${cardano-cli}/bin/cardano-cli conway query utxo   \
            --testnet-magic     ${toString testnetMagic}     \
            --socket-path       "''${socket_path}"           \
            --address           "''${addr}"                  \
            --output-json                                    \
        | ${jq}/bin/jq --raw-output                          \
            --argjson utxo_id "\"''${utxo_id}\""             \
            'keys | any(. == $utxo_id) // false'             \
      )"
      if ! test "''${tries}" = ${toString wait_utxo_id_tries}
      then
        ${coreutils}/bin/sleep ${toString wait_utxo_id_sleep}
      fi
      tries="$((tries - 1))"
    fi
  done
}

################################################################################
# Waits until an specific proposal appears or fails.
# Not to be run during the benchmarking phase: lots of queries!
################################################################################
function wait_proposal_id {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket to use.
  local tx_signed=$2

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  # Get proposal's "txId" from the "--tx-file".
  local tx_id
  tx_id="$( \
    ${cardano-cli}/bin/cardano-cli conway transaction txid    \
      --tx-file     "''${tx_signed}"                          \
  )"

  local contains_proposal="false"
  local tries=${toString wait_proposal_id_tries}
  while test "''${contains_proposal}" = "false"
  do
    if test "''${tries}" -le 0
    then
      # Time's up!
      ${coreutils}/bin/echo "wait_proposal_id: Timeout waiting for: ''${tx_id}"
      exit 1
    else
      # No "--output-json" needed.
      contains_proposal="$(                                       \
          ${cardano-cli}/bin/cardano-cli conway query gov-state   \
            --testnet-magic     ${toString testnetMagic}          \
            --socket-path       "''${socket_path}"                \
        | ${jq}/bin/jq --raw-output                               \
            --argjson tx_id "\"''${tx_id}\""                      \
            '.proposals | any(.actionId.txId == $tx_id) // false' \
      )"
      if ! test "''${tries}" = ${toString wait_proposal_id_tries}
      then
        ${coreutils}/bin/sleep ${toString wait_proposal_id_sleep}
      fi
      tries="$((tries - 1))"
    fi
  done
}

################################################################################
# Waits until an specific number of proposals are visible.
# Not to be run during the benchmarking phase: lots of queries!
################################################################################
function wait_proposals_count {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket to use.
  local count=$2

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local contains_proposals="false"
  local tries=${toString wait_proposals_count_tries}
  while test "''${contains_proposals}" = "false"
  do
    if test "''${tries}" -le 0
    then
      # Time's up!
      ${coreutils}/bin/echo "wait_proposals_count: Timeout waiting for: ''${count}"
      exit 1
    else
      # No "--output-json" needed.
      contains_proposals="$(                                      \
          ${cardano-cli}/bin/cardano-cli conway query gov-state   \
            --testnet-magic     ${toString testnetMagic}          \
            --socket-path       "''${socket_path}"                \
        | ${jq}/bin/jq --raw-output                               \
            --argjson count "''${count}"                          \
            '.proposals | length == $count // false'              \
      )"
      if ! test "''${tries}" = ${toString wait_proposals_count_tries}
      then
        ${coreutils}/bin/sleep ${toString wait_proposals_count_sleep}
      fi
      tries="$((tries - 1))"
    fi
  done
}

################################################################################
# Hack: Given a node "i" and proposal number and a DRep number create always the
# same address keys.
# Only supports up to 99 nodes, 9999 proposals and 999999 DReps by adding the
# missing Hex chars.
# Returns the file path without the extensions (the ".skey" or ".vkey" part).
################################################################################
function create_node_prop_drep_key_files {

  # Function arguments.
  local node_str=$1 # String for the key file name (not for the socket).
  local node_i=$2   # This "i" is part of the node name ("node-i").
  local prop_i=$3
  local drep_i=$4

  local filename=../"''${node_str}"-prop-"''${prop_i}"-drep-"''${drep_i}"
  # Now with the extensions.
  local skey="''${filename}".skey
  local vkey="''${filename}".vkey

  # Only create if not already there!
  if ! test -f "''${vkey}"
  then
      ${jq}/bin/jq --null-input \
        --argjson node_i "''${node_i}" \
        --argjson prop_i "''${prop_i}" \
        --argjson drep_i "''${drep_i}" \
        '
          {"type": "PaymentSigningKeyShelley_ed25519",
           "description": "Payment Signing Key",
           "cborHex": (
                "5820b02868d722df021278c78be3b7363759b37f5852b8747b488bab"
              + (if   $node_i <=  9
                 then ("0" + ($node_i | tostring))
                 elif $node_i >= 10 and $node_i <= 99
                 then (       $node_i | tostring)
                 else (error ("Node ID above 99"))
                 end
                )
              + (if   $prop_i <=      9
                 then (   "000" + ($prop_i | tostring))
                 elif $prop_i >=   10 and $prop_i <=   99
                 then (    "00" + ($prop_i | tostring))
                 elif $prop_i >=  100 and $prop_i <=  999
                 then (     "0" + ($prop_i | tostring))
                 elif $prop_i >= 1000 and $prop_i <= 9999
                 then (           ($prop_i | tostring))
                 else (error ("Proposal ID above 9999"))
                 end
                )
              + (if   $drep_i <=      9
                 then ( "00000" + ($drep_i | tostring))
                 elif $drep_i >=     10 and $drep_i <=     99
                 then (  "0000" + ($drep_i | tostring))
                 elif $drep_i >=    100 and $drep_i <=    999
                 then (   "000" + ($drep_i | tostring))
                 elif $drep_i >=   1000 and $drep_i <=   9999
                 then (    "00" + ($drep_i | tostring))
                 elif $drep_i >=  10000 and $drep_i <=  99999
                 then (     "0" + ($drep_i | tostring))
                 elif $drep_i >= 100000 and $drep_i <= 999999
                 then (           ($drep_i | tostring))
                 else (error ("DRep ID above 999999"))
                 end
                )
            )
          }
        ' \
    > "''${skey}"
    ${cardano-cli}/bin/cardano-cli conway key verification-key         \
      --signing-key-file      "''${skey}"                              \
      --verification-key-file "''${vkey}"
  fi
  ${coreutils}/bin/echo "''${filename}"
}

################################################################################
# Get address of the node-proposal-drep combination!
################################################################################
function build_node_prop_drep_address {

  # Function arguments.
  local node_str=$1 # String for the key file name (not for the socket).
  local node_i=$2   # This "i" is part of the node name ("node-i").
  local prop_i=$3
  local drep_i=$4

  local filename addr
  filename="$(create_node_prop_drep_key_files "''${node_str}" "''${node_i}" "''${prop_i}" "''${drep_i}")"
  addr="''${filename}.addr"
  # Only create if not already there!
  if ! test -f "''${addr}"
  then
    local vkey="''${filename}".vkey
      ${cardano-cli}/bin/cardano-cli address build  \
        --testnet-magic ${toString testnetMagic}    \
        --payment-verification-key-file "''${vkey}" \
    > "''${addr}"
  fi
  ${coreutils}/bin/cat "''${addr}"
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
        ../node-specs.json                 \
    )"
    local producer_addr
    # Drep 0 is No DRep (funds for the node).
    producer_addr="$(build_node_prop_drep_address "''${producer_name}" "''${producer_i}" 0 0)"
    producers_addrs_array+=("''${producer_addr}")
    ${coreutils}/bin/echo "governance_funds_genesis: Splitting to: ''${producer_name} - ''${producer_i} - 0 - (''${producer_addr})"
  done

  # Split!
  funds_from_to \
    "''${node_str}"                   \
    "''${utxo_vkey}" "''${utxo_skey}" \
    "''${constitution_reminder}"      \
    ${toString treasury_donation}     \
    "''${producers_addrs_array[@]}"

  # Wait for the funds of the last producer to arrive.
  wait_any_utxo "''${node_str}" "''${producers_addrs_array[-1]}"
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
      ../node-specs.json                 \
  )"
  local producer_addr producer_vkey producer_skey
  producer_addr="$(build_node_prop_drep_address    "''${producer_name}" "''${producer_i}" 0 0)"
  producer_vkey="$(create_node_prop_drep_key_files "''${producer_name}" "''${producer_i}" 0 0)".vkey
  producer_skey="$(create_node_prop_drep_key_files "''${producer_name}" "''${producer_i}" 0 0)".skey

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
    producer_prop_addr="$(build_node_prop_drep_address "''${producer_name}" "''${producer_i}" "''${prop_i}" 0)"
    producer_prop_addr_array+=("''${producer_prop_addr}")
    ${coreutils}/bin/echo  "governance_funds_producer: Splitting to: ''${producer_name} - ''${producer_i} - ''${prop_i} - ''${producer_prop_addr}"
  done

  # Split!
  funds_from_to \
    "''${node_str}"                           \
    "''${producer_vkey}" "''${producer_skey}" \
    "''${proposals_reminder}"                 \
    0                                         \
    "''${producer_prop_addr_array[@]}"

  # Wait for the funds of the last producer-drep to arrive.
  wait_any_utxo "''${node_str}" "''${producer_prop_addr_array[-1]}"

  ############################
  # Proposals -> DReps split #
  ############################

  local dreps_reminder
  # HACK: Plus "Minimum UTxO threshold: 105986 Lovelace"
  dreps_reminder="$(( 200000 ))"

  for prop_i in {1..${toString proposals_count}}
  do

    local producer_prop_vkey producer_prop_skey
    producer_prop_vkey="$(create_node_prop_drep_key_files "''${producer_name}" "''${producer_i}" "''${prop_i}" 0)".vkey
    producer_prop_skey="$(create_node_prop_drep_key_files "''${producer_name}" "''${producer_i}" "''${prop_i}" 0)".skey

    local producer_dreps_addrs_array=()
    local drep_step=0
    drep_step="$((producer_i * ${toString dreps_per_producer}))"
    local actual_drep
    for i in {1..${toString dreps_per_producer}}
    do
      local producer_drep_addr
      actual_drep="$((drep_step + i))"
      producer_drep_addr="$(build_node_prop_drep_address "''${producer_name}" "''${producer_i}" "''${prop_i}" "''${actual_drep}")"
      producer_dreps_addrs_array+=("''${producer_drep_addr}")
      ${coreutils}/bin/echo  "governance_funds_producer: Splitting to: ''${producer_name} - ''${producer_i} - ''${prop_i} - ''${actual_drep} - ''${producer_drep_addr}"
    done
  
    # Split!
    funds_from_to \
      "''${node_str}"                                     \
      "''${producer_prop_vkey}" "''${producer_prop_skey}" \
      "''${dreps_reminder}"                               \
      0                                                   \
      "''${producer_dreps_addrs_array[@]}"
  
    # Wait for the funds of the last producer-prop-drep to arrive.
    wait_any_utxo "''${node_str}" "''${producer_dreps_addrs_array[-1]}"
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
      --testnet-magic ${toString testnetMagic}    \
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
  funds_tx="$(get_address_utxo_expected "''${node_str}" "''${node_addr}")"

  # Show current gov-state.
    ${cardano-cli}/bin/cardano-cli conway query gov-state            \
      --testnet-magic ${toString testnetMagic}                       \
      --socket-path "''${socket_path}"                               \
  | ${jq}/bin/jq -r                                                  \
      '.nextRatifyState.nextEnactState.prevGovActionIds'

  # Create dummy constitution.
    ${coreutils}/bin/echo "My Constitution: free mate and asado"     \
  > ../"''${node_str}"/constitution.txt
  # Calculate constitution hash.
  ${cardano-cli}/bin/cardano-cli hash anchor-data                    \
    --file-text ../"''${node_str}"/constitution.txt                  \
    --out-file  ../"''${node_str}"/constitution.hash
  # Copy guardrails-script.
  ${coreutils}/bin/cp                                                \
    ../genesis/guardrails-script.plutus                              \
    ../"''${node_str}"/guardrails-script.plutus
  # Calculate guardrails-script hash.
  ${cardano-cli}/bin/cardano-cli hash script                         \
    --script-file ../"''${node_str}"/guardrails-script.plutus        \
    --out-file    ../"''${node_str}"/guardrails-script.hash

  # Create action.
  local tx_filename=../"''${node_str}"/create-constitution
  ${cardano-cli}/bin/cardano-cli conway governance action create-constitution \
    --testnet \
    --anchor-url "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/cip-0100.common.schema.json" \
    --anchor-data-hash "9d99fbca260b2d77e6d3012204e1a8658f872637ae94cdb1d8a53f4369400aa9" \
    --constitution-url "https://ipfs.io/ipfs/Qmdo2J5vkGKVu2ur43PuTrM7FdaeyfeFav8fhovT6C2tto" \
    --constitution-hash        "$(${coreutils}/bin/cat ../"''${node_str}"/constitution.hash)" \
    --constitution-script-hash "$(${coreutils}/bin/cat ../"''${node_str}"/guardrails-script.hash)" \
    --governance-action-deposit "''${action_deposit}" \
    --deposit-return-stake-verification-key-file ../genesis/cache-entry/stake-delegators/delegator0/staking.vkey \
    --out-file "''${tx_filename}".action
  # Build transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction build            \
    --testnet-magic ${toString testnetMagic}                         \
    --socket-path "''${socket_path}"                                 \
    --tx-in "''${funds_tx}"                                          \
    --change-address "''${node_addr}"                                \
    --proposal-file "''${tx_filename}".action                        \
    --out-file      "''${tx_filename}".raw                           \
  > /dev/null
  # Sign transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction sign             \
    --testnet-magic ${toString testnetMagic}                         \
    --signing-key-file "''${utxo_skey}"                              \
    --tx-body-file "''${tx_filename}".raw                            \
    --out-file     "''${tx_filename}".signed
  # Submit transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction submit           \
    --testnet-magic ${toString testnetMagic}                         \
    --socket-path "''${socket_path}"                                 \
    --tx-file     "''${tx_filename}".signed                          \
  > /dev/null

  # Wait for the proposal without releasing the local socket.
  wait_proposal_id "''${node_str}" "''${tx_filename}".signed >/dev/null

  store_address_utxo_expected \
    "''${node_str}"           \
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
  node_drep_skey="$(create_node_prop_drep_key_files "''${node_str}" "''${node_i}" 0 "''${drep_i}")".skey
  node_drep_addr="$(build_node_prop_drep_address    "''${node_str}" "''${node_i}" 0 "''${drep_i}")"

  # Funds needed for this governance action ?
  local action_deposit
  action_deposit="${toString gov_action_deposit}"
  # Funds address.
  # The input is calculated from the last transaction submitted.
  # No waiting! But, if last submitted transaction fails (function
  # `governance_funds_producer` current workflow), everything else fails.
  local funds_tx
  funds_tx="$(get_address_utxo_expected "''${node_str}" "''${node_drep_addr}")"

  local tx_filename=../"''${node_str}"/create-withdrawal."''${node_str}"."''${drep_i}"
  # Create action.
  ${cardano-cli}/bin/cardano-cli conway governance action create-treasury-withdrawal \
    --testnet                                                                                                                    \
    --anchor-url "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0108/examples/treasury-withdrawal.jsonld" \
    --anchor-data-hash "311b148ca792007a3b1fee75a8698165911e306c3bc2afef6cf0145ecc7d03d4"                                        \
    --governance-action-deposit "''${action_deposit}"                                                                            \
    --transfer 50                                                                                                                \
    --deposit-return-stake-verification-key-file  ../genesis/cache-entry/stake-delegators/"delegator''${node_i}"/staking.vkey    \
    --funds-receiving-stake-verification-key-file ../genesis/cache-entry/stake-delegators/"delegator''${node_i}"/staking.vkey    \
    --out-file "''${tx_filename}".action
  # Build transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction build            \
    --testnet-magic ${toString testnetMagic}                         \
    --socket-path "''${socket_path}"                                 \
    --tx-in "''${funds_tx}"                                          \
    --change-address "''${node_drep_addr}"                           \
    --proposal-file "''${tx_filename}".action                        \
    --out-file      "''${tx_filename}".raw                           \
  > /dev/null
  # Sign transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction sign             \
    --testnet-magic ${toString testnetMagic}                         \
    --signing-key-file "''${node_drep_skey}"                         \
    --tx-body-file "''${tx_filename}".raw                            \
    --out-file     "''${tx_filename}".signed
  # Submit transaction.
  ${cardano-cli}/bin/cardano-cli conway transaction submit           \
    --testnet-magic ${toString testnetMagic}                         \
    --socket-path "''${socket_path}"                                 \
    --tx-file     "''${tx_filename}".signed                          \
  > /dev/null

  # Wait for the proposal without releasing the local socket.
  wait_proposal_id "''${node_str}" "''${tx_filename}".signed >/dev/null

  store_address_utxo_expected \
    "''${node_str}"           \
    "''${tx_filename}.signed" \
    "''${node_drep_addr}"
}

################################################################################
# Sleeps the node between votes submissions to achieve the desired TPS.
################################################################################

function vote_tps_throttle() {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local votes_count=$2    # Actual number of total votes already submitted.

  local filepath_first=../"''${node_str}"/first_vote_time
  local filepath_last=../"''${node_str}"/last_vote_time
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
        --argjson votes_count  "''${votes_count}"      \
        '
          if $votes_count < 3
          then 0
          else (
              ($current_time - $start_time  )    as $cluster_seconds
            | ($votes_count  / $cluster_seconds) as $current_tps
            | if $current_tps < ${toString desired_producer_tps}
              then 0
              else (
                  (($votes_count + 1) / ${toString desired_producer_tps}) as $next_cluster_seconds
                | ($start_time + $next_cluster_seconds - $current_time)
              )
              end
          )
          end
        '                                              \
    )"
    ${coreutils}/bin/echo "vote_tps_throttle: ''${node_str}: ''${sleep_arg_number}"
    ${coreutils}/bin/sleep "''${sleep_arg_number}"
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
        --testnet-magic ${toString testnetMagic}                     \
        --socket-path "''${socket_path}"                             \
    | ${jq}/bin/jq '.proposals | map(.actionId.txId)'                \
  )"
  proposal_tx_ids_array=$(echo "''${txIdsJSON}" | ${jq}/bin/jq --raw-output '. | join (" ")')

  # Cycle proposals.
  local prop_i=0
  local votes_count=0 # Keep a counter for TPS calculation.
  for proposal_tx_id in ''${proposal_tx_ids_array[*]}
  do
    prop_i=$(( prop_i + 1 )) # Prop key to use.
    governance_vote_proposal "''${node_str}" "''${node_i}" "''${prop_i}" "''${proposal_tx_id}" "''${votes_count}"
    local proposal_flag="../''${node_str}/proposal.''${proposal_tx_id}.voted"
    ${coreutils}/bin/touch "''${proposal_flag}"
    # Separate counter, which proposal in which epoch, etc will change!
    votes_count=$(( votes_count + ${toString dreps_per_producer} ))
  done
}

################################################################################
# Benchmarking phase function!
# The node votes the proposal with all the DReps it controls.
################################################################################
function governance_vote_proposal {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.
  local node_i=$2         # This "i" is part of the node name ("node-i").
  local prop_i=$3         # Proposal key/address to use.
  local proposal_tx_id=$4
  local votes_count=$5    # Actual number of total votes already submitted.

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local drep_step actual_drep
  drep_step=$((node_i * ${toString dreps_per_producer}))
  for i in {1..${toString dreps_per_producer}}   # for drepDir in ../genesis/cache-entry/drep-keys/drep*
  do
    actual_drep="$((drep_step + i))"

    ${coreutils}/bin/echo "governance_vote_proposal: ''${proposal_tx_id} - ''${node_str} - ''${actual_drep}"

    local node_drep_skey node_drep_addr
    node_drep_skey="$(create_node_prop_drep_key_files "''${node_str}" "''${node_i}" "''${prop_i}" "''${actual_drep}")".skey
    node_drep_addr="$(build_node_prop_drep_address    "''${node_str}" "''${node_i}" "''${prop_i}" "''${actual_drep}")"

    # Funds address.
    # The input is calculated from the last transaction submitted.
    # No waiting! But, if last submitted transaction fails (function
    # `governance_funds_producer` or `governance_vote_proposal` in current
    # workflow), everything else fails.
    local funds_tx
    funds_tx="$(get_address_utxo_expected "''${node_str}" "''${node_drep_addr}")"
    # A next UTxO must be cached by `funds_from_to` when splitting the funds,
    # we don't check the response to be sure there is an expected UTxO to be
    # really sure we are not querying the node unnecessarily.

    vote_tps_throttle "''${node_str}" "''${votes_count}"
    votes_count=$(( votes_count + 1 ))

    # Voting with DRep keys:
    local tx_filename=../"''${node_str}"/proposal."''${proposal_tx_id}"."''${actual_drep}"
    ${cardano-cli}/bin/cardano-cli conway governance vote create   \
      --yes                                                        \
      --governance-action-tx-id "''${proposal_tx_id}"              \
      --governance-action-index "0"                                \
      --drep-verification-key-file ../genesis/cache-entry/drep-keys/drep"''${actual_drep}"/drep.vkey \
      --out-file "''${tx_filename}".action
    # Build the transaction.
    ${cardano-cli}/bin/cardano-cli conway transaction build        \
      --testnet-magic ${toString testnetMagic}                     \
      --socket-path "''${socket_path}"                             \
      --tx-in "''${funds_tx}"                                      \
      --change-address "''${node_drep_addr}"                       \
      --witness-override 2                                         \
      --vote-file "''${tx_filename}".action                        \
      --out-file "''${tx_filename}".raw                            \
    > /dev/null
    # Sign it with the DRep key:
    ${cardano-cli}/bin/cardano-cli conway transaction sign         \
      --testnet-magic ${toString testnetMagic}                     \
      --signing-key-file ../genesis/cache-entry/drep-keys/drep"''${actual_drep}"/drep.skey \
      --signing-key-file "''${node_drep_skey}"                     \
      --tx-body-file "''${tx_filename}".raw                        \
      --out-file "''${tx_filename}".signed
    # Submit the transaction:
    ${cardano-cli}/bin/cardano-cli conway transaction submit       \
      --testnet-magic ${toString testnetMagic}                     \
      --socket-path "''${socket_path}"                             \
      --tx-file "''${tx_filename}".signed                          \
    >/dev/null

    ${coreutils}/bin/touch "''${tx_filename}".voted

    # No `store_address_utxo_expected`, all UTxOs are created before voting!

  done
}

################################################################################
# Entrypoints.
################################################################################

function workflow_generator {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.

  #- Splinting 0 --------------------------------------------------------------#
  ${coreutils}/bin/echo "governance_funds_genesis:        Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_funds_genesis       \
    "''${node_str}"                \
    ${toString genesis_funds_vkey} \
    ${toString genesis_funds_skey}
  ${coreutils}/bin/echo "governance_funds_genesis:        End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #- Preparing ----------------------------------------------------------------#
  ${coreutils}/bin/echo "governance_create_constitution:  Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_create_constitution   \
    "''${node_str}"                \
    ${toString genesis_funds_vkey} \
    ${toString genesis_funds_skey}
  ${coreutils}/bin/echo "governance_create_constitution:  End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #----------------------------------------------------------------------------#

}

function workflow_producer {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket to use.

  local producer_i
  producer_i="$( \
    ${jq}/bin/jq --raw-output            \
      --arg keyName "''${node_str}"      \
      '.[$keyName].i'                    \
      ../node-specs.json                 \
  )"

  #- Splinting 1 --------------------------------------------------------------#
  ${coreutils}/bin/echo "governance_funds_producer:       Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_funds_producer "''${node_str}" "''${node_str}"
  ${coreutils}/bin/echo "governance_funds_producer:       End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  #- Preparing ----------------------------------------------------------------#
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
      --testnet-magic ${toString testnetMagic}                       \
      --socket-path "''${socket_path}"                               \
  > "../''${node_str}/gov-state.start.json"
  ${coreutils}/bin/echo "governance_vote_all:             Start: $(${coreutils}/bin/date --rfc-3339=seconds)"
  governance_vote_all "''${node_str}" "''${producer_i}"
  ${coreutils}/bin/echo "governance_vote_all:             End:   $(${coreutils}/bin/date --rfc-3339=seconds)"
  socket_path="$(get_socket_path "''${node_str}")"
  # Store actual gov-state
    ${cardano-cli}/bin/cardano-cli conway query gov-state            \
      --testnet-magic ${toString testnetMagic}                       \
      --socket-path "''${socket_path}"                               \
  > "../''${node_str}/gov-state.end.json"
  #----------------------------------------------------------------------------#

}

''
