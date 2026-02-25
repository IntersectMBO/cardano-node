{ coreutils
, cardano-cli
, jq
, testnet_magic
# Max number of '--tx-out' when splitting funds.
, outs_per_split_transaction ? 100
# Submit the transaction up to this times on timeout.
, funds_submit_tries ? 3
# Sleeps.
# Used when splitting funds to wait for funds to arrive, as this initial funds
# are sent from a different process (not genesis) this works as a semaphore!
, wait_any_utxo_tries ? 30
, wait_any_utxo_sleep ? 10 # 5 minutes in 10s steps.
# Used when splitting funds, it waits for the expected UTxO to arrive to the
# change-address and re-submits the transaction if necessary!
, wait_utxo_id_tries ? 18
, wait_utxo_id_sleep ? 10 # 3 minutes in 10s steps.
# Used when waiting for the recently created proposal.
, wait_proposal_id_tries ? 30
, wait_proposal_id_sleep ? 10 # 5 minutes
# Use to wait for all proposals to be available before we start voting.
# As nodes will end their splitting phases at different times, this parameters
# work as a formation lap before race start =).
# No tries, waits forever!
, wait_proposals_count_sleep ? 10
}:
''
################################################################################
# Give a node name ("node-0", "explorer", etc) returns the node's socket path.
################################################################################
function get_socket_path {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.

  local socket_path="../../''${node_str}/node.socket"
  ${coreutils}/bin/echo "''${socket_path}"
}

################################################################################
# Given a "tx.signed" returns a JSON object that has as "tx_id" and "tx_ix"
# properties the TxHash#TxIx of the FIRST occurrence of the provided address in
# its "outputs" and in the "value" property the lovelace it will contain.
# For example: {"tx_id":"0000000000", "tx_ix": 0, "value":123456}.
# If NO ADDRESS IS SUPPLIED as argument to this function we use/assume the last
# output is the change address (--change-address) and you want to use that one
# to calculate a future expected UTxO.
################################################################################
function calculate_next_utxo {

  # Function arguments.
  local tx_signed=$1
  local addr=''${2:-null}

  local tx_id
  # Prints a transaction identifier.
  tx_id="$( \
    ${cardano-cli}/bin/cardano-cli conway transaction txid  \
      --tx-file "''${tx_signed}"                            \
      --output-text                                         \
  )"
  # View transaction as JSON and get index of FIRST output containing "$addr".
    ${cardano-cli}/bin/cardano-cli debug transaction view \
      --output-json                                       \
      --tx-file "''${tx_signed}"                          \
  | ${jq}/bin/jq --raw-output                             \
      --argjson tx_id "\"''${tx_id}\""                    \
      --argjson addr  "\"''${addr}\""                     \
      '
          (
            if $addr == null or $addr == "null"
            then
              (.outputs | length - 1)
            else
              (
                .outputs
              | map(.address == $addr)
              | index(true)
              )
            end
          ) as $tx_ix
        | { "tx_id": $tx_id
          , "tx_ix": $tx_ix
          , "value": ( .outputs[$tx_ix].amount.lovelace )
          }
      '
}

################################################################################
# Store the pre-calculated "cached" future UTxO of this address.
# (Only useful if an address is always used from the same node/socket/path).
################################################################################
function store_address_utxo_expected {

  # Function arguments.
  local tx_signed=$1
  local addr=$2

  local utxo_file=./addr."''${addr}".json      # Store in workload's directory!
    calculate_next_utxo                        \
      "''${tx_signed}"                         \
      "''${addr}"                              \
  > "''${utxo_file}"
}

################################################################################
# Get pre-calculated "cached" future UTxO TxHash#TxIx suitable to use as a part
# of a "--tx-in" argument. Returns an "empty" string if not available.
# (Only useful if an address is always used from the same node/socket/path).
################################################################################
function get_address_utxo_expected_id {

  # Function arguments.
  local addr=$1

  local utxo_file=./addr."''${addr}".json      # Store in workload's directory!
  if test -f "''${utxo_file}"
  then
    ${jq}/bin/jq --raw-output                  \
      '( .tx_id + "#" + (.tx_ix | tostring) )' \
      "''${utxo_file}"
  fi
}

################################################################################
# Get pre-calculated "cached" future UTxO lovelace amount suitable to use as
# part of a "--tx-in" argument. Returns an "empty" string if not available.
# (This only works if an address is always used from the same node/socket/path).
################################################################################
function get_address_utxo_expected_value {

  # Function arguments.
  local addr=$1

  local utxo_file=./addr."''${addr}".json      # Store in workload's directory!
  if test -f "''${utxo_file}"
  then
    ${jq}/bin/jq --raw-output '.value' "''${utxo_file}"
  fi
}

################################################################################
# Give a "tx.signed" filepath returns "true" or "false".
# Not to be run during the benchmarking phase: lots of queries!
################################################################################
function is_tx_in_mempool {

  # Function arguments.
  local node_str=$1       # node name / folder where to store the files.
  local tx_signed=$2

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local tx_id
  tx_id="$( \
    ${cardano-cli}/bin/cardano-cli conway transaction txid  \
      --tx-file "''${tx_signed}"                            \
      --output-text                                         \
  )"
    ${cardano-cli}/bin/cardano-cli conway query tx-mempool           \
        tx-exists         "''${tx_id}"                               \
      --testnet-magic     ${toString testnet_magic}                  \
      --socket-path       "''${socket_path}"                         \
  | ${jq}/bin/jq --raw-output                                        \
      .exists
}

################################################################################
# Function to submit the funds-splitting tx and retry if needed.
# Not to be run during the benchmarking phase: lots of queries!
################################################################################
function funds_submit_retry {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket to use.
  local tx_signed=$2        # tx to send and maybe re-send.
  local addr=$3             # Address to wait for (UTxO id must be cached).

  # Only defined in functions that use it.
  local socket_path
  socket_path="$(get_socket_path "''${node_str}")"

  local utxo_id
  utxo_id="$(get_address_utxo_expected_id "''${addr}")"

  local contains_addr="false"
  local submit_tries=${toString funds_submit_tries}
  while test ! "''${contains_addr}" = "true"
  do
    if test "''${submit_tries}" -le 0
    then
      # Time's up!
      ${coreutils}/bin/echo "funds_submit_retry: Timeout waiting for: ''${addr} - ''${utxo_id}"
      exit 1
    else

      # Some debugging.
      ${coreutils}/bin/echo "funds_submit_retry: submit: ''${tx_signed} (''${submit_tries})"

      # (Re)Submit transaction ignoring errors.
        ${cardano-cli}/bin/cardano-cli conway transaction submit              \
          --testnet-magic         ${toString testnet_magic}                   \
          --socket-path           "''${socket_path}"                          \
          --tx-file               "''${tx_signed}"                            \
      || true
      submit_tries="$((submit_tries - 1))"

      # Wait for the transaction to NOT be in the mempool anymore
      local in_mempool="true"
      while test ! "''${in_mempool}" = "false"
      do
        ${coreutils}/bin/sleep 1
        in_mempool="$(is_tx_in_mempool "''${node_str}" "''${tx_signed}")"
      done

      # Some loops to see if the expected UTxO of this address appears.
      local utxo_tries=${toString wait_utxo_id_tries}
      while test ! "''${contains_addr}" = "true" && test "''${utxo_tries}" -gt 0
      do
        ${coreutils}/bin/sleep ${toString wait_utxo_id_sleep}
        # Some debugging.
        ${coreutils}/bin/echo "funds_submit_retry: wait_utxo_id: ''${utxo_id} (''${utxo_tries})"
        contains_addr="$(                                      \
            ${cardano-cli}/bin/cardano-cli conway query utxo   \
              --testnet-magic     ${toString testnet_magic}    \
              --socket-path       "''${socket_path}"           \
              --address           "''${addr}"                  \
              --output-json                                    \
          | ${jq}/bin/jq --raw-output                          \
              --argjson utxo_id "\"''${utxo_id}\""             \
              'keys | any(. == $utxo_id) // false'             \
        )"
        utxo_tries="$((utxo_tries - 1))"
      done

    fi
  done

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
      --testnet-magic ${toString testnet_magic}        \
      --payment-verification-key-file "''${utxo_vkey}" \
  )"
  # This three only needed for the first batch and to calculate funds per node.
  local funds_json funds_tx funds_lovelace
  funds_json="$( \
    ${cardano-cli}/bin/cardano-cli conway query utxo \
      --testnet-magic ${toString testnet_magic}      \
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
               * ( ($denominator / ${toString outs_per_split_transaction}) | ceil )
               )
             )
           / $denominator
         | round
       )'                                                                      \
  )"

  # Split the funds in batchs (donations only happen in the first batch).
  local i=0
  local txOuts_args_array=() txOuts_addrs_array=()
  local batch=${toString outs_per_split_transaction}
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
      if test "$batch" -eq ${toString outs_per_split_transaction}
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
        # The input comes from the last transaction submitted.
        # No need to wait for it because the submission function does this!
        tx_in="$(get_address_utxo_expected_id "''${funds_addr}")"
        # Treasury donation happens only once.
        treasury_donation_args_array=()
      fi

      # Some debugging!
      ${coreutils}/bin/echo "funds_from_to: ''${utxo_vkey} (''${funds_addr}): --tx-in ''${tx_in}"

      # Send this batch to each node!
      # Build transaction.
      tx_filename=./funds_from_to."''${funds_addr}"."''${i}"
      ${cardano-cli}/bin/cardano-cli conway transaction build               \
        --testnet-magic         ${toString testnet_magic}                   \
        --socket-path           "''${socket_path}"                          \
        --tx-in                 "''${tx_in}"                                \
        ''${txOuts_args_array[@]}                                           \
        ''${treasury_donation_args_array[@]}                                \
        --change-address        "''${funds_addr}"                           \
        --out-file              "''${tx_filename}.raw"
      # Sign transaction.
      ${cardano-cli}/bin/cardano-cli conway transaction sign                \
        --testnet-magic         ${toString testnet_magic}                   \
        --signing-key-file      "''${utxo_skey}"                            \
        --tx-body-file          "''${tx_filename}.raw"                      \
        --out-file              "''${tx_filename}.signed"

      # Store outs/addresses next UTxO.
      for addr_cache in "''${txOuts_addrs_array[@]}"
      do
        store_address_utxo_expected \
          "''${tx_filename}.signed" \
          "''${addr_cache}"
      done
      # Without the change address we can't wait for the funds after submission
      # or calculate the next input to use if an extra batch is needed!
      store_address_utxo_expected \
        "''${tx_filename}.signed" \
        "''${funds_addr}"

      # Submit transaction and wait for settlement.
      funds_submit_retry            \
        "''${node_str}"             \
        "''${tx_filename}.signed"   \
        "''${funds_addr}"

      # Reset variables for next batch iteration.
      txOuts_args_array=() txOuts_addrs_array=()
      batch="$((batch + ${toString outs_per_split_transaction}))"
    fi
  done
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
        --testnet-magic     ${toString testnet_magic}      \
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
    ${cardano-cli}/bin/cardano-cli conway transaction txid \
      --tx-file     "''${tx_signed}"                       \
      --output-text                                        \
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
            --testnet-magic     ${toString testnet_magic}         \
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
  while test "''${contains_proposals}" = "false"
  do
    # No "--output-json" needed.
    contains_proposals="$(                                      \
        ${cardano-cli}/bin/cardano-cli conway query gov-state   \
          --testnet-magic     ${toString testnet_magic}         \
          --socket-path       "''${socket_path}"                \
      | ${jq}/bin/jq --raw-output                               \
          --argjson count "''${count}"                          \
          '.proposals | length == $count // false'              \
    )"
    ${coreutils}/bin/sleep ${toString wait_proposals_count_sleep}
  done
}
''
