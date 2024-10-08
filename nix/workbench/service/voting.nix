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
  # Where to obtain the genesis funds from.
  genesis_funds_vkey = "../genesis/cache-entry/utxo-keys/utxo2.vkey";
  genesis_funds_skey = "../genesis/cache-entry/utxo-keys/utxo2.skey";
  # How many constitutions to create with the genesis funds.
  constitutions_from_genesis = 1;
  # Initial donation from genesis funds to make "valid" withdrawal proposals.
  treasury_donation = 500000;
  # Construct an "array" with node producers to use in BASH `for` loops.
  producers_array =
      "("
    + (builtins.concatStringsSep
        " "
        (builtins.map
          (x: "\"" + x.name + "\"")
          (builtins.filter
            (nodeSpec: nodeSpec.isProducer)
            (pkgs.lib.mapAttrsToList
              (nodeName: nodeSpec: {inherit (nodeSpec) name isProducer;})
              nodeSpecs
            )
          )
        )
      )
    + ")"
  ;
  # When splitting the genesis funds, we first move to a "node address" (called
  # DRep 0) for each producer, and then to a "node-drep address" for each
  # node-drep combination.
  dreps_per_producer = builtins.floor (
      profile.genesis.dreps
    / (builtins.length
        (builtins.filter
          (nodeSpec: nodeSpec.isProducer)
          (builtins.attrValues nodeSpecs)
        )
      )
  );
  # Max number of '--tx-out' when splitting funds.
  # DUCT TAPE: Split 10000 DReps to 52 nodes in 1 TX per node.
  #            Signed tx file is 16337 bytes of a tx maximum of 16384 bytes!!!
  outs_per_transaction = 193;
  # To calculate how much funds to leave on nodes' addresses (DRep 0) for the
  # node to create withdrawal proposals (`--governance-action-deposit` arg).
  withdrawal_proposals_per_node = 2;
  # Sleeps.
  wait_any_utxo_tries = 12;
  wait_any_utxo_sleep =  10; # 2 minutes in 10s steps.
  wait_utxo_id_tries = 24;
  wait_utxo_id_sleep = 10; # 4 minutes in 10s steps.
  wait_proposal_id_tries = 12;
  wait_proposal_id_sleep = 10; # 2 minutes

in ''

################################################################################
# Given a tx.signed return TxHash#TxIx of the FIRST occurrence of an address in
# its "outputs". If NO ADDRESS IS SUPPLIED as argument to this function we
# use/assume the last output is the change address (--change-address) and you
# want to use that one to calculate a future expected UTxO.
# Returns UTxO formatted to use in "--tx-in" arguments.
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

  if test "''${tx_ix}" = "null"
  then
    # Fail
    ${coreutils}/bin/echo "calculate_next_utxo: ''${tx_signed} - ''${addr}"
    exit 1
  else
    ${coreutils}/bin/echo "''${tx_id}#''${tx_ix}"
  fi
}

################################################################################
# Get the first UTxO available (Must be non-empty or it fails!)
################################################################################
function get_address_utxo_0 {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local addr=$2

  # Only defined in functions that use it.
  local socket_path="../''${node_str}/node.socket"

    ${cardano-cli}/bin/cardano-cli query utxo       \
      --testnet-magic ${toString testnetMagic}      \
      --socket-path "''${socket_path}"              \
      --address "''${addr}"                         \
      --output-json                                 \
  | ${jq}/bin/jq -r 'keys[0]'
}

################################################################################
# Get pre-calculated "cached" future UTxO or nothing (an "empty" string).
################################################################################
function get_address_utxo_expected {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local addr=$2

  local utxo_file=../"''${node_str}"/"''${addr}".utxo
  if test -f "''${utxo_file}"
  then
    ${coreutils}/bin/cat "''${utxo_file}"
  fi
}

################################################################################
# Must be called by UTxO consumers.
# Returns the node's socket path and creates a lock.
################################################################################
function get_socket_lock {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.

  local socket_path="../''${node_str}/node.socket"
  local lockfile_path="''${socket_path}".lock

  exec 200>"''${lockfile_path}"
  flock 200
  ${coreutils}/bin/echo "''${socket_path}"
}

################################################################################
# Release the socket and cache the next, expected, UTxO.
################################################################################
function release_socket_lock {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local tx_signed=$2
  local addr=$3           # Mandatory argument to store the tx_id in a file!

  local socket_path="../''${node_str}/node.socket"
  local lockfile_path="''${socket_path}".lock

  # Store address next UTxO
    calculate_next_utxo                      \
      "''${tx_signed}"                       \
      "''${addr}"                            \
  > ../"''${node_str}"/"''${addr}".utxo

  # A mystery!
  flock -u 200 2>/dev/null || true
  exec 200>&-
}

################################################################################
# Evenly split the first UTxO of this key to the addresses in the array!
# Does it in batchs so we don't exceed "maxTxSize" of 16384.
# Stores the future UTxO in a file for later references.
# Not to be run during the benchmarking phase: waits for funds between batchs!
################################################################################
function funds_from_to {

  # Function arguments.
  local node_str=''${1};  shift       # node name / folder to find the socket.
  local utxo_vkey=''${1}; shift       # In
  local utxo_skey=''${1}; shift       # In
  local reminder=''${1};  shift       # Funds to keep in the origin address.
  local donation=''${1};  shift       # To treasury.
  local addrs_array=("$@")            # Outs

  # Only defined in functions that use it.
  local socket_path
  # Lock needed. Creates or destroys UTxOs.
  socket_path="$(get_socket_lock "''${node_str}")"

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
    ${cardano-cli}/bin/cardano-cli query utxo      \
      --testnet-magic ${toString testnetMagic}     \
      --socket-path "''${socket_path}"             \
      --address "''${funds_addr}"                  \
      --output-json                                \
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
  ### HACK: Fees! Always using 300000!!!
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
             - (500000 * $denominator / ${toString outs_per_transaction} | ceil)
             )
           / $denominator
         | round
       )'                                                                      \
  )"

  # Split the funds in batchs (donations only happen in the first batch).
  local i=0
  local txOuts_array=()
  local batch=${toString outs_per_transaction}
  local tx_in treasury_donation
  for addr in "''${addrs_array[@]}"
  do
    i="$((i + 1))"
    # Build the "--tx-out" arguments array of this batch.
    txOuts_array+=("--tx-out")
    txOuts_array+=("''${addr}+''${per_out_lovelace}")
    # We send if last addr in the for loop or batch max exceeded.
    if test "$i" -ge "''${#addrs_array[@]}" || test "$i" -ge "$batch"
    then
      if test "$batch" -eq ${toString outs_per_transaction}
      then
        # First transaction.
        # The input comes from the function arguments.
        tx_in="''${funds_tx}"
        # Treasury donation happens only once.
        treasury_donation="''${donation}"
      else
        # Not the first batch.
        # The input comes from the last transaction submitted. Wait for it!!!
        tx_in="$(get_address_utxo_expected "''${node_str}" "''${funds_addr}")"
        ${coreutils}/bin/echo "funds_from_to: Wait for funds:  $(${coreutils}/bin/date --rfc-3339=seconds)"
        wait_utxo_id "''${node_str}" "''${funds_addr}" "''${tx_in}"
        ${coreutils}/bin/echo "funds_from_to: Funds available: $(${coreutils}/bin/date --rfc-3339=seconds)"
        # Treasury donation happens only once.
        treasury_donation=0
      fi
      # Some debugging!
      ${coreutils}/bin/echo "funds_from_to: ''${utxo_vkey} (''${funds_addr}): --tx-in ''${tx_in} ''${txOuts_array[*]}"
      # Send this batch to each node!
      # Build transaction.
      local tx_filename=../"''${node_str}"/tx."''${tx_in}"
      ${cardano-cli}/bin/cardano-cli conway transaction build               \
        --testnet-magic         ${toString testnetMagic}                    \
        --socket-path           "''${socket_path}"                          \
        --tx-in                 "''${tx_in}"                                \
        ''${txOuts_array[@]}                                                \
        --treasury-donation     "''${treasury_donation}"                    \
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
      # Store address next UTxO.
      # Without this we can't properly wait for the funds!
        calculate_next_utxo                       \
          "''${tx_filename}.signed"               \
          "''${funds_addr}"                       \
      > ../"''${node_str}"/"''${funds_addr}".utxo
      # Reset variables for next batch iteration.
      txOuts_array=()
      batch="$((batch + ${toString outs_per_transaction}))"
    fi
  done

  # All the waiting / batchs above do not release the lock.
  # The next, expected, UTxO is stored again by this function!
  release_socket_lock         \
    "''${node_str}"           \
    "''${tx_filename}.signed" \
    "''${funds_addr}"
}

################################################################################
# Waits until the UTxOs of this address are not empty (errors on timeout).
################################################################################
function wait_any_utxo {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket.
  local addr=$2

  # Only defined in functions that use it.
  local socket_path="../''${node_str}/node.socket"

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
      ${cardano-cli}/bin/cardano-cli query utxo            \
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

  # Return first tx_id from the "cached" response (not get_address_utxo_0!).
    ${coreutils}/bin/echo "''${utxos_json}" \
  | ${jq}/bin/jq -r                         \
      'keys[0]'
}

################################################################################
# Waits until an specific UTxO of this address appears (errors on timeout).
################################################################################
function wait_utxo_id {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket.
  local addr=$2
  local utxo_id=$3

  # Only defined in functions that use it.
  local socket_path="../''${node_str}/node.socket"

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
      local utxos_json
      utxos_json="$( \
        ${cardano-cli}/bin/cardano-cli query utxo            \
          --testnet-magic     ${toString testnetMagic}       \
          --socket-path       "''${socket_path}"             \
          --address           "''${addr}"                    \
          --output-json                                      \
      )"
      contains_addr="$(                                      \
          ${coreutils}/bin/echo "''${utxos_json}"            \
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

  # Return the expected UTxO ID to be able to easily check the response.
  ${coreutils}/bin/echo "''${utxo_id}"
}

################################################################################
# Waits until an specific proposal appears and returns its "txId" or fails.
################################################################################
function wait_proposal_id {

  # Function arguments.
  local node_str=$1         # node name / folder to find the socket.
  local tx_signed=$2

  # Only defined in functions that use it.
  local socket_path="../''${node_str}/node.socket"

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
      local proposals_json
      # No "--output-json" needed.
      proposals_json="$( \
        ${cardano-cli}/bin/cardano-cli conway query gov-state     \
          --testnet-magic     ${toString testnetMagic}            \
          --socket-path       "''${socket_path}"                  \
      )"
      contains_proposal="$(                                       \
          ${coreutils}/bin/echo "''${proposals_json}"             \
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

  # Returns the proposal ID.
  ${coreutils}/bin/echo "''${tx_id}"
}

################################################################################
# Hack: Given a node "i" and a DRep number create always the same address keys.
# Only supports up to 99 nodes and 999999 DReps by adding the missing Hex chars.
# Returns the file path without the extensions (the ".skey" or ".vkey" part).
################################################################################
function create_node_drep_keys {

  # Function arguments.
  local node_str=$1 # String for the key file name (not for the socket).
  local node_i=$2   # This "i" is part of the node name ("node-i").
  local drep_i=$3

  local filename=../"''${node_str}"-drep-"''${drep_i}"
  # Now with the extensions.
  local skey="''${filename}".skey
  local vkey="''${filename}".vkey

  # Only create if not already there!
  if ! test -f "''${vkey}"
  then
      ${jq}/bin/jq --null-input \
        --argjson node_i "''${node_i}" \
        --argjson drep_i "''${drep_i}" \
        '
          {"type": "PaymentSigningKeyShelley_ed25519",
           "description": "Payment Signing Key",
           "cborHex": (
                "5820b02868d722df021278c78be3b7363759b37f5852b8747b488bab20c3"
              + (if   $node_i <=  9
                 then ("0" + ($node_i | tostring))
                 elif $node_i >= 10 and $node_i <= 99
                 then (       $node_i | tostring)
                 else (error ("Node ID above 99"))
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
# Get address of the node-drep combination!
################################################################################
function build_node_drep_address {

  # Function arguments.
  local node_str=$1 # String for the key file name (not for the socket).
  local node_i=$2   # This "i" is part of the node name ("node-i").
  local drep_i=$3

  local filename addr
  filename="$(create_node_drep_keys "''${node_str}" "''${node_i}" "''${drep_i}")"
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
# Deposit needed to create a proposal (`--governance-action-deposit` argument).
################################################################################
function get_gov_action_deposit {

  local node_str=$1       # node name / folder to find the socket.

  # Only defined in functions that use it.
  local socket_path="../''${node_str}/node.socket"

    ${cardano-cli}/bin/cardano-cli conway query gov-state \
      --testnet-magic ${toString testnetMagic}            \
      --socket-path "''${socket_path}"                    \
  | ${jq}/bin/jq -r '.currentPParams.govActionDeposit'
}

################################################################################
# Evenly distribute the "utxo_*key" genesis funds to all producer nodes.
# Splits all funds evenly to all producer-assigned addresses so then you can
# call function `governance_funds_producer_dreps` to split funds into the DReps
# each producer controls.
# Not to be run during the benchmarking phase: waits for funds to arrive!
################################################################################
function governance_funds_producers {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local utxo_vkey=$2
  local utxo_skey=$3
  local producers=${toString producers_array}

  # Send funds to each node (using DRep ID 0 as a special logical separation).
  ${coreutils}/bin/echo "governance_funds_producers: Node(s) splitting phase!"

  local action_deposit constitution_reminder
  action_deposit="$(get_gov_action_deposit "''${node_str}")"
  # HACK: Plus a fee estimate ("Estimated transaction fee: 172585 Lovelace").
  #       Plus "Minimum UTxO threshold: 105986 Lovelace"
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
    producer_addr="$(build_node_drep_address "''${producer_name}" "''${producer_i}" 0)"
    producers_addrs_array+=("''${producer_addr}")
    ${coreutils}/bin/echo "governance_funds_producers: Splitting to: ''${producer_name} - ''${producer_i} - 0 - ''${producer_addr}"
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
# Evenly distribute producer funds to all producer-assigned DReps.
# First send all funds evenly to all producer-assigned addresses using function
# `governance_funds_producers` and later call this function to do the same with
# the addresses assigned to each producers' DRep.
# (TODO in as many UTxO as the times it intends to vote ???).
# Not to be run during the benchmarking phase: waits for funds to arrive!
################################################################################
function governance_funds_producer_dreps {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local producer_name=$2

  # Send funds to each node's assigned DReps (DReps N to ???).
  ${coreutils}/bin/echo "governance_funds_producer_dreps: Node(s)-DRep(s) splitting phase!"

  local action_deposit proposals_reminder
  action_deposit="$(get_gov_action_deposit "''${node_str}")"
  # HACK: Plus a fee estimate ("Estimated transaction fee: 374457 Lovelace").
  #       Plus "Minimum UTxO threshold: 105986 Lovelace"
  proposals_reminder="$(( (action_deposit + 4000000) * ${toString withdrawal_proposals_per_node} + 200000 ))"

  local producer_i
  producer_i="$( \
    ${jq}/bin/jq --raw-output            \
      --arg keyName "''${producer_name}" \
      '.[$keyName].i'                    \
      ../node-specs.json                 \
  )"
  local producer_addr producer_vkey producer_skey
  producer_addr="$(build_node_drep_address "''${producer_name}" "''${producer_i}" 0)"
  producer_vkey="$(create_node_drep_keys   "''${producer_name}" "''${producer_i}" 0)".vkey
  producer_skey="$(create_node_drep_keys   "''${producer_name}" "''${producer_i}" 0)".skey

  # Wait for initial funds to arrive!
  ${coreutils}/bin/echo "governance_funds_producer_dreps: Wait for funds:  $(${coreutils}/bin/date --rfc-3339=seconds)"
  wait_any_utxo "''${node_str}" "''${producer_addr}" >/dev/null
  ${coreutils}/bin/echo "governance_funds_producer_dreps: Funds available: $(${coreutils}/bin/date --rfc-3339=seconds)"

  local producer_dreps_addrs_array=()
  local drep_step=0
  drep_step="$((producer_i * ${toString dreps_per_producer}))"
  local actual_drep
  for i in {1..${toString dreps_per_producer}}
  do
    local producer_drep_addr
    actual_drep="$((drep_step + i))"
    producer_drep_addr="$(build_node_drep_address "''${producer_name}" "''${producer_i}" "''${actual_drep}")"
    producer_dreps_addrs_array+=("''${producer_drep_addr}")
    ${coreutils}/bin/echo  "governance_funds_producer_dreps: Splitting to: ''${producer_name} - ''${producer_i} - ''${actual_drep} - ''${producer_drep_addr}"
  done

  # Split!
  funds_from_to \
    "''${node_str}"                           \
    "''${producer_vkey}" "''${producer_skey}" \
    "''${proposals_reminder}"                 \
    0                                         \
    "''${producer_dreps_addrs_array[@]}"

  # Wait for the funds of the last producer-drep to arrive.
  wait_any_utxo "''${node_str}" "''${producer_dreps_addrs_array[-1]}"
}

################################################################################
# Create constitution proposal and wait for it in the `gov-state` query.
# Not to be run during the benchmarking phase: Waits for the expected UTxO and
# if timeout uses the first one available.
# Not to be run during the benchmarking phase: waits for proposal to appear!
################################################################################
function governance_create_constitution {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local utxo_vkey=$2
  local utxo_skey=$3

  ${coreutils}/bin/echo "governance_create_constitution: ''${node_str} - ''${utxo_vkey}"

  # Only defined in functions that use it.
  local socket_path
  # Lock needed. Creates or destroys UTxOs.
  socket_path="$(get_socket_lock "''${node_str}")"

  local node_addr
  node_addr="$( \
    ${cardano-cli}/bin/cardano-cli address build  \
      --testnet-magic ${toString testnetMagic}    \
      --payment-verification-key-file "''${utxo_vkey}"
  )"

  # Funds needed for this governance action ?
  local action_deposit
  action_deposit="$(get_gov_action_deposit "''${node_str}")"
  # Funds address.
  # The input is calculated from the last transaction submitted.
  # No waiting! But, if last submitted transaction fails (function
  # `governance_funds_producers` in current workflow), everything else fails.
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
    --deposit-return-stake-verification-key-file ../genesis/pools/staking-reward1.vkey \
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
  wait_proposal_id "''${node_str}" "''${tx_filename}".signed

  release_socket_lock         \
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
  local node_str=$1       # node name / folder to find the socket.
  local node_i=$2
  local drep_i=$3

  ${coreutils}/bin/echo "governance_create_withdrawal: ''${node_str} - ''${drep_i}"

  # Only defined in functions that use it.
  local socket_path
  # Lock needed. Creates or destroys UTxOs.
  socket_path="$(get_socket_lock "''${node_str}")"

  local node_drep_skey node_drep_addr
  node_drep_skey="$(create_node_drep_keys   "''${node_str}" "''${node_i}" "''${drep_i}")".skey
  node_drep_addr="$(build_node_drep_address "''${node_str}" "''${node_i}" "''${drep_i}")"

  # Funds needed for this governance action ?
  local action_deposit
  action_deposit="$(get_gov_action_deposit "''${node_str}")"
  # Funds address.
  # The input is calculated from the last transaction submitted.
  # No waiting! But, if last submitted transaction fails (function
  # `governance_funds_producer_dreps` current workflow), everything else fails.
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
    --deposit-return-stake-verification-key-file  ../genesis/pools/staking-reward1.vkey                                          \
    --funds-receiving-stake-verification-key-file ../genesis/pools/staking-reward2.vkey                                          \
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
  wait_proposal_id "''${node_str}" "''${tx_filename}".signed

  release_socket_lock         \
    "''${node_str}"           \
    "''${tx_filename}.signed" \
    "''${node_drep_addr}"
}

################################################################################
function governance_vote_proposal {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local node_i=$2
  local proposal_id=$3

  # Only defined in functions that use it.
  local socket_path

  local drep_step actual_drep
  drep_step=$((node_i * ${toString dreps_per_producer}))
  for i in {1..${toString dreps_per_producer}}   # for drepDir in ../genesis/cache-entry/drep-keys/drep*
  do
    actual_drep="$((drep_step + i))"

    local proposal_flag="../''${node_str}/''${txId}.''${actual_drep}.voted"
    if test -f "''${proposal_flag}"
    then
      break
    fi

    ${coreutils}/bin/echo "governance_vote_proposal: ''${proposal_id} - ''${node_str} - ''${actual_drep}"

    # Lock needed. Creates or destroys UTxOs.
    socket_path="$(get_socket_lock "''${node_str}")"

    local node_drep_skey node_drep_addr
    node_drep_skey="$(create_node_drep_keys   "''${node_str}" "''${node_i}" "''${actual_drep}")".skey
    node_drep_addr="$(build_node_drep_address "''${node_str}" "''${node_i}" "''${actual_drep}")"

    # Funds address.
    # The input is calculated from the last transaction submitted.
    # No waiting! But, if last submitted transaction fails (function
    # `governance_funds_producer_dreps` or `governance_vote_proposal` in current
    # workflow), everything else fails.
    local funds_tx
    funds_tx="$(get_address_utxo_expected "''${node_str}" "''${node_drep_addr}")"
    # No cache if it's the first one from this node-drep.
    if test -z "''${funds_tx}"
    then
      funds_tx="$(get_address_utxo_0 "''${node_str}" "''${node_drep_addr}")"
    fi

    # Voting with DRep keys:
    local tx_filename=../"''${node_str}"/vote."''${node_str}"."''${actual_drep}"
    ${cardano-cli}/bin/cardano-cli conway governance vote create   \
      --yes                                                        \
      --governance-action-tx-id "''${proposal_id}"                 \
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
    ${cardano-cli}/bin/cardano-cli transaction sign                \
      --testnet-magic ${toString testnetMagic}                     \
      --signing-key-file ../genesis/cache-entry/drep-keys/drep"''${actual_drep}"/drep.skey \
      --signing-key-file "''${node_drep_skey}"                     \
      --tx-body-file "''${tx_filename}".raw                        \
      --out-file "''${tx_filename}".signed
    # Submit the transaction:
    ${cardano-cli}/bin/cardano-cli transaction submit              \
      --testnet-magic ${toString testnetMagic}                     \
      --socket-path "''${socket_path}"                             \
      --tx-file "''${tx_filename}".signed                          \
    >/dev/null

    ${coreutils}/bin/touch "''${proposal_flag}"

    release_socket_lock         \
      "''${node_str}"           \
      "''${tx_filename}.signed" \
      "''${node_drep_addr}"

  done
}

################################################################################
function governance_vote_all {

  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.
  local node_i=$2

  ${coreutils}/bin/echo "governance_vote_all: ''${node_str}"

  # Only defined in functions that use it.
  local socket_path="../''${node_str}/node.socket"

  # Store actual gov-state
    ${cardano-cli}/bin/cardano-cli conway query gov-state            \
      --testnet-magic ${toString testnetMagic}                       \
      --socket-path "''${socket_path}"                               \
  > "../''${node_str}/gov-state.json"

  # Cycle proposals.
  local txIdsJSON txIds
  txIdsJSON="$( \
      ${cardano-cli}/bin/cardano-cli conway query gov-state          \
        --testnet-magic ${toString testnetMagic}                     \
        --socket-path "''${socket_path}"                             \
    | ${jq}/bin/jq '.proposals | map(.actionId.txId)'                \
  )"
  txIds=$(echo "''${txIdsJSON}" | ${jq}/bin/jq --raw-output '. | join (" ")')
  for txId in ''${txIds[*]}
  do
    local proposal_flag="../''${node_str}/''${txId}.voted"
    if ! test -f "''${proposal_flag}"
    then
      governance_vote_proposal "''${node_str}" "''${node_i}" "''${txId}"
      ${coreutils}/bin/touch "''${proposal_flag}"
    fi
  done
}

################################################################################
# Entrypoints.
################################################################################

function workflow_generator {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.

  ${coreutils}/bin/echo "governance_funds_producers ........"
  governance_funds_producers       \
    "''${node_str}"                \
    ${toString genesis_funds_vkey} \
    ${toString genesis_funds_skey}

  ${coreutils}/bin/echo "governance_create_constitution  ..."
  governance_create_constitution   \
    "''${node_str}"                \
    ${toString genesis_funds_vkey} \
    ${toString genesis_funds_skey}
}

function workflow_producer {
  # Function arguments.
  local node_str=$1       # node name / folder to find the socket.

  local producer_i
  producer_i="$( \
    ${jq}/bin/jq --raw-output            \
      --arg keyName "''${node_str}"      \
      '.[$keyName].i'                    \
      ../node-specs.json                 \
  )"

  governance_funds_producer_dreps "''${node_str}" "''${node_str}"

  for i in {1..${toString withdrawal_proposals_per_node}}
  do
    governance_create_withdrawal "''${node_str}" "''${producer_i}" 0
  done

  governance_vote_all "''${node_str}" "''${producer_i}"
}

''
