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
  testnetMagic       = 42; # TODO: get it from `profile`!
  # Construct an "array" with node producers to use in BASH `for` loops.
  producers_array    =
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
  # Where to obtain the genesis funds from.
  genesis_funds_vkey = "../genesis/cache-entry/utxo-keys/utxo2.vkey";
  genesis_funds_skey = "../genesis/cache-entry/utxo-keys/utxo2.skey";
  # Sleeps.
  wait_any_utxos_tries = 20;
  wait_any_utxos_sleep =  5;
  wait_utxo_id_tries   = 10;
  wait_utxo_id_sleep   = 10;
  # How many constitutions to create with the genesis funds.
  constitutions_from_genesis = 1; # HACK: "Estimated transaction fee: 182617 Lovelace"
  # Initial donation from genesis funds to make "valid" withdrawal proposals.
  treasury_donation = 500000;
  # When splitting the genesis funds, we first move to a "node address" (called
  # DRep 0) for each producer, and then to a "node-drep address" for each
  # node-drep combination.
  # HACK: 1000 DReps for 52 producers. TODO: Calculate from profile / workload!
  dreps_per_producer = 19;
  # To calculate how much to leave on each nodes' address (DRep 0) for the nodes
  # to create withdrawal proposals (`--governance-action-deposit` argument).
  proposals_per_node = 2;
  # TODO: Calculate from profile and workload!
  votes_per_dreps = 2;
in ''

################################################################################
# Must be called by UTxO consumers.
################################################################################
function get_socket_lock {

  # Function arguments.
  local nodeName=''${1};  shift       # node name / folder to find the socket.

  local socket_path="../''${nodeName}/node.socket"
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
  local nodeName=$1       # node name / folder to find the socket.
  local tx_signed=$2
  local addr=$3

  local socket_path="../''${nodeName}/node.socket"
  local lockfile_path="''${socket_path}".lock

  # Store address next UTxO
    calculate_next_utxo                      \
      "''${tx_signed}"                       \
      "''${addr}"                            \
  > ../"''${nodeName}"/"''${addr}".utxo

  # A mystery!
  flock -u 200 2>/dev/null || true
  exec 200>&-
}

################################################################################
# Evenly split the first UTxO of this key to the addresses in the array!
# No waiting, no UTxO "cache", no fancy stuff for the input.
# Stores the future UTxO in a file for later references.
################################################################################
function funds_from_to {

  # Function arguments.
  local nodeName=''${1};  shift       # node name / folder to find the socket.
  local utxo_vkey=''${1}; shift       # In
  local utxo_skey=''${1}; shift       # In
  local reminder=''${1};  shift       # Funds to keep in the origin address.
  local donation=''${1};  shift       # To treasury.
  local addrs_array=("$@")            # Outs

  # Only defined in functions that use it.
  local socket_path
  # Lock needed. Creates or destroys UTxOs.
  socket_path="$(get_socket_lock "''${nodeName}")"

  # Get the "in" address and its first utxo.
  local funds_addr funds_json funds_tx funds_lovelace
  funds_addr="$( \
    ${cardano-cli}/bin/cardano-cli address build       \
      --testnet-magic ${toString testnetMagic}         \
      --payment-verification-key-file "''${utxo_vkey}" \
  )"
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
  local addrs_count per_out_lovelace
  addrs_count="''${#addrs_array[@]}"
  ### HACK: Fees! Always using 300000!!!
  ### With  2 outputs: "Estimated transaction fee: 172233 Lovelace"
  ### With 10 outputs: "Estimated transaction fee: 186665 Lovelace"
  ### With 53 outputs: "Estimated transaction fee: 264281 Lovelace"
  per_out_lovelace="$(                                                         \
    ${jq}/bin/jq -r --null-input                                               \
      --argjson numerator    "''${funds_lovelace}"                             \
      --argjson denominator  "''${addrs_count}"                                \
      --argjson reminder     "''${reminder}"                                   \
      --argjson donation     "''${donation}"                                   \
      '(($numerator - $reminder - $donation - 300000) / $denominator | round)' \
  )"

  # Build the "--tx-out" arguments array.
  local txOuts_array=()
  for addr in "''${addrs_array[@]}"
  do
    txOuts_array+=("--tx-out")
    txOuts_array+=("''${addr}+''${per_out_lovelace}")
  done

  # Some debugging!
  ${coreutils}/bin/echo "funds_from_to: ''${utxo_vkey} (''${funds_addr}): --tx-in ''${funds_tx} ''${txOuts_array[*]}"

  # Send to each node!
  # Build transaction.
  local tx_filename=../"''${nodeName}"/tx."''${funds_addr}"
  ${cardano-cli}/bin/cardano-cli conway transaction build               \
    --testnet-magic         ${toString testnetMagic}                    \
    --socket-path           "''${socket_path}"                          \
    --tx-in                 "''${funds_tx}"                             \
    ''${txOuts_array[@]}                                                \
    --treasury-donation     "''${donation}"                             \
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

  release_socket_lock         \
    "''${nodeName}"           \
    "''${tx_filename}.signed" \
    "''${funds_addr}"
}

################################################################################
# Given a tx.signed return TxHash#TxIx of the first occurrence of an address.
################################################################################
function calculate_next_utxo {

  # Function arguments.
  local tx_signed=$1
  local addr=$2

  local tx_id tx_ix

  tx_id="$( \
    ${cardano-cli}/bin/cardano-cli conway transaction txid  \
      --tx-file "''${tx_signed}"                            \
  )"

  tx_ix="$(                                                 \
      ${cardano-cli}/bin/cardano-cli debug transaction view \
        --tx-file "''${tx_signed}"                          \
    |                                                       \
      ${jq}/bin/jq -r                                       \
        --argjson addr "\"''${addr}\""                      \
        '
            .outputs
          | map(.address == $addr)
          | index(true)
        '                                                   \
  )"

  if test "''${tx_ix}" = "null"
  then
    error "ERROR"
  else
    ${coreutils}/bin/echo "''${tx_id}#''${tx_ix}"
  fi
}

################################################################################
# Get pre-calculated "cached" future UTxO or nothing (an "empty" string).
################################################################################
function get_address_utxo_expected {

  # Function arguments.
  local nodeName=$1       # node name / folder to find the socket.
  local addr=$2

  local utxo_file=../"''${nodeName}"/"''${addr}".utxo
  if test -f "''${utxo_file}"
  then
    ${coreutils}/bin/cat "''${utxo_file}"
  fi
}

################################################################################
# Get the first UTxO available (Must be non-empty or it fails!)
################################################################################
function get_address_utxo_0 {

  # Function arguments.
  local nodeName=$1       # node name / folder to find the socket.
  local addr=$2

  # Only defined in functions that use it.
  local socket_path="../''${nodeName}/node.socket"

    ${cardano-cli}/bin/cardano-cli query utxo       \
      --testnet-magic ${toString testnetMagic}      \
      --socket-path "''${socket_path}"              \
      --address "''${addr}"                         \
      --output-json                                 \
  | ${jq}/bin/jq -r 'keys[0]'
}

################################################################################
# Waits until the UTxOs of this address are not empty (Error on timeout).
################################################################################
function wait_any_utxos {

  # Function arguments.
  local nodeName=$1         # node name / folder to find the socket.
  local addr=$2

  # Only defined in functions that use it.
  local socket_path="../''${nodeName}/node.socket"

  local tries=${toString wait_any_utxos_tries}
  local utxos_json="{}"
  while test "''${utxos_json}" = "{}"
  do
    if test "''${tries}" = 0
    then
      ${coreutils}/bin/echo "wait_any_utxos: Failed waiting for ''${addr}"
    fi
    utxos_json="$( \
      ${cardano-cli}/bin/cardano-cli query utxo            \
        --testnet-magic     ${toString testnetMagic}       \
        --socket-path       "''${socket_path}"             \
        --address           "''${addr}"                    \
        --output-json                                      \
    )"
    if ! test "''${tries}" = ${toString wait_any_utxos_tries}
    then
      ${coreutils}/bin/sleep ${toString wait_any_utxos_sleep}
    fi
    tries="$((tries - 1))"
  done

  # Return first tx_id from the "cached" response (not get_address_utxo_0!).
    ${coreutils}/bin/echo "''${utxos_json}" \
  | ${jq}/bin/jq -r                         \
      'keys[0]'
}

################################################################################
# Waits until an specific UTxO of this address appears or returns empty.
################################################################################
function wait_utxo_id {

  # Function arguments.
  local nodeName=$1         # node name / folder to find the socket.
  local addr=$2
  local utxo_id=$3

  # Only defined in functions that use it.
  local socket_path="../''${nodeName}/node.socket"

  local contains_addr="false"
  local tries=${toString wait_utxo_id_tries}
  while test "''${contains_addr}" = "false"
  do
    if test "''${tries}" = 0
    then
      # Time's up!
      return;
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
# Waits until the UTxOs of this address are not empty.
################################################################################
function wait_utxo_expected_or_0 {

  # Function arguments.
  local nodeName=$1         # node name / folder to find the socket.
  local addr=$2

  local tx_id_expected=""
  tx_id_expected="$(get_address_utxo_expected "''${nodeName}" "''${addr}")"
  if test "''${tx_id_expected}" = "" || test -z "''${tx_id_expected}"
  then
    # No expected tx_id
    # Waits for any to appear and returns the first one.
    wait_any_utxos "''${nodeName}" "''${addr}"
  else
    local tx_id_wait
    tx_id_wait="$(wait_utxo_id "''${nodeName}" "''${addr}" "''${tx_id_expected}")"
    if test "''${tx_id_expected}" = "''${tx_id_wait}"
    then
      ${coreutils}/bin/echo "''${tx_id_expected}"
    else
      get_address_utxo_0
    fi
  fi
}

################################################################################
# Hack: Given a node "i" and a DRep number create always the same address keys.
# Only supports up to 99 nodes and 9999 DReps by adding the missing Hex chars.
# Returns the file path without the extensions (the ".skey" or ".vkey" part).
################################################################################
function create_node_drep_keys {

  # Function arguments.
  local nodeName=$1
  local node_i=$2   # This "i" is part of the node name ("node-i").
  local drep_i=$3

  local filename=../"''${nodeName}"-drep-"''${drep_i}"
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
                "5820b02868d722df021278c78be3b7363759b37f5852b8747b488bab20c3c8"
              + (if   $node_i <=  9
                 then ("0" + ($node_i | tostring))
                 elif $node_i >= 10 and $node_i <= 99
                 then (       $node_i | tostring)
                 else (error ("Node ID above 99"))
                 end
                )
              + (if   $drep_i <=    9
                 then ( "000" + ($drep_i | tostring))
                 elif $drep_i >=   10 and $drep_i <=   99
                 then (  "00" + ($drep_i | tostring))
                 elif $drep_i >=  100 and $drep_i <=  999
                 then (   "0" + ($drep_i | tostring))
                 elif $drep_i >= 1000 and $drep_i <= 9999
                 then (         ($drep_i | tostring))
                 else (error ("DRep ID above 9999"))
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
  local nodeName=$1 # node name to use.
  local node_i=$2   # This "i" is part of the node name ("node-i").
  local drep_i=$3

  local filename addr
  filename="$(create_node_drep_keys "''${nodeName}" "''${node_i}" "''${drep_i}")"
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

  local nodeName=$1       # node name / folder to find the socket.

  # Only defined in functions that use it.
  local socket_path="../''${nodeName}/node.socket"

    ${cardano-cli}/bin/cardano-cli conway query gov-state \
      --testnet-magic ${toString testnetMagic}            \
      --socket-path "''${socket_path}"                    \
  | ${jq}/bin/jq -r '.currentPParams.govActionDeposit'
}

################################################################################
# Evenly distribute to all producer nodes the "utxo_key" genesis funds.
# First send all funds evenly to all producers and later to an address for each
# producers DReps (TODO in as many UTxO as the times it intends to vote ???).
################################################################################
function governance_workload_funds {

  # Function arguments.
  local nodeName=$1       # node name / folder to find the socket.
  local utxo_vkey=$2
  local utxo_skey=$3
  local producers=${toString producers_array}

  local action_deposit
  action_deposit="$(get_gov_action_deposit "''${nodeName}")"

  local constitution_reminder
  # HACK: Plus a fee estimate.
  constitution_reminder="$((action_deposit * ${toString constitutions_from_genesis} + 500000))"

  # Send funds to each node (using DRep ID as 0).
  ${coreutils}/bin/echo "Node splitting phase!"
  local node_addrs_array=()
  for nodeName in ''${producers[*]}
  do
    local node_i
    local node_addr
    node_i="$( \
      ${jq}/bin/jq --raw-output        \
        --arg keyName "''${nodeName}"  \
        '.[$keyName].i'                \
        ../node-specs.json             \
    )"
    # Drep 0 is No DRep (funds for the node)
    node_addr="$(build_node_drep_address "''${nodeName}" "''${node_i}" 0)"
    node_addrs_array+=("''${node_addr}")
    ${coreutils}/bin/echo "Splitting to: ''${nodeName} - ''${node_i} - 0 - ''${node_addr}"
  done
  funds_from_to \
    "''${nodeName}"                   \
    "''${utxo_vkey}" "''${utxo_skey}" \
    "''${constitution_reminder}"      \
    ${toString treasury_donation}     \
    "''${node_addrs_array[@]}"

  local proposals_reminder
  # HACK: Plus a fee estimate.
  proposals_reminder="$((action_deposit * ${toString proposals_per_node} + 500000))"

  # Send funds to each node's assigned DReps (DReps N to ???).
  ${coreutils}/bin/echo "Nodes' assigned DRep(s) splitting phase!"
  local drep_step=0
  for nodeName in ''${producers[*]}
  do
    local node_i
    node_i="$( \
      ${jq}/bin/jq --raw-output        \
        --arg keyName "''${nodeName}"  \
        '.[$keyName].i'                \
        ../node-specs.json             \
    )"
    local actual_drep
    local node_i_addrs_array=()
    for i in {1..${toString dreps_per_producer}}
    do
      local node_addr
      actual_drep="$((drep_step + i))"
      node_addr="$(build_node_drep_address "''${nodeName}" "''${node_i}" "''${actual_drep}")"
      node_i_addrs_array+=("''${node_addr}")
      ${coreutils}/bin/echo "Splitting to: ''${nodeName} - ''${node_i} - ''${actual_drep} - ''${node_addr}"
    done
    local node_i_vkey node_i_skey
    node_i_vkey="$(create_node_drep_keys "''${nodeName}" "''${node_i}" 0)".vkey
    node_i_skey="$(create_node_drep_keys "''${nodeName}" "''${node_i}" 0)".skey
    local node_i_addr
    node_i_addr="$(build_node_drep_address "''${nodeName}" "''${node_i}" 0)"
    ${coreutils}/bin/echo "Wait for funds: $(${coreutils}/bin/date --rfc-3339=seconds)"
    wait_any_utxos "''${nodeName}" "''${node_i_addr}" >/dev/null
    ${coreutils}/bin/echo "Funds available: $(${coreutils}/bin/date --rfc-3339=seconds)"
    funds_from_to \
      "''${nodeName}"                       \
      "''${node_i_vkey}" "''${node_i_skey}" \
      "''${proposals_reminder}"             \
      0                                     \
      "''${node_i_addrs_array[@]}"
    drep_step="$((drep_step + ${toString dreps_per_producer}))"
  done
}

################################################################################
function governance_create_constitution {

  # Function arguments.
  local nodeName=$1
  local utxo_vkey=$2
  local utxo_skey=$3

  ${coreutils}/bin/echo "governance_create_constitution: ''${nodeName} - ''${utxo_vkey}"

  # Only defined in functions that use it.
  local socket_path
  # Lock needed. Creates or destroys UTxOs.
  socket_path="$(get_socket_lock "''${nodeName}")"

  local node_addr
  node_addr="$( \
    ${cardano-cli}/bin/cardano-cli address build  \
      --testnet-magic ${toString testnetMagic}    \
      --payment-verification-key-file "''${utxo_vkey}"
  )"

  # Funds needed for this governance action ?
  local action_deposit
  action_deposit="$(get_gov_action_deposit "''${nodeName}")"
  # Funds address.
  local funds_tx
  funds_tx="$(wait_utxo_expected_or_0 "''${nodeName}" "''${node_addr}")"

  # Show current gov-state.
    ${cardano-cli}/bin/cardano-cli conway query gov-state            \
      --testnet-magic ${toString testnetMagic}                       \
      --socket-path "''${socket_path}"                               \
  | ${jq}/bin/jq -r                                                  \
      '.nextRatifyState.nextEnactState.prevGovActionIds'

  # Create dummy constitution.
    ${coreutils}/bin/echo "My Constitution: free mate and asado"     \
  > ../"''${nodeName}"/constitution.txt
  # Calculate constitution hash.
  ${cardano-cli}/bin/cardano-cli hash anchor-data                    \
    --file-text ../"''${nodeName}"/constitution.txt                  \
    --out-file  ../"''${nodeName}"/constitution.hash
  # Copy guardrails-script.
  ${coreutils}/bin/cp                                                \
    ../genesis/guardrails-script.plutus                              \
    ../"''${nodeName}"/guardrails-script.plutus
  # Calculate guardrails-script hash.
  ${cardano-cli}/bin/cardano-cli hash script                         \
    --script-file ../"''${nodeName}"/guardrails-script.plutus        \
    --out-file    ../"''${nodeName}"/guardrails-script.hash

  # Create action.
  local tx_filename=../"''${nodeName}"/create-constitution
  ${cardano-cli}/bin/cardano-cli conway governance action create-constitution \
    --testnet \
    --anchor-url "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/cip-0100.common.schema.json" \
    --anchor-data-hash "9d99fbca260b2d77e6d3012204e1a8658f872637ae94cdb1d8a53f4369400aa9" \
    --constitution-url "https://ipfs.io/ipfs/Qmdo2J5vkGKVu2ur43PuTrM7FdaeyfeFav8fhovT6C2tto" \
    --constitution-hash        "$(cat ../"''${nodeName}"/constitution.hash)" \
    --constitution-script-hash "$(cat ../"''${nodeName}"/guardrails-script.hash)" \
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
    --out-file      "''${tx_filename}".raw
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
    --tx-file     "''${tx_filename}".signed

  release_socket_lock         \
    "''${nodeName}"           \
    "''${tx_filename}.signed" \
    "''${node_addr}"
}

################################################################################
function governance_create_withdrawal {

  # Function arguments.
  local nodeName=$1
  local node_i=$2
  local drep_i=$3

  ${coreutils}/bin/echo "governance_create_withdrawal: ''${nodeName} - ''${node_i} - ''${drep_i}"

  # Only defined in functions that use it.
  local socket_path
  # Lock needed. Creates or destroys UTxOs.
  socket_path="$(get_socket_lock "''${nodeName}")"

  local node_drep_vkey node_drep_skey node_drep_addr
  node_drep_vkey="$(create_node_drep_keys   "''${nodeName}" "''${node_i}" "''${drep_i}")".vkey
  node_drep_skey="$(create_node_drep_keys   "''${nodeName}" "''${node_i}" "''${drep_i}")".skey
  node_drep_addr="$(build_node_drep_address "''${nodeName}" "''${node_i}" "''${drep_i}")"

  # Funds needed for this governance action ?
  local action_deposit
  action_deposit="$(get_gov_action_deposit "''${nodeName}")"
  # Funds address.
  local funds_tx
  funds_tx="$(wait_utxo_expected_or_0 "''${nodeName}" "''${node_drep_addr}")"

  local tx_filename=../"''${nodeName}"/create-withdrawal
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
    --out-file      "''${tx_filename}".raw
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
    --tx-file     "''${tx_filename}".signed

  release_socket_lock         \
    "''${nodeName}"           \
    "''${tx_filename}.signed" \
    "''${node_drep_addr}"
}

################################################################################
function governance_vote_proposal {

  # Function arguments.
  local nodeName=$1
  local node_i=$2
  local proposal_id=$3

  ${coreutils}/bin/echo "governance_vote_proposal: ''${nodeName} - ''${node_i} - ''${proposal_id}"

  # Only defined in functions that use it.
  local socket_path
  # Lock needed. Creates or destroys UTxOs.
  socket_path="$(get_socket_lock "''${nodeName}")"

  local drep_step actual_drep
  drep_step=$((node_i * ${toString dreps_per_producer}))
  for i in {1..${toString dreps_per_producer}}   # for drepDir in ../genesis/cache-entry/drep-keys/drep*
  do
    actual_drep="$((drep_step + i))"

    local node_drep_vkey node_drep_skey node_drep_addr
    node_drep_vkey="$(create_node_drep_keys   "''${nodeName}" "''${node_i}" "''${actual_drep}")".vkey
    node_drep_skey="$(create_node_drep_keys   "''${nodeName}" "''${node_i}" "''${actual_drep}")".skey
    node_drep_addr="$(build_node_drep_address "''${nodeName}" "''${node_i}" "''${actual_drep}")"

    # Funds address.
    local funds_tx
    funds_tx="$(wait_utxo_expected_or_0 "''${nodeName}" "''${node_drep_addr}")"

    # Voting with DRep keys:
    local tx_filename=../"''${nodeName}"/vote
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
      --out-file "''${tx_filename}".raw
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
      --tx-file "''${tx_filename}".signed

    release_socket_lock         \
      "''${nodeName}"           \
      "''${tx_filename}.signed" \
      "''${node_drep_addr}"

    done
}

################################################################################
function governance_vote_all {

  # Function arguments.
  local nodeName=$1
  local node_i=$2

  ${coreutils}/bin/echo "governance_vote_all: ''${nodeName} - ''${node_i}"

  # Only defined in functions that use it.
  local socket_path="../''${nodeName}/node.socket"

  ## Show proposals
  #  ${cardano-cli}/bin/cardano-cli conway query gov-state            \
  #    --testnet-magic ${toString testnetMagic}                       \
  #    --socket-path "''${socket_path}"                               \
  #| ${jq}/bin/jq '.proposals'                                        \

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
    governance_vote_proposal "''${nodeName}" "''${node_i}" "''${txId}"
  done

  # TODO: TRY!
  governance_create_withdrawal "''${nodeName}" "''${node_i}" 0

}
''
