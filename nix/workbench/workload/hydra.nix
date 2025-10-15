{ pkgs
, profile
, nodeSpecs
, workload
}:

let

  # supervisor: make playground-development-hydra
  # nomadexec:  nix-shell -A 'workbench-shell' --max-jobs 8 --cores 0 --show-trace --argstr profileName development-hydra-coay --argstr backendName nomadexec

  # Packages
  ##########

  bashInteractive    = pkgs.bashInteractive;
  coreutils          = pkgs.coreutils;
  jq                 = pkgs.jq;
  # Avoid rebuilding on every commit because of `set-git-rev`.
  cardano-cli        = pkgs.cardanoNodePackages.cardano-cli.passthru.noGitRev;
  # Hyra (Release 1.0.0).
  commit = "b5e33b55e9fba442c562f82cec6c36b1716d9847";
  flake = (__getFlake "github:cardano-scaling/hydra/${commit}");
  hydra = flake.packages.${builtins.currentSystem}.hydra-node;

  # Parameters
  ############

  testnet_magic = 42;
  baseport = workload.parameters.baseport or 31000;
  heads_per_cardano_node = workload.parameters.heads_per_cardano_node or 1;
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

in ''
${import ./utils/keys.nix
  { inherit coreutils jq cardano-cli testnet_magic;
  }
}

${import ./utils/utxo.nix
  { inherit coreutils jq cardano-cli testnet_magic;
  }
}

# Waits for all jobs to finish independent of their exit status!
# Returns the first error code obtained if any one fails.
wait_all () {
  wait_internal 0 "false" "$@"
}

# Waits for any job to fail or all to be OK!
# All processes are killed as soon as one fails!
# Returns the first error code obtained if any one fails.
wait_kill_em_all () {
  # We are scanning the scene in the city tonite ... searching, seek and destroy
  wait_internal 0 "true" "$@"
}

# Returns 0/success if no process fails, else returns the first error code
# obtained that is not zero.
wait_internal () {
  # The initial status for recursion, on first call it should always be zero!
  local initial_exit_status=''${1}; shift
  # Should all processes be killed as soon as one fails? Else waits for all
  # processes to finish independent of their exit status.
  local kill_em_all=''${1}; shift
  # Array of processes IDs or a jobs specifications.
  # If ID is a job specification, waits for all processes in that job's pipeline
  local processes_ids=("$@")
  # Are there any processes left to wait for ?
  if test -n "''${processes_ids[*]:-}"
  then
    local wait_exit_status
    local exited_process_id
    # Wait for a single job from the list of processes and returns its exit
    # status and the process or job identifier of the job for which the exit
    # status is returned is assigned to the variable provided by `-p VAR`.
    wait -n -p exited_process_id "''${processes_ids[@]}"
    wait_exit_status=$?
    # Only if the exit status to return is still zero we care about the
    # new exit status.
    if test "''${initial_exit_status}" -eq 0
    then
      initial_exit_status="''${wait_exit_status}"
    fi
    # Create a wew array without the newly exited process.
    local processes_ids_p=()
    for p in "''${processes_ids[@]}"
    do
      if test "''${p}" != "''${exited_process_id}"
      then
        processes_ids_p+=("''${p}")
      fi
    done
    # Are there still any processes left to wait for ?
    if test -n "''${processes_ids_p[*]:-}"
    then
      # Keep waiting or kill 'em all ?'
      if ! test "''${wait_exit_status}" -eq 0 && test "''${kill_em_all}" = "true"
      then
        kill "''${processes_p[@]}" 2>/dev/null || true
        return "''${wait_exit_status}"
      else
        # Recursion, wiiiiiiiii!
        wait_internal \
          "''${initial_exit_status}" "''${kill_em_all}" "''${processes_ids_p[@]}"
      fi
    else
      return "''${initial_exit_status}"
    fi
  else
    return 0
  fi
}

###############################################################################
# Workload entrypoint #########################################################
###############################################################################

function hydra {
  # Run the workflow for each deployed producer node.
  local producers=${toString producers_bash_array}
  local producers_jobs_array=()
  for producer_name in ''${producers[*]}
  do
    # Checks if the producer node is deployed in this machine.
    if test -d "../../''${producer_name}"
    then
      hydra_producer "''${producer_name}" &
      producers_jobs_array+=("$!")
    fi
  done
  wait_all "''${producers_jobs_array[@]}"
}

###############################################################################
# Producer node entrypoint ####################################################
###############################################################################

function hydra_producer {
  # Function arguments.
  local producer_name=$1 # node name / folder to find the socket to use.

  local producer_i
  # Lookup producer numeric index by name.
  producer_i="$( \
    ${jq}/bin/jq --raw-output            \
      --arg keyName "''${producer_name}" \
      '.[$keyName].i'                    \
      ../../node-specs.json              \
  )"

  msg "Starting producer \"''${producer_name}\" (''${producer_i})"

  # Parameters for this producer node:
  # - Where to obtain the genesis funds for this producer.
  genesis_funds_vkey="../../genesis/cache-entry/utxo-keys/utxo$((producer_i + 1)).vkey"
  genesis_funds_skey="../../genesis/cache-entry/utxo-keys/utxo$((producer_i + 1)).skey"
  # - IP address and port.
  producer_ip="127.0.0.1"
  producer_port="$((${toString baseport} + producer_i * 10))"

  msg "Producer params: ''${genesis_funds_vkey} - ''${genesis_funds_skey} - ''${producer_port}"

  # Split funds to each of this producer's head.
  producer_addr="$(build_x_y_z_address    "''${producer_name}" "''${producer_i}" 0 0)"
  producer_vkey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" 0 0)".vkey
  producer_skey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" 0 0)".skey
  local producer_head_addr_array=()
  for head_i in {1..${toString heads_per_cardano_node}}
  do
    local producer_head_addr
    producer_head_addr="$(build_x_y_z_address "''${producer_name}" "''${producer_i}" "''${head_i}" 0)"
    producer_head_addr_array+=("''${producer_head_addr}")
    ${coreutils}/bin/echo  "hydra_producer: Splitting to: ''${producer_name} - ''${producer_i} - ''${head_i} - ''${producer_head_addr}"
  done
  # Split (no need to wait for the funds or re-submit, function takes care)!
  funds_from_to \
    "''${producer_name}"                      \
    "''${genesis_funds_vkey}"                 \
    "''${genesis_funds_skey}"                 \
    0                                         \
    0                                         \
    "''${producer_head_addr_array[@]}"

  for head_i in $(seq 1 ${toString heads_per_cardano_node}); do
    hydra_producer_head    \
      "''${producer_name}" \
      "''${producer_i}"    \
      "''${producer_port}" \
      "''${head_i}"
  done
}

###############################################################################
# Producer-head entrypoint ####################################################
###############################################################################

function hydra_producer_head {
  # Function arguments.
  local producer_name=$1 # node name / folder to find the socket to use.
  local producer_i=$2
  local producer_port=$3
  local head_i=$4

  msg "Starting head: Producer \"''${producer_name}\" (''${producer_i}) head ''${head_i}"

  # Head parameters.
  head_vkey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" "''${head_i}" 0)".vkey
  head_skey="$(create_x_y_z_key_files "''${producer_name}" "''${producer_i}" "''${head_i}" 0)".skey
  head_addr="$(build_x_y_z_address    "''${producer_name}" "''${producer_i}" "''${head_i}" 0)"
  # - IP address and port.
  head_ip="127.0.0.1"
  head_port="$((producer_port + head_i))"

  msg "Head parameters: ''${head_port}"

  wait_any_utxo "''${producer_name}" "''${head_addr}"

  ${hydra}/bin/hydra-node                                             \
    --node-id                  "''${producer_i}"                      \
    --listen                   "''${head_ip}:''${head_port}"          \
    --advertise                "''${head_ip}:''${head_port}"          \
    --cardano-verification-key "''${genesis_funds_vkey}"              \
    --cardano-signing-key      "''${genesis_funds_skey}"              \
    --testnet-magic            ${toString testnet_magic}              \
    --node-socket              "../../''${producer_name}/node.socket"
}

###############################################################################
# Utils #######################################################################
###############################################################################

function msg {
  # Outputs to stdout, unbuffered if not the message may be lost!
  ${coreutils}/bin/stdbuf -o0 \
    ${bashInteractive}/bin/sh -c \
      "${coreutils}/bin/echo -e \"$(${coreutils}/bin/date --rfc-3339=seconds): $1\""
}
''
