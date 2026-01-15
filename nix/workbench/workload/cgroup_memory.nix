{ pkgs
, haskellProject
, profile
, nodeSpecs
, workload
}:

with pkgs.lib;

let
  bashInteractive = pkgs.bashInteractive;
  coreutils       = pkgs.coreutils;
  jq              = pkgs.jq;
# Assumptions:
# - Command `date` and node's log use the same timezone!
in ''
#!${bashInteractive}/bin/sh

######################################################################
# Set script globals #################################################
######################################################################

# Strict runtime
################

# e:        Immediately exit if any command has a non-zero exit status
# u:        Reference to non previously defined variables is an error
# pipefail: Any failed command in a pipeline is used as return code
set -euo pipefail

# Fetch all defined node names (Including "explorer" nodes)
###########################################################

node_specs_nodes=$(${jq}/bin/jq --raw-output \
  "keys | join (\" \")"                      \
  ../../node-specs.json                      \
)
node_specs_pools=$(${jq}/bin/jq              \
  'map(select(.kind == "pool")) | length'    \
  ../../node-specs.json                      \
)
${coreutils}/bin/echo "node-specs.json:"
${coreutils}/bin/echo "- Nodes: [''${node_specs_nodes[*]}]"
${coreutils}/bin/echo "- Pools: ''${node_specs_pools}"

# Look for locally deployed nodes and save starting time
########################################################

nodes=()
started_time=$(${coreutils}/bin/date +%s)
for node in ''${node_specs_nodes[*]}
do
  if test -d "../../''${node}"
  then
    nodes+=("''${node}")
    # Save the starting time
    ${coreutils}/bin/echo "''${started_time}" > "./start_time_''${node}"
  fi
done
${coreutils}/bin/echo "Found deployed nodes:"
${coreutils}/bin/echo "- Nodes: [''${nodes[*]}]"

######################################################################
# Main ###############################################################
######################################################################

# The entrypoint function.
function cgroup_memory() {

  msg "Started!"

  local jobs_array=()
  for node in ''${nodes[*]}
  do
    cgroup_memory_deployed "''${node}" &
    jobs_array+=("$!")
  done
  wait "''${jobs_array[@]}"
}

function cgroup_memory_deployed() {
  local node=$1

  msg "Starting node: ''${node}"

  local cgroup
  cgroup="$(${coreutils}/bin/cat /proc/self/cgroup | ${coreutils}/bin/cut -d: -f3)"
  msg "CGroup found for \"''${node}\": ''${cgroup}"

  while true
  do
      ${coreutils}/bin/cat /sys/fs/cgroup"''${cgroup}"/memory.stat \
    > "./memory.stat_''${node}-''$(${coreutils}/bin/date +"%Y-%m-%d-%H-%M-%S-%3N")"
    ${coreutils}/bin/sleep 60
  done
}

######################################################################
# Utils ##############################################################
######################################################################

function msg {
  # Outputs to stdout, unbuffered if not the message may be lost!
  ${coreutils}/bin/stdbuf -o0 \
    ${bashInteractive}/bin/sh -c \
      "${coreutils}/bin/echo -e \"$(${coreutils}/bin/date --rfc-3339=seconds): $1\""
}
''
