{ pkgs }:

with pkgs.lib;

let
  latency-service =
    (nodeSpecs:
    let
      bashInteractive = pkgs.bashInteractive;
      coreutils       = pkgs.coreutils;
      iputils         = pkgs.iputils;
      jq              = pkgs.jq;
    in {
      start = rec {
        # Assumptions:
        # - Command `date` and node's log use the same timezone!
        value = ''
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
            ../node-specs.json                         \
          )
          node_specs_pools=$(${jq}/bin/jq              \
            'map(select(.kind == "pool")) | length'    \
            ../node-specs.json                         \
          )
          ${coreutils}/bin/echo "node-specs.json:"
          ${coreutils}/bin/echo "- Nodes: [''${node_specs_nodes[*]}]"
          ${coreutils}/bin/echo "- Pools: ''${node_specs_pools}"

          # Look for locally deployed nodes and allocate latency
          ######################################################

          nodes=()
          started_time=$(${coreutils}/bin/date +%s)
          for node in ''${node_specs_nodes[*]}
          do
            if test -d "../''${node}"
            then
              nodes+=("''${node}")
              # Create latency directory inside available node directories
              ${coreutils}/bin/mkdir "../''${node}/latency"
              # Save the starting time
              ${coreutils}/bin/echo "''${started_time}" > "../''${node}/latency/start_time"
            fi
          done
          ${coreutils}/bin/echo "Found deployed nodes:"
          ${coreutils}/bin/echo "- Nodes: [''${nodes[*]}]"

          ######################################################################
          # Main ###############################################################
          ######################################################################

          # The main function, called at the end of the file/script.
          function latency() {

            msg "Started!"

            for node in ''${nodes[*]}
            do

              msg "Latency of \"''${node}\"'s Producers using 'ping'"
              local topology_path="../''${node}/topology.json"
              # Merge non-P2P and P2P in the same {addr:"ADDR",port:0} format.
              local producers
              producers=$(${jq}/bin/jq '.Producers//[] + ((.localRoots[0].accessPoints//[]) | map({addr:.address,port:.port}))' "''${topology_path}")
              local keys
              keys=$(echo "''${producers}" | ${jq}/bin/jq --raw-output 'keys | join (" ")')
              for key in ''${keys[*]}
              do

                local host
                host=$(echo "''${producers}" | ${jq}/bin/jq --raw-output ".[''${key}].addr")
                local port
                port=$(echo "''${producers}" | ${jq}/bin/jq --raw-output ".[''${key}].port")

                # If the ping fails the whole script must fail!

                msg "Executing 'ping' to \"''${host}:''${port}\""
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O       "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O       "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O       "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O       "''${host}"

                msg "Executing 'ping' to \"''${host}:''${port}\" with size 16"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 16 "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 16 "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 16 "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 16 "''${host}"

                msg "Executing 'ping' to \"''${host}:''${port}\" with size 65507"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 65507 "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 65507 "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 65507 "''${host}"
                ${coreutils}/bin/sleep 60
                ${iputils}/bin/ping -D -c 60 -i 1 -n -O -s 65507 "''${host}"

              done

            done
          }

          function msg {
            # Outputs to stdout, unbuffered if not the message may be lost!
            ${coreutils}/bin/stdbuf -o0 \
              ${bashInteractive}/bin/sh -c \
                "${coreutils}/bin/echo -e \"$(${coreutils}/bin/date --rfc-3339=seconds): $1\""
          }

          latency $@
        '';
        JSON = pkgs.writeScript "startup-latency.sh" value;
      };
    })
    nodeSpecs;
in
  { inherit latency-service; }
