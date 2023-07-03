{ pkgs
, runJq
, backend
, profile
, nodeSpecs
}:

with pkgs.lib;

let
  ##
  ## generator-service :: (TracerConfig, NixosServiceConfig, Config, StartScript)
  ##
  healthcheck-service =
    (nodeSpecs:
    let
      bashInteractive = pkgs.bashInteractive;
      coreutils       = pkgs.coreutils;
      supervisor      = pkgs.supervisor;
      grep            = pkgs.gnugrep;
      jq              = pkgs.jq;
      cardano-cli     = pkgs.cardanoNodePackages.cardano-cli;
    in {
      start = rec {
        # Assumptions:
        # - Command `date` and node's log use the same timezone!
        value = ''
          #!${bashInteractive}/bin/sh

          # Set script globals #################################################
          ######################################################################

          # Strict runtime
          ################
          # e:        Immediately exit if any command has a non-zero exit status
          # u:        Reference to non previously defined variables is an error
          # pipefail: Any failed command in a pipeline is used as return code
          set -euo pipefail

          # Fetch profile parameters
          ##########################
          network_magic=$(${jq}/bin/jq      .genesis.network_magic      ../profile.json)
          slot_duration=$(${jq}/bin/jq      .genesis.slot_duration      ../profile.json)
          epoch_length=$(${jq}/bin/jq       .genesis.epoch_length       ../profile.json)
          active_slots_coeff=$(${jq}/bin/jq .genesis.active_slots_coeff ../profile.json)
          #active_slots="$((epoch_length * active_slots_coeff))"
          ${coreutils}/bin/echo "profile.json:"
          ${coreutils}/bin/echo "- network_magic:      ''${network_magic}"
          ${coreutils}/bin/echo "- slot_duration:      ''${slot_duration}"
          ${coreutils}/bin/echo "- epoch_length:       ''${epoch_length}"
          ${coreutils}/bin/echo "- active_slots_coeff: ''${active_slots_coeff}"
          #${coreutils}/bin/echo "- active_slots:       ''${active_slots}"

          # Fetch node names (Including "explorer" nodes)
          ###############################################
          node_specs_nodes=$(${jq}/bin/jq --raw-output "keys | join (\" \")"      ../node-specs.json)
          node_specs_pools=$(${jq}/bin/jq 'map(select(.kind == "pool")) | length' ../node-specs.json)
          ${coreutils}/bin/echo "node-specs.json:"
          ${coreutils}/bin/echo "- Nodes: [''${node_specs_nodes[*]}]"
          ${coreutils}/bin/echo "- Pools: ''${node_specs_pools}"

          # Look for available nodes and allocate healthcheck
          ###################################################
          nodes=()
          now=$(${coreutils}/bin/date +%s)
          for node in ''${node_specs_nodes[*]}
          do
            if test -d "../''${node}"
            then
              nodes+=("''${node}")
              # Create healthcheck directory inside available node directories
              ${coreutils}/bin/mkdir "../''${node}/healthcheck"
              # TODO: Store the `+RTS --info`
              # Save the starting time
              ${coreutils}/bin/echo "''${now}" > "../''${node}/healthcheck/start_time"
            fi
          done
          ${coreutils}/bin/echo "Found deployed nodes:"
          ${coreutils}/bin/echo "- Nodes: [''${nodes[*]}]"

          # Look for the generator
          ########################
          if test -d "../generator"
          then
            ${coreutils}/bin/echo "Found deployed generator"
          else
            ${coreutils}/bin/echo "Found no deployed generator"
          fi

          # Main ###############################################################
          ######################################################################

          # The main function, called at the end of the file/script.
          function healthcheck() {
            # Start the healthcheck infinite loop
            trap "${coreutils}/bin/echo \"trap PIPE\" >&2" PIPE
            msg "Started!"
            # Do a one and only networking/latency test!
            for node in ''${nodes[*]}
            do
              latency_topology_producers "''${node}"
            done
            while true
            do
              for node in ''${nodes[*]}
              do
                healthcheck_node "''${node}"
              done
              healthcheck_generator
              ${coreutils}/bin/sleep 5 # Enough?
            done
            trap - PIPE
          }

          ######################################################################

          function latency_topology_producers() {
            local node=$1
            msg "Latencies using 'cardano-cli ping' of \"''${node}\"'s Producers"
            local topology_path="../''${node}/topology.json"
            local keys=$(${jq}/bin/jq --raw-output '.Producers | keys | join (" ")' "''${topology_path}")
            for key in ''${keys[*]}
            do
              local host=$(${jq}/bin/jq --raw-output ".Producers[''${key}].addr" "''${topology_path}")
              local port=$(${jq}/bin/jq --raw-output ".Producers[''${key}].port" "''${topology_path}")
              msg "'cardano-cli ping' of \"''${host}:''${port}\""
              # If the ping fails the whole script must fail!
              ${cardano-cli}/bin/cardano-cli ping \
                --magic "''${network_magic}"      \
                --count 3                         \
                --json                            \
                --host "''${host}"                \
                --port "''${port}"
            done
          }

          function healthcheck_node() {
            local node=$1
            if is_program_running "''${node}"
            then
              # The node is running! ###########################################
              ##################################################################
              # Don't query the node's tip everytime once it got past slot 0!!!!
              # This avoids consuming the node's resources in "random" and also
              # avoids the corner case were the "tip" returns empty when
              # shutdown is requested / terminate signal was already sent.
              local synced_flag_path="../''${node}/healthcheck/synced"
              if ! test -f "''${synced_flag_path}"
              then
                # The node was not flagged as synced! ##########################
                ################################################################
                if ! is_node_synced_and_past_slot_zero "''${node}"
                then
                  # The node is still not synced! ##############################
                  ##############################################################
                  local now=$(${coreutils}/bin/date +%s)
                  local start_time=$(${coreutils}/bin/cat "../''${node}/healthcheck/start_time")
                  if test $((now - start_time)) -ge 180
                  then
                    exit_healthcheck "''${node}: More than 3m waiting for slot 0"
                  fi
                else
                  # The node is now synced! ####################################
                  ##############################################################
                  ${coreutils}/bin/touch "''${synced_flag_path}"
                fi
              else
                # The node was already flagged as synced! ######################
                ################################################################
                # TODO: healthcheck_node_forge "''${node}"
                healthcheck_node_block "''${node}"
                healthcheck_node_txs   "''${node}"
              fi
            fi
          }

          function healthcheck_generator() {
            # Only checks that the generator has not exited with errors
            is_program_running "generator" || true
          }

          function is_program_running() {
            local program=$1
            local exit_code_path="../''${program}/exit_code"
            # File exists and is a regular file?
            if ! test -f "''${exit_code_path}"
            then
              # The program was not even started! ##############################
              ##################################################################
              false
            else
              # The program was started! #######################################
              ##################################################################
              # File exists and has a size greater than zero?
              if test -s "''${exit_code_path}"
              then
                # The program exited! ##########################################
                ################################################################
                local exit_code
                exit_code="$(${coreutils}/bin/cat "''${exit_code_path}")"
                # Program failed with non-zero exit code?
                if test "''${exit_code}" != "0"
                then
                  # The program exited with a non-zero exit code! ##############
                  ##############################################################
                  # Exit!
                  exit_healthcheck \
                    "Program \"''${program}\" exited with a non-zero exit code!"
                else
                  # The program exited with a zero exit code! ##################
                  ##############################################################
                  # Only output a message, don't exit!
                  msg \
                    "Program \"''${program}\" exited with a zero exit code!"
                  false
                fi
              else
                # The program is running! ######################################
                ################################################################
                true
              fi
            fi
          }

          # `cardano-cli query tip` responses:
          # {
          #     "epoch": 0,
          #     "era": "Babbage",
          #     "slotInEpoch": 0,
          #     "slotsToEpochEnd": 600,
          #     "syncProgress": "100.00"
          # }
          # {
          #     "block": 1,
          #     "epoch": 0,
          #     "era": "Babbage",
          #     "hash": "9777....8833",
          #     "slot": 22,
          #     "slotInEpoch": 22,
          #     "slotsToEpochEnd": 578,
          #     "syncProgress": "100.00"
          # }
          function is_node_synced_and_past_slot_zero() {
            local node=$1
            local socket_path="../''${node}/node.socket"
            local db_path="../''${node}/db/"
            # Socket and db folder exist?
            if ! test -S "''${socket_path}" || ! test -d "''${db_path}"
            then
              false
            else
              local tip block slot
              # Returns nothing/empty/"" when the node is exiting
              tip=$(${cardano-cli}/bin/cardano-cli query tip \
                --testnet-magic "''${network_magic}"         \
                --socket-path "../''${node}/node.socket"
              )
              block=$(${coreutils}/bin/echo "''${tip}" | ${jq}/bin/jq .block)
              slot=$(${coreutils}/bin/echo  "''${tip}" | ${jq}/bin/jq .slot)
              # Block greater than or qeual to zero!
              if test -n "''${block}" && test "''${block}" != "null" && test "''${block}" -ge 0
              then
                # Slot greater than zero!
                if test -n "''${slot}" && test "''${slot}" != "null" && test "''${slot}" -gt 0
                then
                  true
                else
                  false
                fi
              else
                false
              fi
            fi
          }

          function healthcheck_node_forge() {
            local node=$1
            local start_time
            local now=$(${coreutils}/bin/date +%s)
            local last_forged
            last_forged=$(last_block_forged "''${node}")
            if test -z "''${last_forged}"
            then
              start_time=$(${coreutils}/bin/cat "../''${node}/healthcheck/start_time")
              if test $((now - start_time)) -ge 300
              then
                exit_healthcheck "''${node}: More than 5m with no blocks forged at all"
              fi
            else
              ${coreutils}/bin/echo "''${last_forged}" > "../''${node}/healthcheck/last_forged"
              start_time=$(${coreutils}/bin/echo "''${last_forged}" | ${jq}/bin/jq .at)
              if test $((now - start_time)) -ge 120
              then
                exit_healthcheck "''${node}: More than 2m with no newer blocks forged"
              fi
            fi
          }

          function healthcheck_node_block() {
            local node=$1
            local start_time
            local now=$(${coreutils}/bin/date +%s)
            local last_block
            last_block=$(last_block_transmitted "''${node}")
            if test -z "''${last_block}"
            then
              start_time=$(${coreutils}/bin/cat "../''${node}/healthcheck/start_time")
              if test $((now - start_time)) -ge 300
              then
                exit_healthcheck "''${node}: More than 5m without a first block sent or received"
              fi
            else
              ${coreutils}/bin/echo "''${last_block}" > "../''${node}/healthcheck/last_block"
              start_time=$(${coreutils}/bin/echo "''${last_block}" | ${jq}/bin/jq .at)
              if test $((now - start_time)) -ge 180
              then
                exit_healthcheck "''${node}: More than 3m with no newer blocks sent or received\n''${last_block}"
              fi
            fi
          }

          function healthcheck_node_txs() {
            local node=$1
            local start_time
            local now=$(${coreutils}/bin/date +%s)
            local last_txs
            last_txs=$(last_block_with_txs "''${node}")
            if test -z "''${last_txs}"
            then
              start_time=$(${coreutils}/bin/cat "../''${node}/healthcheck/start_time")
              if test $((now - start_time)) -ge 300
              then
                exit_healthcheck "''${node}: More than 5m without a first block with transactions"
              fi
            else
              ${coreutils}/bin/echo "''${last_txs}" > "../''${node}/healthcheck/last_txs"
              start_time=$(${coreutils}/bin/echo "''${last_txs}" | ${jq}/bin/jq .at)
              if test $((now - start_time)) -ge 180
              then
                exit_healthcheck "''${node}: More than 3m with no newer blocks with transactions\n''${last_txs}"
              fi
            fi
          }

          # Helper/auxiliary functions!
          ######################################################################

          # Gets the last "matching" JSON message from a Node's stdout file.
          #
          # To avoid reading the whole node's "stdout" file its contents are
          # reversed using `tac` (assuming `tac` is efficient) and piped to
          # `jq --unbuffered` that uses its "minimal support for I/O" using
          # 'inputs' to control over when inputs are read in combination with
          # the null input option '--null-input' to prevent one input from being
          # read implicitly.
          # With all these, we can efficiently select the first occurrence like
          # this: "nth(0; inputs | select( INSERT_BLOCK_QUERY ))"
          #
          # Note that the log file is composed of one JSON object per line were
          # 'inputs' read one by one and we are using `--compact-output` to make
          # sure every output log message matched by `jq` is contained within a
          # line.
          #
          # Also, the stdout of the node starts with some text that's echoed by
          # node's start.sh script that is not valid JSON, so we `grep` for
          # "{"at": ... }". We still need to check the exit code of these
          # functions just in case because if it fails a query may have failed
          # and the healthcheck should fail. This is tricky because when 'jq'
          # finishes and exists `tac` or `grep` may throw the following error:
          # "writing output failed: Broken pipe"
          #
          # Finally the "at" time has format "2023-05-16 19:57:19.0231Z" and I
          # can't parse it using any standard `date` format so I'm stripping the
          # milliseconds part and converting to Unix time (Integer). This has to
          # be done after filtering for "null" inputs that are the output of
          # 'nth(0;...)', if not an error occurs.
          #
          # $1: node name
          # $2: jq's query string
          function jq_node_stdout_last() {
            local node=$1
            local select=$2
            local stdout_path="../''${node}/stdout"
            local ans return_code=0 pipe_status=(0 0 0 0)
            ans="$( \
                  { ${coreutils}/bin/tac "''${stdout_path}" 2>/dev/null; } \
                | \
                  { ${grep}/bin/grep -E  "^{\"at\":.*}$"    2>/dev/null; } \
                | \
                  ${jq}/bin/jq                             \
                    --compact-output                       \
                    --unbuffered                           \
                    --null-input                           \
                    "nth(0; inputs | select(''${select}))" \
                | \
                  ${jq}/bin/jq \
                    '   select( . != null )
                      |
                        (
                             .at
                          |=
                             (.[:20] | strptime("%Y-%m-%d %H:%M:%S.") | mktime)
                        )
                    ' \
              || \
                { return_code="$?"; pipe_status="''${PIPESTATUS[@]}"; } \
            )"
            if test "''${return_code}" == 0
            then
               ${coreutils}/bin/echo "''${ans}"
            else
              # Ignore "writing output failed: Broken pipe"
              if test "''${pipe_status[2]}" != 0 || test "''${pipe_status[3]}" != 0 || test "''${return_code}" != 141
              then
                exit_22 "jq error: jq_node_stdout_last: ''${node}"
              else
                ${coreutils}/bin/echo "''${ans}"
              fi
            fi
          }

          # This one exists with "write error: Broken pipe"
          # To do this the stdout file is reversed using `tac`, `jq` is applied
          # and its answers fetched using `head --lines=1`. To avoid reading the
          # whole file (and assuming `tac` is efficient) `jq` is called with
          # `--unbuffered`.
          # To avoid "Error: writing output failed: Broken pipe"!" we use
          # `tail -n +1` between `jq` and `head`, is smart enough to check
          # standard output and closes the pipe down cleanly.
          function jq_node_stdout_last_v0() {
            local node=$1
            local select=$2
            local stdout_path="../''${node}/stdout"
            local ans return_code=0 pipe_status=(0 0 0 0 0)
            ans="$( \
                  { ${coreutils}/bin/tac "''${stdout_path}" 2>/dev/null; } \
                | \
                  { ${grep}/bin/grep -E  "^{\"at\":.*}$"    2>/dev/null; } \
                | \
                  ${jq}/bin/jq            \
                    --compact-output      \
                    --unbuffered          \
                    "select(''${select})" \
                | \
                  { ${coreutils}/bin/head --lines=+1        2>/dev/null; } \
                | \
                  ${jq}/bin/jq \
                    '
                        .at
                      |=
                        (.[:20] | strptime("%Y-%m-%d %H:%M:%S.") | mktime)
                    ' \
              || \
                { return_code="$?"; pipe_status="''${PIPESTATUS[@]}"; } \
            )"
            if test "''${return_code}" == 0
            then
               ${coreutils}/bin/echo "''${ans}"
            else
              # Ignore "writing output failed: Broken pipe"
              if test "''${pipe_status[2]}" != 0 || test "''${pipe_status[4]}" != 0 || test "''${return_code}" != 141
              then
                exit_22 "jq error: jq_node_stdout_last_v0: ''${node}"
              else
                ${coreutils}/bin/echo "''${ans}"
              fi
            fi
          }

          # {
          #   "at": "2023-06-01 21:26:14.0025Z",
          #   "ns": "Forge.Loop.NodeIsLeader",
          #   "data": {
          #     "kind": "TraceNodeIsLeader",
          #     "slot": 995
          #   },
          #   "sev": "Info",
          #   "thread": "34",
          #   "host": "ip-10-24-123-88.eu-central-1.compute.internal"
          # }
          function last_node_is_leader() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (.ns                       == "Forge.Loop.NodeIsLeader")
                and
                (.data.kind?               == "TraceNodeIsLeader")
                and
                (.data.slot?               != null)
              '
            then
              exit_22 "jq error: last_node_is_leader: ''${node}"
            fi
          }

          # {
          #   "at": "2023-05-16 15:02:46.0051Z",
          #   "ns": "Forge.Loop.AdoptedBlock",
          #   "data": {
          #     "blockHash": "26ab7db1ca55e69885665f625262118ef7db99172be6d3591ad2a8e9bfeabffa",
          #     "blockSize": 864,
          #     "kind": "TraceAdoptedBlock",
          #     "slot": 627
          #   },
          #   "sev": "Info",
          #   "thread": "34",
          #   "host": "ip-10-24-123-88.eu-central-1.compute.internal"
          # }
          function last_block_forged() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (.ns                       == "Forge.Loop.AdoptedBlock")
                and
                (.data.blockHash?          != null)
                and
                (.data.kind?               == "TraceAdoptedBlock")
              '
            then
              exit_22 "jq error: last_block_forged: ''${node}"
            fi
          }

          # Block sent:
          # {
          #   "at": "2023-05-17 16:00:57.0222Z",
          #   "ns": "BlockFetch.Server.SendBlock",
          #   "data": {
          #     "block": "dde....414",
          #     "kind": "BlockFetchServer"
          #   },
          #   "sev": "Info",
          #   "thread": "67",
          #   "host": "localhost"
          # }
          # Block received:
          # {
          #   "at": "2023-05-17 16:00:57.0246Z",
          #   "ns": "BlockFetch.Remote.Receive.Block",
          #   "data": {
          #     "kind": "Recv",
          #     "msg": {
          #       "agency": "ServerAgency TokStreaming",
          #       "blockHash": "dde....414",
          #       "blockSize": 64334,
          #       "kind": "MsgBlock",
          #       "txIds": [
          #         "60e....6ec",
          #         ...
          #         "95d....165"
          #       ]
          #     },
          #     "peer": {
          #       "connectionId": "127.0.0.1:37117 127.0.0.1:30000"
          #     }
          #   },
          #   "sev": "Info",
          #   "thread": "77",
          #   "host": "localhost"
          # }

          function last_block_transmitted() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (
                  (.ns                     == "BlockFetch.Server.SendBlock")
                  and
                  (.data.block?            != null)
                  and
                  (.data.kind?             == "BlockFetchServer")
                ) or (
                  (.ns                     == "BlockFetch.Remote.Receive.Block")
                  and
                  (.data.kind?             == "Recv")
                  and
                  (.data.msg?.blockHash    != null)
                  and
                  (.data.msg?.kind         == "MsgBlock")
                )
              '
            then
              exit_22 "jq error: last_block_transmitted: ''${node}"
            fi
          }

          function last_block_sent() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (.ns                       == "BlockFetch.Server.SendBlock")
                and
                (.data.block?              != null)
                and
                (.data.kind?               == "BlockFetchServer")
              '
            then
              exit_22 "jq error: last_block_sent: ''${node}"
            fi
          }

          function last_block_received() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (.ns                       == "BlockFetch.Remote.Receive.Block")
                and
                (.data.kind?               == "Recv")
                and
                (.data.msg?.blockHash      != null)
                and
                (.data.msg?.kind           == "MsgBlock")
              '
            then
              exit_22 "jq error: last_block_received: ''${node}"
            fi
          }

          # {
          #   "at": "2023-05-17 16:00:57.0246Z",
          #   "ns": "BlockFetch.Remote.Receive.Block",
          #   "data": {
          #     "kind": "Recv",
          #     "msg": {
          #       "agency": "ServerAgency TokStreaming",
          #       "blockHash": "dde....414",
          #       "blockSize": 64334,
          #       "kind": "MsgBlock",
          #       "txIds": [
          #         "60e....6ec",
          #         ...
          #         "95d....165"
          #       ]
          #     },
          #     "peer": {
          #       "connectionId": "127.0.0.1:37117 127.0.0.1:30000"
          #     }
          #   },
          #   "sev": "Info",
          #   "thread": "77",
          #   "host": "localhost"
          # }
          function last_block_with_txs() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (.ns                       == "BlockFetch.Remote.Receive.Block")
                and
                (.data.kind?               == "Recv")
                and
                (.data.msg?.blockHash      != null)
                and
                (.data.msg?.kind           == "MsgBlock")
                and
                (.data.msg?.txIds          != null)
                and
                (.data.msg?.txIds          != [])
              '
            then
              exit_22 "jq error: last_block_with_txs: ''${node}"
            fi
          }

          # {
          #   "at": "2023-05-17 16:00:08.3351Z",
          #   "ns": "Mempool.AddedTx",
          #   "data": {
          #     "kind": "TraceMempoolAddedTx",
          #     "mempoolSize": {
          #       "bytes": 1328,
          #       "numTxs": 1
          #     },
          #     "tx": {
          #       "txid": "3d724466"
          #     }
          #   },
          #   "sev": "Info",
          #   "thread": "66",
          #   "host": "localhost"
          # }
          function last_mempool_txs_added() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (.ns                       == "Mempool.AddedTx")
                and
                (.data.kind?               == "TraceMempoolAddedTx")
                and
                (.data.mempoolSize?.numTxs >= 1)
              '
            then
              exit_22 "jq error: last_mempool_txs_added: ''${node}"
            fi
          }

          # {
          #   "at": "2023-05-31 20:01:06.0238Z",
          #   "ns": "Shutdown.Requesting",
          #   "data": {
          #     "kind": "RequestingShutdown",
          #     "reason": "spawnLimitTerminator: reached target block BlockNo 3"
          #   },
          #   "sev": "Warning",
          #   "thread": "38",
          #   "host": "localhost"
          # }
          function last_shutdown_requesting() {
            local node=$1
            if ! jq_node_stdout_last "''${node}" \
              '
                (.ns                       == "Shutdown.Requesting")
                and
                (.data.kind?               == "RequestingShutdown")
              '
            then
              exit_22 "jq error: last_shutdown_requesting: ''${node}"
            fi
          }

          function msg {
            # Outputs to stdout, unbuffered if not the message may be lost!
            ${coreutils}/bin/stdbuf -o0 \
              ${bashInteractive}/bin/sh -c \
                "${coreutils}/bin/echo -e \"$(${coreutils}/bin/date --rfc-3339=seconds): $1\""
          }

          function exit_healthcheck {
            # Outputs to stdout and stderr, unbuffered if not message may be lost!
            ${coreutils}/bin/stdbuf -o0 -e0 \
              ${bashInteractive}/bin/sh -c \
                "${coreutils}/bin/echo -e \"$(${coreutils}/bin/date --rfc-3339=seconds): $1\" | ${coreutils}/bin/tee /dev/stderr"
            exit 1
          }

          function exit_22 {
            # Outputs to stdout and stderr, unbuffered if not message may be lost!
            ${coreutils}/bin/stdbuf -o0 -e0 \
              ${bashInteractive}/bin/sh -c \
                "${coreutils}/bin/echo -e \"$(${coreutils}/bin/date --rfc-3339=seconds): $1\" | ${coreutils}/bin/tee /dev/stderr"
            exit 22
          }

          if test -n "''${NOMAD_DEBUG:-}"
          then
            DEBUG_FILE="$(${coreutils}/bin/dirname "$(${coreutils}/bin/readlink -f "$0")")"/"$0".debug
            echo "Using debug file ''${DEBUG_FILE}" >&2
            exec 5> "''${DEBUG_FILE}"
            BASH_XTRACEFD="5"
            PS4='$LINENO: '
            set -x
          fi

          healthcheck $@
        '';
        JSON = pkgs.writeScript "startup-healthcheck.sh" value;
      };
    })
    nodeSpecs;
in
  { inherit healthcheck-service; }
