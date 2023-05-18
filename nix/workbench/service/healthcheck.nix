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
      startupScript = rec {
        JSON = pkgs.writeScript "startup-healthcheck.sh" value;
        jqFilter = "";
        # Assumptions:
        # - Command `date` and node's log use the same timezone!
        value = ''
          #!${bashInteractive}/bin/sh

          NOMAD_DEBUG=1

          # e:        Immediately exit if any command has a non-zero exit status
          # u:        Reference to non previously defined variables is an error
          # pipefail: Any failed command in a pipeline is used as return code
          set -euo pipefail

          echo "$(${coreutils}/bin/date --rfc-3339=seconds): started"

          # Fetch every node name (Including "explorer" nodes)
          nodes=$(${jq}/bin/jq --raw-output "keys | join (\" \")"      ../node-specs.json)
          pools=$(${jq}/bin/jq 'map(select(.kind == "pool")) | length' ../node-specs.json)
          echo "Nodes: [''${nodes}]"
          echo "Pools: ''${pools}"

          # Set the defaults!
          now=$(${coreutils}/bin/date +%s)
          for node in ''${nodes[*]}
          do
            # Create a healthcheck directory inside every node directory
            ${coreutils}/bin/mkdir "../''${node}/healthcheck"
            # TODO: Store the `+RTS --info`
            # Save the starting time
            echo "''${now}" > "../''${node}/healthcheck/start_time"
          done

          # Fetch profile parameters
          network_magic=$(${jq}/bin/jq      .genesis.network_magic      ../profile.json)
          slot_duration=$(${jq}/bin/jq      .genesis.slot_duration      ../profile.json)
          epoch_length=$(${jq}/bin/jq       .genesis.epoch_length       ../profile.json)
          active_slots_coeff=$(${jq}/bin/jq .genesis.active_slots_coeff ../profile.json)
          #active_slots="$((epoch_length * active_slots_coeff))"
          echo "network_magic:      ''${network_magic}"
          echo "slot_duration:      ''${slot_duration}"
          echo "epoch_length:       ''${epoch_length}"
          echo "active_slots_coeff: ''${active_slots_coeff}"
          #echo "active_slots:      ''${active_slots}"

          # The main function, called at the end of the file/script.
          function healthcheck() {
            # Start the healthcheck infinite loop
            trap "" PIPE
            while true
            do
              for node in ''${nodes[*]}
              do
                node_healthcheck "''${node}"
              done
              sleep 5 # Enough?
            done
            trap - PIPE
          }

          function node_healthcheck() {
            local node=$1
            local exit_code_path="../''${node}/exit_code"
            # File exists and is a regular file?
            if test -f "''${exit_code_path}"
            then
              # The node was started!
              # File exists and has a size greater than zero?
              if test -s "''${exit_code_path}"
              then
                # The node exited!
                # Node failed with non-zero exit code?
                if test "''${exit_code}" != "0"
                then
                  # The node exited with a non-zero exit code!
                  exit_healthcheck "Node \"''${node}\" exited with a non-zero exit code!"
                else
                  # The node exited with a zero exit code!
                  echo "Node \"''${node}\" exited with a zero exit code!"
                fi
              else
                # The node is running!
                if is_node_synced_and_past_slot_zero "''${node}"
                then
                  node_healthcheck_block "''${node}"
                  node_healthcheck_txs   "''${node}"
                else
                  local now=$(${coreutils}/bin/date +%s)
                  local start_time=$(cat "../''${node}/healthcheck/start_time")
                  if test $((now - start_time)) -ge 300
                  then
                    exit_healthcheck "''${node}: More than 5m waiting for slot 0"
                  fi
                fi
              fi
            fi
          }

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
            local db_path="../''${node}/run/current/''${node}/db-testnet/"
            # Socket and db folder exist!
            if ! test -S "''${socket_path}" || ! test -d "''${db_path}"
            then
              false
            else
              local tip block slot
              tip=$(${cardano-cli}/bin/cardano-cli query tip \
                --testnet-magic "''${network_magic}"         \
                --socket-path "../''${node}/node.socket"
              )
              block=$(echo "''${tip}" | ${jq}/bin/jq .block)
              slot=$(echo  "''${tip}" | ${jq}/bin/jq .slot)
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

          function node_healthcheck_forge() {
            local node=$1
            local start_time
            local now=$(${coreutils}/bin/date +%s)
            local last_forged
            last_forged=$(last_block_forged "''${node}" 2>&1)
            if test -z "''${last_forged}"
            then
              start_time=$(cat "../''${node}/healthcheck/start_time")
              if test $((now - start_time)) -ge 300
              then
                exit_healthcheck "''${node}: More than 5m with no blocks forged at all"
              fi
            else
              echo "''${last_forged}" > "../''${node}/healthcheck/last_forged"
              start_time=$(echo "''${last_forged}" | ${jq}/bin/jq .at)
              if test $((now - start_time)) -ge 120
              then
                exit_healthcheck "''${node}: More than 2m with no newer blocks forged"
              fi
            fi
          }

          function node_healthcheck_block() {
            local node=$1
            local start_time
            local now=$(${coreutils}/bin/date +%s)
            local last_block
            last_block=$(last_block_transmitted "''${node}" 2>&1)
            if test -z "''${last_block}"
            then
              start_time=$(cat "../''${node}/healthcheck/start_time")
              if test $((now - start_time)) -ge 300
              then
                # Stderr
                echo "$(${coreutils}/bin/date --rfc-3339=seconds): ''${node}: More than 5m without a first block sent or received" >&2
                exit 1
              fi
            else
              echo "''${last_block}" > "../''${node}/healthcheck/last_block"
              start_time=$(echo "''${last_block}" | ${jq}/bin/jq .at)
              if test $((now - start_time)) -ge 60
              then
                exit_healthcheck "''${node}: More than 60s with no newer blocks sent or received\n''${last_block}"
              fi
            fi
          }

          function node_healthcheck_txs() {
            local node=$1
            local start_time
            local now=$(${coreutils}/bin/date +%s)
            local last_txs
            last_txs=$(last_block_with_txs "''${node}" 2>&1)
            if test -z "''${last_txs}"
            then
              start_time=$(cat "../''${node}/healthcheck/start_time")
              if test $((now - start_time)) -ge 300
              then
                exit_healthcheck "''${node}: More than 5m without a first block with transactions"
              fi
            else
              echo "''${last_txs}" > "../''${node}/healthcheck/last_txs"
              start_time=$(echo "''${last_txs}" | ${jq}/bin/jq .at)
              if test $((now - start_time)) -ge 60
              then
                exit_healthcheck "''${node}: More than 60s with no newer blocks with transactions\n''${last_txs}"
              fi
            fi
          }

          # We have txIds
          # 20 seconds for new one, 3 minutes for first one!
          # Also check block number to see if slots are going forward!
          # A specific number of slot before looking for txIds ?

          # Helper/auxiliary functions!
          ######################################################################

          # Gets the last "matching" JSON message from a Node's stdout file.
          #
          # To do this the stdout file is reversed using `tac`, `jq` is applied
          # and its answers fetched using `head --lines=1`. To avoid reading the
          # whole file (and assuming `tac` is efficient) `jq` is called with
          # `--unbuffered`.
          #
          # Note that the log file is composed of one JSON object per line and
          # we are using `--compact-output` to make sure every output log
          # message matched by `jq` is contained within a line.
          #
          # Also, the stdout of the node starts with some text that echoed by
          # node's start.sh script that is not valid JSON, so we `grep` for
          # "{"at": ... }". We still need to check the exit code of these
          # functions just in case because if it fails a query may have failed
          # and the healthcheck should fail.
          #
          # Finally the "at" time has format "2023-05-16 19:57:19.0231Z" and I
          # can't parse it using any standard `date` format so I'm stripping the
          # milliseconds part and converting to Unix time (Integer).
          #
          # To avoid "Error: writing output failed: Broken pipe"!" we use
          # `tail -n +1` between `jq` and `head`, is smart enough to check
          # standard output and closes the pipe down cleanly.
          #
          # $1: node name
          # $2: jq's query string
          function jq_node_stdout_last() {
            local node=$1
            local select=$2
            local stdout_path="../''${node}/stdout"
            if !  ${coreutils}/bin/tac "''${stdout_path}" \
                | \
                  ${grep}/bin/grep -E "^{\"at\":.*}$" \
                | \
                  ${jq}/bin/jq                             \
                    --compact-output                       \
                    --unbuffered                           \
                    --null-input                           \
                    "nth(0; inputs | select(''${select}))" \
                | \
                  ${jq}/bin/jq \
                    '
                        .at
                      |=
                        (.[:20] | strptime("%Y-%m-%d %H:%M:%S.") | mktime)
                    '
            then
              exit_22 "stdout error: jq_node_stdout_last: ''${node}"
            fi
          }

          # This one exists with "write error: Broken pipe"
          function jq_node_stdout_last_v0() {
            local node=$1
            local select=$2
            local stdout_path="../''${node}/stdout"
            if !  ${coreutils}/bin/tac "''${stdout_path}" \
                | \
                  ${grep}/bin/grep -E "^{\"at\":.*}$" \
                | \
                  ${jq}/bin/jq --compact-output --unbuffered "''$2" \
                | \
                  ${coreutils}/bin/head --lines=+1 \
                | \
                  ${jq}/bin/jq \
                    '
                        .at
                      |=
                        (.[:20] | strptime("%Y-%m-%d %H:%M:%S.") | mktime)
                    '
            then
              exit_22 "stdout error: jq_node_stdout_last: ''${node}"
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
          #   "host": "rapide"
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
          #   "host": "rapide"
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
          #   "host": "rapide"
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
          #   "host": "rapide"
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

          function exit_healthcheck {
            # Unbuffered stderr, if not the error message may be lost!
            stdbuf -o0 -e0 echo -e "$(${coreutils}/bin/date --rfc-3339=seconds): $1" >&2
            exit 1
          }

          function exit_22 {
            # Unbuffered stderr, if not the error message may be lost!
            stdbuf -o0 -e0 echo -e "$(${coreutils}/bin/date --rfc-3339=seconds): $1" >&2
            exit 22
          }

          if test -n "''${NOMAD_DEBUG:-}"
          then
            exec 5> "$(dirname "$(readlink -f "$0")")"/start.sh.debug
            BASH_XTRACEFD="5"
            PS4='$LINENO: '
            set -x
          fi

          healthcheck $@
        '';
      };
    })
    nodeSpecs.value;
in
  { inherit healthcheck-service; }
