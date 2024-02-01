usage_nomad() {
     usage "nomad" "nomad helpers" <<EOF
    $(helpcmd dir-path \(vault\|client\|server\|plugin\|webfs\))
                     Gets the corresponding cache directory file path.

    $(helpcmd vault \(ci\|world\) login)
                     Login with your GitHub token. First copy the token by doing:
                     Your profile -> Settings -> Developer Settings ->
                     Tokens (Classic) -> Generate New Token (Classic)
                     and create a new token with only the "read:org" permission.
    $(helpcmd vault \(ci\|world\) nomad-token)
                     Gets the corresponding Nomad token from the chosen Vault
                     (WARNING: shows secrets!!!).
    $(helpcmd vault ci pg-user)
                     Gets SRE's Postgres server username from Vault
                     (WARNING: shows secrets!!!).
    $(helpcmd vault ci pg-pass)
                     Gets SRE's Postgres server password from Vault
                     (WARNING: shows secrets!!!).
    $(helpcmd vault world aws-s3-credentials)
                     Gets Cardano World's AWS S3 crdentials from Vault in JSON
                     (WARNING: shows secrets!!!).

    $(helpcmd nodes)
                     Creates a JSON array with all the SRE's perf nodes in a
                     format that can be used to ensure cloud runs are
                     reproducible.
                     Needed envars (NOMAD_TOKEN, NOMAD_ADDR or NOMAD_NAMESPACE)
                     must be provided by the user.

    $(helpcmd agents start SERVER-NAME CLIENT-NAME TASK-DRIVER-NAME)
                     Start a default 1 server 1 client Nomad cluster.
    $(helpcmd agents stop  SERVER-NAME CLIENT-NAME TASK-DRIVER-NAME)
                     Stop the default 1 server 1 client Nomad cluster.

    $(helpcmd \(server/client\) state-dir-path           NAME)
                     Location of the Agent's everything folder
    $(helpcmd \(server/client\) config-file-path         NAME)
                     Location of the Agent's config file (using only one)
    $(helpcmd \(server/client\) configure                NAME HTTP-PORT RPC-PORT SERV-PORT)
    $(helpcmd \(server/client\) port \(http\|rcp\|serv\) NAME)
                     Getter for the previously configured ports
    $(helpcmd \(server/client\) pid-filepath             NAME)
                     Location of the Agent's "running" flag
    $(helpcmd \(server/client\) pids-array               NAME)
                     Search for Agent's PIDs independent of the "running" flag
    $(helpcmd \(server/client\) is-running               NAME)
                     True if the "running" flag is present or there are PIDs
    $(helpcmd \(server/client\) start                    NAME)
                     Must call "configure" first
    $(helpcmd \(server/client\) stop                     NAME)
                     Stops the Agent but does not clean its files
    $(helpcmd \(server/client\) cleanup                  NAME)
                     Deletes all logs and state files

    $(helpcmd plugin nomad-driver-podman socket-path)
    $(helpcmd plugin nomad-driver-podman pid-filepath)
    $(helpcmd plugin nomad-driver-podman pid)
    $(helpcmd plugin nomad-driver-podman is-running)
    $(helpcmd plugin nomad-driver-podman start)
    $(helpcmd plugin nomad-driver-podman stop)

    $(helpcmd webfs state-dir-path)
    $(helpcmd webfs document-root-path)
    $(helpcmd webfs add-genesis-dir)
    $(helpcmd webfs pid-filepath)
    $(helpcmd webfs pids-array)
    $(helpcmd webfs is-running)
    $(helpcmd webfs start)
    $(helpcmd webfs stop)

    $(helpcmd nuke)

    $(helpcmd job start)
    $(helpcmd job stop)
    $(helpcmd job monitor)
    $(helpcmd job monitor-job-evals)
    $(helpcmd job monitor-job-allocs)
    $(helpcmd job monitor-job-alloc-tasks)
    $(helpcmd job check-eval-id-placement-failures)
    $(helpcmd job monitor-eval-id)
    $(helpcmd job monitor-deploy-id)
    $(helpcmd job monitor-alloc-id)
    $(helpcmd job monitor-alloc-id-task-name)
    $(helpcmd job task-name-allocation-id)
    $(helpcmd job task-name-node-name)
EOF
}

wb_nomad() {

  # Note on the use of bash's `local`:
  # "Local can only be used within a function; it makes the variable name have a
  # visible scope restricted to that function and its children."
  # "when declaring and setting a local variable in a single command, apparently
  # the order of operations is to first set the variable, and only afterwards
  # restrict it to local scope."
  # https://tldp.org/LDP/abs/html/localvar.html

  local op=${1:-$nomad_default_op}; test $# -gt 0 && shift

  case "${op}" in

################################################################################
### dir-path ) #################################################################
################################################################################
    dir-path )
      local usage="USAGE: wb nomad ${op} vault|server|client|plugin|webfs"
      # Calling `wb nomad dir-path  XXX` inside a Nix derivation will fail:
      # "mkdir: cannot create directory '/homeless-shelter': Permission denied"
      local nomad_cache_dir="$(envjqr 'cacheDir')"/nomad
      mkdir -p "${nomad_cache_dir}"
      local subop=${1:?$usage}; shift
      case "${subop}" in
        vault )
          local vault_dir="${nomad_cache_dir}"/vault
          mkdir -p "${vault_dir}"
          echo "${vault_dir}"
        ;;
        server )
          local nomad_servers_dir="${nomad_cache_dir}"/server
          mkdir -p "${nomad_servers_dir}"
          echo "${nomad_servers_dir}"
        ;;
        client )
          local nomad_clients_dir="${nomad_cache_dir}"/client
          mkdir -p "${nomad_clients_dir}"
          echo "${nomad_clients_dir}"
        ;;
        plugin )
          local plugin_dir="${nomad_cache_dir}"/plugin
          mkdir -p "${plugin_dir}"
          echo "${plugin_dir}"
        ;;
        webfs )
          local webfs_dir="${nomad_cache_dir}"/webfs
          mkdir -p "${webfs_dir}"
          echo "${webfs_dir}"
        ;;
        ssh )
          local ssh_dir="${nomad_cache_dir}"/ssh
          mkdir -p "${ssh_dir}"
          echo "${ssh_dir}"
        ;;
        * )
          usage_nomad
        ;;
      esac
    ;;
################################################################################
### vault ) ####################################################################
################################################################################
    vault )
      local usage="USAGE: wb nomad ${op} world|ci"
      local vault_dir="$(wb_nomad dir-path vault)"
      local entity=${1:?$usage}; shift
      case "${entity}" in
####### vault -> ci )###########################################################
        ci )
          mkdir -p "${vault_dir}"/ci
          local login_file="${vault_dir}"/ci/login.json
          local vault_address="https://vault.ci.iog.io"
          local action=${1:?$usage}; shift
          case "${action}" in
            login )
              msg "First create and copy your GitHub token by doing: "
              msg "Your profile -> Settings -> Developer Settings -> Tokens (Classic) -> Generate New Token (Classic)"
              msg "and create a new token with only the \"read:org\" permission."
              read -p "Hit enter to continue ..."
              vault login                             \
                -address="${vault_address}"           \
                -method=github -path=github-employees \
                -no-store -format=json                \
              > "${login_file}"
            ;;
            enabled )
              if test -f "${login_file}"
              then
                # Fetch token info from vault
                local client_token
                client_token=$(jq -r '.auth.client_token' "${login_file}")
                local token_lookup_response
                if token_lookup_response=$(VAULT_TOKEN="${client_token}" vault token lookup -address="${vault_address}" -namespace=perf -format=json)
                then
                  local expire_time
                  expire_time=$(echo "${token_lookup_response}" | jq -r .data.expire_time)
                  # Compare expire date with the actual date minus one day.
                  # This avoids a token expiring while a profile is running.
                  if test "$(date -u -d "${expire_time}" "+%s")" -ge "$(($(date -u "+%s") - 86400))"
                  then
                    true
                  else
                    rm "${login_file}"
                    false
                  fi
                else
                  fatal "Are you logged in to Vault? Call 'wb nomad vault ${entity} login' with your IOHK GitHub token (classic)"
                fi
              else
                false
              fi
            ;;
            nomad-token )
              if ! wb_nomad vault "${entity}" enabled
              then
                wb_nomad vault "${entity}" login
              fi
              local client_token
              client_token=$(jq -r '.auth.client_token' "${login_file}")
              local nomad_token_json
              if nomad_token_json=$(VAULT_TOKEN="${client_token}" vault read -address="${vault_address}" -non-interactive -format=json nomad/creds/perf)
              then
                echo "${nomad_token_json}" | jq -r .data.secret_id
              else
                fatal "Unable to fetch Nomad token from Vault"
              fi
            ;;
            pg-user )
              if ! wb_nomad vault "${entity}" enabled
              then
                wb_nomad vault "${entity}" login
              fi
              local client_token
              client_token=$(jq -r '.auth.client_token' "${login_file}")
              VAULT_TOKEN="${client_token}" vault kv get \
                --address="${vault_address}"             \
                -non-interactive                         \
                -format=json                             \
                kv/postgrest/perf                        \
              | jq -r .data.data.postgrestDbUser
            ;;
            pg-pass )
              if ! wb_nomad vault "${entity}" enabled
              then
                wb_nomad vault "${entity}" login
              fi
              local client_token
              client_token=$(jq -r '.auth.client_token' "${login_file}")
              VAULT_TOKEN="${client_token}" vault kv get \
                --address="${vault_address}"             \
                -non-interactive                         \
                -format=json                             \
                kv/postgrest/perf                        \
              | jq -r .data.data.postgrestDbPass
            ;;
####### vault -> ci -> * )######################################################
            * )
              usage_nomad
            ;;
          esac
        ;;
####### vault -> world )########################################################
        world )
          mkdir -p "${vault_dir}"/world
          local login_file="${vault_dir}"/world/login.json
          local vault_address="https://vault.world.dev.cardano.org"
          local action=${1:?$usage}; shift
          case "${action}" in
            login )
              msg "First create and copy your GitHub token by doing: "
              msg "Your profile -> Settings -> Developer Settings -> Tokens (Classic) -> Generate New Token (Classic)"
              msg "and create a new token with only the \"read:org\" permission."
              read -p "Hit enter to continue ..."
              vault login                             \
                -address="${vault_address}"           \
                -method=github -path=github-employees \
                -no-store -format=json                \
              > "${login_file}"
            ;;
            enabled )
              if test -f "${login_file}"
              then
                local client_token
                client_token=$(jq -r '.auth.client_token' "${login_file}")
                local token_lookup_response
                if token_lookup_response=$(VAULT_TOKEN="${client_token}" vault token lookup -address="${vault_address}" -namespace=perf -format=json)
                then
                  # TODO: I need to check the expiration time?
                  # echo "${token_lookup_response}" | jq -r .data.expire_time
                  # 2023-02-19T13:07:26.125306646Z
                  true
                else
                  fatal "Are you logged in to Vault? Call 'wb nomad vault ${entity} login' with your IOHK GitHub token (classic)"
                fi
              else
                false
              fi
            ;;
            nomad-token )
              if ! wb_nomad vault "${entity}" enabled
              then
                wb_nomad vault "${entity}" login
              fi
              local client_token
              client_token=$(jq -r '.auth.client_token' "${login_file}")
              local nomad_token_json
              if nomad_token_json=$(VAULT_TOKEN="${client_token}" vault read -address="${vault_address}" -non-interactive -format=json nomad/creds/perf)
              then
                echo "${nomad_token_json}" | jq -r .data.secret_id
              else
                fatal "Unable to fetch Nomad token from Vault"
              fi
            ;;
            aws-s3-credentials )
              if ! wb_nomad vault "${entity}" enabled
              then
                wb_nomad vault "${entity}" login
              fi
              local client_token
              client_token=$(jq -r '.auth.client_token' "${login_file}")
              VAULT_TOKEN="${client_token}" vault read \
                --address="${vault_address}"           \
                -format=json                           \
                aws/creds/perf
            ;;
####### vault -> world -> * )###################################################
            * )
              usage_nomad
            ;;
          esac
        ;;
####### vault -> * )############################################################
        * )
          usage_nomad
        ;;
      esac
    ;;
################################################################################
### ssh ) ######################################################################
################################################################################
    ssh )
      local usage="USAGE: wb nomad ${op} key"
      local ssh_dir="$(wb_nomad dir-path ssh)"
      local subop=${1:?$usage}; shift
      case "${subop}" in
####### ssh -> key )############################################################
        key )
          local key=${1:?$usage}; shift
          case "${key}" in
####### ssh -> key -> server )##################################################
            server )
              local key_path="${ssh_dir}"/server.id_ed25519
              if ! test -f "${key_path}"
              then
                ssh-keygen -t ed25519 -f "${key_path}" -C "" -N "" >/dev/null
              fi
              echo "${key_path}"
            ;;
####### ssh -> key -> user )####################################################
            user )
              local key_path="${ssh_dir}"/user.id_ed25519
              if ! test -f "${key_path}"
              then
                ssh-keygen -t ed25519 -f "${key_path}" -C "" -N "" >/dev/null
              fi
              echo "${key_path}"
            ;;
####### ssh -> key -> * )#######################################################
            * )
              usage_nomad
            ;;
          esac
        ;;
####### ssh -> config )####################################################
        config )
          local file_path="${ssh_dir}"/config
          if ! test -f "${file_path}"
          then
cat > "${file_path}" << EOL
Host *
  StrictHostKeyChecking    accept-new
  GlobalKnownHostsFile     $(wb nomad ssh known_hosts)
  UserKnownHostsFile       $(wb nomad ssh known_hosts)
  PasswordAuthentication   no
  PubKeyAuthentication     yes
  PreferredAuthentications publickey
  IdentitiesOnly           yes
  IdentityFile             $(wb nomad ssh key user)
  Compression              yes
  TCPKeepAlive             no
  ServerAliveInterval      15
  ServerAliveCountMax      4
  ControlMaster            auto
  ControlPath              ${ssh_dir}/%h-%p-%r
  ControlPersist           15
EOL
          fi
          echo "${file_path}"
        ;;
####### ssh -> known_hosts )####################################################
        known_hosts )
          local file_path="${ssh_dir}"/known_hosts
          if ! test -f "${file_path}"
          then
            touch "${file_path}"
          fi
          echo "${file_path}"
        ;;
####### ssh -> * )##############################################################
        * )
          usage_nomad
        ;;
      esac
    ;;
################################################################################
### nodes ) ####################################################################
################################################################################
    nodes )
      local usage="USAGE: wb nomad ${op}"
      # Fetch the status of all nodes that are in the "ready" state.
      # If a node is removed status is "down" and will still show its details.
      # Not using cardano specific filters anymore (-filter 'NodeClass=="perf"').
      local perf_nodes
      perf_nodes="$(nomad node status -filter 'Status=="ready"' -json)"
      # Create the base JSON string but without the "attributes" because those
      # are only available when fetching the status of individual nodes.
      local nodes_json
      nodes_json="$( \
          echo "${perf_nodes}" \
        | \
          jq \
            "map( {                                   \
                \"id\":         .ID                   \
              , \"name\":       .Name                 \
              , \"datacenter\": .Datacenter           \
              , \"ip\":         .Address              \
              , \"attributes\":  null                 \
            } )"                                      \
      )"
      # For each node
      local nodes_ids
      nodes_ids="$( \
          echo "${nodes_json}" \
        | \
          jq -S -r "map(.id) | sort | join (\" \")" \
      )"
      for node_id in ${nodes_ids[*]}
      do
        # Fetch the attributes
        local node_attributes
        node_attributes="$(nomad node status -json "${node_id}" | jq .Attributes)"
        # Add the attributes of this node to the JSON string
        nodes_json="$( \
            echo "${nodes_json}" \
          | \
            jq --argjson attrs "${node_attributes}" "                          \
                .[]                                                            \
              |= (                                                             \
                if ( .id == \"${node_id}\" )                                   \
                then (                                                         \
                    .attributes                                                \
                  |=                                                           \
                    {                                                          \
                      \"cpu\": {                                               \
                         \"arch\": \$attrs[\"cpu.arch\"]                       \
                       , \"frequency\": \$attrs[\"cpu.frequency\"]             \
                       , \"modelname\": \$attrs[\"cpu.modelname\"]             \
                       , \"numcores\": \$attrs[\"cpu.numcores\"]               \
                       , \"reservablecores\": \$attrs[\"cpu.reservablecores\"] \
                       , \"totalcompute\": \$attrs[\"cpu.totalcompute\"]       \
                      }                                                        \
                    , \"kernel\": {                                            \
                         \"arch\": \$attrs[\"kernel.arch\"]                    \
                       , \"name\": \$attrs[\"kernel.name\"]                    \
                       , \"version\": \$attrs[\"kernel.version\"]              \
                      }                                                        \
                    , \"memory\": {                                            \
                         \"totalbytes\": \$attrs[\"memory.totalbytes\"]        \
                      }                                                        \
                    , \"os\": {                                                \
                         \"name\": \$attrs[\"os.name\"]                        \
                       , \"version\": \$attrs[\"os.version\"]                  \
                      }                                                        \
                    , \"platform\": {                                          \
                        \"aws\": {                                             \
                            \"ami-id\": \$attrs[\"platform.aws.ami-id\"]       \
                          , \"instance-type\": \$attrs[\"platform.aws.instance-type\"] \
                          , \"placement\": {                                   \
                              \"availability-zone\": \$attrs[\"platform.aws.placement.availability-zone\"] \
                          }                                                    \
                        }                                                      \
                      }                                                        \
                    , \"unique\": {                                            \
                        \"hostname\": \$attrs[\"unique.hostname\"]             \
                      , \"network\": {                                         \
                            \"ip-address\": \$attrs[\"unique.network.ip-address\"] \
                        }                                                      \
                      , \"platform\": {                                        \
                            \"aws\": {                                         \
                                \"hostname\":        \$attrs[\"unique.platform.aws.hostname\"       ] \
                              , \"instance-id\":     \$attrs[\"unique.platform.aws.instance-id\"    ] \
                              , \"local-hostname\":  \$attrs[\"unique.platform.aws.local-hostname\" ] \
                              , \"local-ipv4\":      \$attrs[\"unique.platform.aws.local-ipv4\"     ] \
                              , \"mac\":             \$attrs[\"unique.platform.aws.mac\"            ] \
                              , \"public-hostname\": \$attrs[\"unique.platform.aws.public-hostname\"] \
                              , \"public-ipv4\":     \$attrs[\"unique.platform.aws.public-ipv4\"    ] \
                            }                                                  \
                        }                                                      \
                      }                                                        \
                    }                                                          \
                ) else (                                                       \
                  .                                                            \
                ) end                                                          \
              )                                                                \
            " \
        )"
      done
      # Output the JSON string ordered by Nomad Client "id"
      echo "${nodes_json}" | jq '. | sort_by(.id)'
    ;;
################################################################################
### agents ) ###################################################################
################################################################################
    agents )
      ### Start/stop server and client
      ################################
      # The Nomad agent is a long running process which runs on every machine
      # that is part of the Nomad cluster. The behavior of the agent depends
      # on if it is running in client or server mode. Clients are responsible
      # for running tasks, while servers are responsible for managing the
      # cluster.
      local usage="USAGE: wb nomad ${op} start|stop"
      local subop=${1:?$usage}; shift
      case "${subop}" in
####### agents -> start )#######################################################
        start )
          local usage="USAGE:wb nomad ${op} ${subop} SERVER-NAME CLIENT-NAME DRIVER-NAME"
          local server_name=${1:?$usage}; shift
          local client_name=${1:?$usage}; shift
          local task_driver=${1:?$usage}; shift
          # Create config files for the server and start it.
          if ! wb_nomad server configure "${server_name}" 4646 4647 4648
          then
            fatal "Failed to configure Nomad server \"${server_name}\""
          fi
          if ! wb_nomad server start "${server_name}"
          then
            fatal "Failed to start Nomad server \"${server_name}\""
          fi
          # Set up the podman driver and start it if it's needed.
          if test "${task_driver}" = "podman"
          then
            # Create config files for the client and the Podman plugin/task driver.
            wb_nomad plugin nomad-driver-podman start
          fi
          # Create config files for the client and start it.
          # WARNING: Actually the client is configured to connect to all the
          # running servers, so if there are no servers ready the Nomad
          # cluster state is uknown (at least to me with the actual config).
          if ! wb_nomad client configure "${client_name}" 14646 14647 14648 "${task_driver}"
          then
            wb_nomad server stop "${server_name}" || true
            fatal "Failed to configure Nomad client \"${client_name}\""
          fi
          # Only the exec driver must be run as root.
          if test "${task_driver}" = "exec"
          then
            # Pass the "root prefix" (command prefix)
            if ! wb_nomad client start "${client_name}" "sudo "
            then
              wb_nomad server stop "${server_name}" || true
              fatal "Failed to start Nomad agents"
            fi
          else
            if ! wb_nomad client start "${client_name}"
            then
              wb_nomad plugin nomad-driver-podman stop || true
              wb_nomad server stop "${server_name}" || true
              fatal "Failed to start Nomad agents"
            fi
          fi
        ;;
####### agents -> stop )########################################################
        stop )
          local usage="USAGE:wb nomad ${op} ${subop} SERVER-NAME CLIENT-NAME DRIVER-NAME"
          local server_name=${1:?$usage}; shift
          local client_name=${1:?$usage}; shift
          local task_driver=${1:?$usage}; shift
          # Collect garbage to avoid orphaned mounts
          # https://support.hashicorp.com/hc/en-us/articles/360000654467-Removing-Orphaned-Mounts-from-Nomad-Allocation-Directory
          nomad system gc 2>&1 >/dev/null || true
          # Stop client
            wb_nomad client stop "${client_name}" \
          ||                                      \
            msg "$(red "Failed to stop Nomad client \"${client_name}\"")"
          # Stop driver(s)
          if test "${task_driver}" = "podman"
          then
              wb_nomad plugin nomad-driver-podman stop \
            ||                                         \
              msg "$(red "Failed to stop nomad-driver-podman")"
          fi
          # Stop server
            wb_nomad server stop "${server_name}" \
          ||                                      \
            msg "$(red "Failed to stop Nomad server \"${server_name}\"")"
        ;;
####### agents -> * )###########################################################
        * )
          usage_nomad
        ;;
      esac # agents
    ;;
################################################################################
### server ) ###################################################################
################################################################################
    server )
      local usage="USAGE: wb nomad ${op} state-dir-path|config-file-path|configure|port|pid-filepath|pids-array|is-running|start|stop"
      local subop=${1:?$usage}; shift
      case "${subop}" in
####### server -> state-dir-path )##############################################
        state-dir-path )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAME"
          local name=${1:?$usage}; shift
          local nomad_servers_dir="$(wb_nomad dir-path server)"
          echo "${nomad_servers_dir}"/"${name}"
        ;;
####### server -> config-file-path )############################################
        config-file-path )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAME"
          local name=${1:?$usage}; shift
          local state_dir=$(wb_nomad server state-dir-path "${name}")
          echo "${state_dir}"/config/nomad.hcl
        ;;
####### server -> configure )###################################################
        configure )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAME HTTP-PORT RPC-PORT SERV-PORT"
          local name=${1:?$usage}; shift
          # Ports
          local http_port=${1:?$usage}; shift
          local rpc_port=${1:?$usage}; shift
          local serv_port=${1:?$usage}; shift
          # Assume the presence of the PID file means "running" because it
          # can represent an abnormal exit / uknown state!
          if wb_nomad server is-running "${name}"
          then
            msg "$(red "FATAL: Nomad server \"${name}\" is already running or in an unknown state, call 'wb nomad server stop ${name}' or 'wb nomad nuke' first")"
            return 1
          else
            local state_dir=$(wb_nomad server state-dir-path "${name}")
            # Delete previous state if any.
            rm -rf "${state_dir}" >/dev/null 2>&1
            # Needed folders:
            mkdir -p "${state_dir}"/config
            mkdir -p "${state_dir}"/data/server
            # Store the ports config
            echo "{\"http\": ${http_port}, \"rpc\": ${rpc_port}, \"serv\": ${serv_port}}" > "${state_dir}"/ports.json
            # Configure
            nomad_create_server_config "${name}" \
              "${http_port}" "${rpc_port}" "${serv_port}"
          fi
        ;;
####### server -> port )########################################################
        port )
          local usage="USAGE: wb nomad ${op} ${subop} (http|rcp|serv) SERVER-NAME"
          local port=${1:?$usage}; shift
          local name=${1:?$usage}; shift
          local state_dir=$(wb_nomad server state-dir-path "${name}")
          local ports_file="${state_dir}"/ports.json
          case "$port" in
            http )
              jq .http "${ports_file}"
            ;;
            rpc )
              jq .rpc "${ports_file}"
            ;;
            serv )
              jq .serv "${ports_file}"
            ;;
            * )
              false
            ;;
          esac
        ;;
####### server -> pid-filepath )################################################
        pid-filepath )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAMENAME"
          local name=${1:?$usage}; shift
          local state_dir=$(wb_nomad server state-dir-path "${name}")
          # Look up PID by Nomad server name
          echo "${state_dir}"/nomad.pid
        ;;
####### server -> pids-array )##################################################
        pids-array )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAME"
          local name=${1:?$usage}; shift
          local config_file=$(wb_nomad server config-file-path "${name}")
          # Make it Mac compatible by only using shorthand options:
          # `-d` instead of `--delimiter` and `-f` instead of `--full`
          pgrep -d ' ' -f "nomad.*${config_file}.*"
          # Clean up is only done by the `stop` subcommand!
          # No `rm "${pid_file}"` if not running.
        ;;
####### server -> is-running )##################################################
        is-running )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAMENAME"
          local name=${1:?$usage}; shift
          local pid_file=$(wb_nomad server pid-filepath "${name}")
          local config_file=$(wb_nomad server config-file-path "${name}")
          # It's running if we haven't PROPERLY stopped it or PIDs exist!
          # `pgrep` piped to `wc -l` instead "--count" to make it Mac comptible
          # Also only shorthand options: like `-f` instead of `--full`
          test -f "${pid_file}" || test $(pgrep -f "nomad.*${config_file}.*" | wc -l) -gt 0
        ;;
####### server -> start )#######################################################
        start )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAMENAME"
          local name=${1:?$usage}; shift
          local state_dir=$(wb_nomad server state-dir-path "${name}")
          # Checks
          if wb_nomad server is-running "${name}"
          then
            msg "$(red "FATAL: Nomad server \"${name}\" is already running or in an unknown state, call 'wb nomad server stop ${name}' or 'wb nomad nuke' first")"
            return 1
          fi
          # Start `nomad` server".
          msg "$(blue Starting) Nomad $(yellow "server \"${name}\"") ..."
          local config_file=$(wb_nomad server config-file-path "${name}")
          local pid_file=$(wb_nomad server pid-filepath "${name}")
          local pid_number
          # Start the agent on a new session (`setsid`) so it does not receive
          # CTRL+C. It must be properly ended by the workbench's exit trap.
          setsid nomad agent          \
            -config="${config_file}"  \
            >> "${state_dir}"/stdout  \
            2>> "${state_dir}"/stderr \
            &
          pid_number="$!"
          echo "${pid_number}" > "${pid_file}"
          msg "$(green "Nomad server \"${name}\" started with PID ${pid_number}")"
          # Even if Nomad server was already running, try to connect to it!
          local i=0 patience=25
          local http_port=$(wb_nomad server port http "${name}")
          msg "$(blue Waiting) for the listening HTTP server (${patience}s) ..."
          until curl -Isf 127.0.0.1:"${http_port}" 2>&1 | head --lines=1 | grep --quiet "HTTP/1.1"
          do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge ${patience}
            then echo
              # Not using `fatal` here, let the caller decide!
              msg "$(red "FATAL: Nomad server startup did not succeed")"
              msg "$(yellow "port \"127.0.0.1:${http_port}\" not ready")"
              msg "$(yellow "Check logs (${state_dir})")"
              # Let the "stop" subcommand clean everything!
              wb_nomad server stop "${name}" || true
              return 1
            fi
            echo -ne "\b\b\b"
          done >&2
        ;;
####### server -> stop )########################################################
        stop )
          # We don't check first if the PID file exists!
          # Also clean up, so here do not assume that Nomad is running!
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAME"
          local name=${1:?$usage}; shift
          # Stop Nomad server by name
          local pids=$(wb_nomad server pids-array "${name}")
          if test -z "${pids}"
          then
            msg "$(red "Found no running Nomad server process to stop, manually clean possible remaining processes")"
            return 1
          else
            for pid_number in ${pids[@]}
            do
              msg "$(blue Stopping) Nomad $(yellow "server \"${name}\"") process PID ${pid_number} ..."
              if ! kill -SIGINT "${pid_number}" >/dev/null 2>&1
              then
                msg "$(red "Killing PID ${pid_number} failed")"
              else
                # Wait 15 seconds for the process to fully exit or kill it.
                msg "$(blue Wait) up to 15 seconds for PID ${pid_number} to exit"
                timeout 15 tail --pid="${pid_number}" -f /dev/null || true
                if kill -0 "${pid_number}" >/dev/null 2>&1
                then
                  msg "$(yellow "Timeout killing PID ${pid_number}, trying SIGKILL")"
                  kill -SIGKILL "${pid_number}" >/dev/null 2>&1 || true
                fi
              fi
            done
            # Remove PID file if all processes were killed!
            if test -z "$(wb_nomad server pids-array "${name}")"
            then
              local pid_file=$(wb_nomad server pid-filepath "${name}")
              if test -f "${pid_file}"
              then
                rm "${pid_file}"
              fi
            fi
          fi
        ;;
####### server -> cleanup )#####################################################
        cleanup )
          local usage="USAGE: wb nomad ${op} ${subop} SERVER-NAMENAME"
          local name=${1:?$usage}; shift
          if wb_nomad server is-running "${name}"
          then
            msg "Won't cleanup the running server \"${name}\""
          else
            local state_dir=$(wb_nomad server state-dir-path "${name}")
            rm -rf "${state_dir}" >/dev/null 2>&1 || true
          fi
        ;;
####### server -> * )###########################################################
        * )
          usage_nomad
        ;;
      esac # server
    ;;
################################################################################
### client ) ###################################################################
################################################################################
    client )
      local usage="USAGE: wb nomad ${op} state-dir-path|config-file-path|configure|port|pid-filepath|pids-array|is-running|start|stop"
      local subop=${1:?$usage}; shift
      case "${subop}" in
####### client -> state-dir-path )##############################################
        state-dir-path )
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAME"
          local name=${1:?$usage}; shift
          local nomad_clients_dir="$(wb_nomad dir-path client)"
          echo "${nomad_clients_dir}"/"${name}"
        ;;
####### client -> config-file-path )############################################
        config-file-path )
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAME"
          local name=${1:?$usage}; shift
          local state_dir=$(wb_nomad client state-dir-path "${name}")
          echo "${state_dir}"/config/nomad.hcl
        ;;
####### client -> configure )###################################################
        configure )
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAME HTTP-PORT RPC-PORT SERV-PORT DRIVER-NAME [GENESIS-DIR]"
          local name=${1:?$usage}; shift
          # Ports
          local http_port=${1:?$usage}; shift
          local rpc_port=${1:?$usage}; shift
          local serv_port=${1:?$usage}; shift
          # Unlike the server, the client can have different task drivers!
          local task_driver=${1:?$usage}; shift
          # Checks
          # Assume the presence of the PID file means "running" because it
          # can represent an abnormal exit / uknown state!
          if wb_nomad client is-running "${name}"
          then
            # When reusing, remember to check that client is running with
            # the needed task driver!
            msg "$(red "FATAL: Nomad client \"${name}\" is already running or in an uknown state, call 'wb nomad client stop ${name}' or 'wb nomad nuke' first")"
            return 1
          else
            local state_dir=$(wb_nomad client state-dir-path "${name}")
            # Delete previous state if any.
            rm -rf "${state_dir}" >/dev/null 2>&1
            # Needed folders:
            mkdir -p "${state_dir}"/config
            mkdir -p "${state_dir}"/data/{client,plugins,alloc}
            # Store the ports config
            echo "{\"http\": ${http_port}, \"rpc\": ${rpc_port}, \"serv\": ${serv_port}}" > "${state_dir}"/ports.json
            # Store tast driver parameter
            echo "${task_driver}" > "${state_dir}"/task_driver
            # Task driver specific client configuration
            if test "${task_driver}" = "podman"
            then
              local podman_socket_path=$(wb_nomad plugin nomad-driver-podman socket-path)
              # Podman Task Driver - Client Requirements:
              ## "Ensure that Nomad can find the plugin, refer to `plugin_dir`."
              ### https://www.nomadproject.io/plugins/drivers/podman#client-  requirements
              ## On every call to `wb nomad client configure` the
              ## available `nomad-driver-podman` is replaced.
              # TODO: Somehow move this logic to `wb nomad plugin`
              rm -f "${state_dir}"/data/plugins/nomad-driver-podman
              ln -s -f "$(which nomad-driver-podman)" "${state_dir}"/data/plugins/nomad-driver-podman
              # Create configuration file
              nomad_create_client_config "${name}" \
                "${http_port}" "${rpc_port}" "${serv_port}" \
                "${task_driver}" "${podman_socket_path}"
            else
              # Create configuration file
              nomad_create_client_config "${name}" \
                "${http_port}" "${rpc_port}" "${serv_port}" \
                "${task_driver}"
            fi
          fi
        ;;
####### client -> port )########################################################
        port )
          local usage="USAGE: wb nomad ${op} ${subop} (http|rcp|serv) CLIENT-NAME"
          local port=${1:?$usage}; shift
          local name=${1:?$usage}; shift
          local state_dir=$(wb_nomad client state-dir-path "${name}")
          local ports_file="${state_dir}"/ports.json
          case "$port" in
            http )
              jq .http "${ports_file}"
            ;;
            rpc )
              jq .rpc "${ports_file}"
            ;;
            serv )
              jq .serv "${ports_file}"
            ;;
            * )
              false
            ;;
          esac
        ;;
####### client -> pid-filepath )################################################
        pid-filepath )
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAME"
          local name=${1:?$usage}; shift
          local state_dir=$(wb_nomad client state-dir-path "${name}")
          # Look up PID by Nomad client name
          echo "${state_dir}"/nomad.pid
        ;;
####### client -> pids-array )##################################################
        pids-array )
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAME"
          local name=${1:?$usage}; shift
          local config_file=$(wb_nomad client config-file-path "${name}")
          # Make it Mac compatible by only using shorthand options:
          # `-d` instead of `--delimiter` and `-f` instead of `--full`
          pgrep -d ' ' -f "nomad.*${config_file}.*"
          # Clean up is only done by the `stop` subcommand!
          # No `rm "${pid_file}"` if not running.
        ;;
####### client -> is-running )##################################################
        is-running )
          local usage="USAGE:wb nomad ${op} ${subop} CLIENT-NAME"
          local name=${1:?$usage}; shift
          local pid_file=$(wb_nomad client pid-filepath "${name}")
          local config_file=$(wb_nomad client config-file-path "${name}")
          # It's running if we haven't PROPERLY stopped it or PIDs exist!
          # `pgrep` piped to `wc -l` instead "--count" to make it Mac comptible
          # Also only shorthand options: like `-f` instead of `--full`
          test -f "${pid_file}" || test $(pgrep -f "nomad.*${config_file}.*" | wc -l) -gt 0
        ;;
####### client -> start )#######################################################
        # Agent is started with `-network-interface lo` if not without a proper
        # network connection it fails with the message shown below.
        # (Fix added while on a 13:25 hour long non-stop flight)
        # {
        #   "@level": "error",
        #   "@message": "error starting agent",
        #   "@module": "agent",
        #   "@timestamp": "2023-05-07T15:12:34.631345Z",
        #   "error": "client setup failed: fingerprinting failed: Error while detecting network interface  during fingerprinting: No default interface found"
        # }
        # {
        #   "@level": "error",
        #   "@message": "Error starting agent: client setup failed: fingerprinting failed: Error while detecting network interface  during fingerprinting: No default interface found",
        #   "@module": "agent",
        #   "@timestamp": "2023-05-07T15:12:34.631378Z"
        # }
        # TODO: Look for a default route first? (same logic Nomad uses)
        #       ip route list default | head --lines=1 | cut -d " " -f 3
        #       See also Client config "fingerprint.network.disallow_link_local"
        start )
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAME [ROOT-PREFIX]"
          local name=${1:?$usage}; shift
          # A "root" prefix, like "sudo " (blankspace intended).
          local root_prefix=""
          local state_dir=$(wb_nomad client state-dir-path "${name}")
          if test $# -gt 0
          then
            root_prefix=${1:?$usage}; shift
            if test -n "${root_prefix}"
            then
              echo "${root_prefix}" > "${state_dir}"/root
            fi
          fi
          # Checks
          if wb_nomad client is-running "${name}"
          then
            msg "$(red "FATAL: Nomad client \"${name}\" is already running or in an uknown state, call 'wb nomad client stop ${name}' or 'wb nomad nuke' first")"
            return 1
          fi
          # Start `nomad` client".
          msg "$(blue Starting) Nomad $(yellow "client \"${name}\"") ..."
          local config_file=$(wb_nomad client config-file-path "${name}")
          local pid_file=$(wb_nomad client pid-filepath "${name}")
          local pid_number
          local cmd_array=("${root_prefix}" "bash" "-c")
          # Start the agent on a new session (`setsid`) so it does not receive
          # CTRL+C. It must be properly ended by the workbench's exit trap.
          pid_number=$(${cmd_array[@]} "setsid nomad agent \
            -config="${config_file}"                       \
            -network-interface lo                          \
            >> "${state_dir}"/stdout                       \
            2>> "${state_dir}"/stderr                      \
            & echo \"\$!\"")
          echo "${pid_number}" > "${pid_file}"
          msg "$(green "Nomad client \"${name}\" started with PID ${pid_number}")"
          # Even if Nomad server was already running, try to connect to it!
          local i=0 patience=25
          local http_port=$(wb_nomad client port http "${name}")
          msg "$(blue Waiting) for the listening HTTP server (${patience}s) ..."
          until curl -Isf 127.0.0.1:"${http_port}" 2>&1 | head --lines=1 | grep --quiet "HTTP/1.1"
          do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge ${patience}
            then echo
              # Not using `fatal` here, let the caller decide!
              msg "$(red "FATAL: Nomad client startup did not succeed")"
              msg "$(yellow "port \"127.0.0.1:${http_port}\" not ready")"
              msg "$(yellow "Check logs (${state_dir})")"
              # Let the "stop" subcommand clean everything!
              wb_nomad client stop "${name}" || true
              return 1
            fi
            echo -ne "\b\b\b"
          done >&2
          # Now check that the server and client are connected and the
          # client as eligible
          local i=0 patience=25
          msg "$(blue Waiting) until the Nomad server sees the client (${patience}s) ..."
          until nomad node status -filter "\"workbench-nomad-client-${name}\" in Name" -json | jq -r '.[0].Status' | grep --quiet "^ready"
          do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge ${patience}
            then echo
              tail "${state_dir}"/stderr
              # Not using `fatal` here, let the caller decide!
              msg "$(red "FATAL: Nomad client startup did not succeed")"
              msg "$(yellow "Nomad client not connected to Nomad server")"
              msg "$(yellow "Check logs (${state_dir})")"
              # Let the "stop" subcommand clean everything!
              wb_nomad client stop "${name}" || true
              return 1
            fi
            echo -ne "\b\b\b"
          done >&2
          # TODO: List the known server addresses of the client node.
          # nomad node config -servers
          local client_id=$(nomad node status -filter "\"workbench-nomad-client-cli1\" in Name" -json | jq -r '.[0].ID')
          # TODO: Configure the node?
          # nomad node eligibility -enable "${client_id}"
          # nomad node drain -disable "${client_id}"
          local task_driver=$(cat "${state_dir}"/task_driver)
          if test "${task_driver}" == "exec"
          then
            # Look for "Drivers":{"exec":  {"Detected":true,"Healthy":true}}
            if ! test $(nomad node status -filter "\"workbench-nomad-client-${name}\" in Name" -json | jq '.[0].Drivers.exec.Detected') = "true"
            then
              # Not using `fatal` here, let the caller decide!
              msg "$(red "FATAL: Task driver \"exec\" was not detected")"
              return 1
            fi
            if ! test $(nomad node status -filter "\"workbench-nomad-client-${name}\" in Name" -json | jq '.[0].Drivers.exec.Healthy') = "true"
            then
              # Not using `fatal` here, let the caller decide!
              msg "$(red "FATAL: Task driver \"exec\" is not healthy")"
              return 1
            fi
          else
            # Look for "Drivers":{"podman":{"Detected":true,"Healthy":true}}
            if ! test $(nomad node status -filter "\"workbench-nomad-client-${name}\" in Name" -json | jq '.[0].Drivers.podman.Detected') = "true"
            then
              # Not using `fatal` here, let the caller decide!
              msg "$(red "FATAL: Task driver \"podman\" was not detected")"
              return 1
            fi
            if ! test $(nomad node status -filter "\"workbench-nomad-client-${name}\" in Name" -json | jq '.[0].Drivers.podman.Healthy') = "true"
            then
              # Not using `fatal` here, let the caller decide!
              msg "$(red "FATAL: Task driver \"podman\" is not healthy")"
              return 1
            fi
          fi
          true
          # TODO: Check all the clients connected to the server!
        ;;
####### client -> stop )########################################################
        stop )
          # We don't check first if the PID file exists!
          # Also clean up, so here do not assume that Nomad is running!
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAME"
          local name=${1:?$usage}; shift
          # Look for the "root" flag with the command prefix (like `sudo `)
          local state_dir=$(wb_nomad client state-dir-path "${name}")
          local root_prefix
          if test -e "${state_dir}"/root
          then
            root_prefix=$(cat "${state_dir}"/root)
          else
            root_prefix=""
          fi
          # Stop Nomad client by name
          local pids=$(wb_nomad client pids-array "${name}")
          if test -z "${pids}"
          then
            msg "$(red "Found no running Nomad client process to stop, manually clean possible remaining processes")"
            return 1
          else
            for pid_number in ${pids[@]}
            do
              msg "$(blue Stopping) Nomad $(yellow "client \"${name}\"") process PID ${pid_number} ..."
              local cmd_array=("${root_prefix}" "bash" "-c")
              if ! ${cmd_array[@]} "kill -SIGINT ${pid_number}" >/dev/null 2>&1
              then
                msg "Killing PID ${pid_number} failed"
              else
                # Wait 15 seconds for the process to fully exit or kill it.
                msg "$(blue Wait) up to 30 seconds for PID ${pid_number} to exit"
                timeout 30 tail --pid="${pid_number}" -f /dev/null || true
                local cmd_array=("${root_prefix}" "bash" "-c")
                if ${cmd_array[@]} "kill -0 ${pid_number}" >/dev/null 2>&1
                then
                  msg "$(yellow "Timeout killing PID ${pid_number}, trying SIGKILL")"
                  local cmd_array=("${root_prefix}" "bash" "-c")
                  ${cmd_array[@]} "kill -SIGKILL ${pid_number}" >/dev/null 2>&1 || true
                fi
              fi
            done
            # Remove PID file if all processes were killed!
            if test -z "$(wb_nomad client pids-array "${name}")"
            then
              # WHY? The client is keeping some directories mounted!
              msg "Unmount any folders left by the client"
              local cmd_array=("${root_prefix}" "bash" "-c")
              # Command fails when there's nothing to umount!
              grep "${state_dir}" /proc/mounts | cut -f2 -d" " | sort -r | ${cmd_array[@]} 'xargs -I "{}" umount -n "{}"' || true
              # Now mark as "not running"
              local pid_file=$(wb_nomad client pid-filepath "${name}")
              if test -f "${pid_file}"
              then
                rm "${pid_file}"
              fi
            fi
          fi
        ;;
####### client -> cleanup )#####################################################
        cleanup )
          local usage="USAGE: wb nomad ${op} ${subop} CLIENT-NAMENAME"
          local name=${1:?$usage}; shift
          if wb_nomad client is-running "${name}"
          then
            msg "Won't cleanup the running client \"${name}\""
          else
            local state_dir=$(wb_nomad client state-dir-path "${name}")
            # Look for the "root" flag with the command prefix (like `sudo `)
            local root_prefix
            if test -e "${state_dir}"/root
            then
              root_prefix=$(cat "${state_dir}"/root)
            else
              root_prefix=""
            fi
            local cmd_array=("${root_prefix}" "bash" "-c")
            ${cmd_array[@]} "rm -rf ${state_dir}" >/dev/null 2>&1 || true
          fi
        ;;
        # Client specific subcommands here (not available for servers):
####### client -> * )###########################################################
        * )
          usage_nomad
        ;;
      esac # client
    ;;
################################################################################
### plugin ) ###################################################################
################################################################################
    plugin )
      local usage="USAGE: wb nomad ${op} nomad-driver-podman"
      local plugin=${1:?$usage}; shift
      case "${plugin}" in
####### plugin -> nomad-driver-podman )#########################################
        nomad-driver-podman )
          local usage="USAGE: wb nomad ${op} ${plugin}"
          local subop=${1:?$usage}; shift
          case "$subop" in
########### plugin -> nomad-driver-podman -> socket-path )######################
            socket-path )
              # Socket of the process that connects nomad-driver-podman with podman.
              # Can't reside inside "$dir", can't use a path longer than 108 characters!
              # See: https://man7.org/linux/man-pages/man7/unix.7.html
              # char        sun_path[108];            /* Pathname */
              echo "${XDG_RUNTIME_DIR:-/run/user/$UID}/workbench-podman.sock"
            ;;
########### plugin -> nomad-driver-podman -> pid-filepath )#####################
            pid-filepath )
              local plugin_dir="$(wb_nomad dir-path plugin)"
              echo "${plugin_dir}"/nomad-driver-podman.pid
            ;;
########### plugin -> nomad-driver-podman -> pid )##############################
            pid )
              local pid_file=$(wb_nomad plugin nomad-driver-podman pid-filepath)
              if test -f $pid_file
              then
                local pid_number=$(cat "${pid_file}")
                # Check if the process is running
                if kill -0 "${pid_number}" >/dev/null 2>&1
                then
                  echo "${pid_number}"
                else
                  rm "${pid_file}"
                  false
                fi
              else
                false
              fi
            ;;
########### plugin -> nomad-driver-podman -> is-running )#######################
            is-running )
              wb_nomad plugin nomad-driver-podman pid >/dev/null
            ;;
########### plugin -> nomad-driver-podman -> start )############################
            # Start the `podman` API service needed by `nomad`.
            start ) # TODO: Check that it's not already running!
              msg "Preparing podman API service for nomad driver \`nomad-driver-podman\` ..."
              local podman_socket_path=$(wb_nomad plugin nomad-driver-podman socket-path)
        #      if test -S "$socket"
        #      then
        #          msg "Podman API service was already running"
        #      else
                # The session is kept open waiting for a new connection for 60 seconds.
                # https://discuss.hashicorp.com/t/nomad-podman-rhel8-driver-difficulties/21877/4
                # `--time`: Time until the service session expires in seconds. Use 0
                # to disable the timeout (default 5).
                local pid_file=$(wb_nomad plugin nomad-driver-podman pid-filepath)
                podman system service --time 60 "unix://$podman_socket_path" &
                local pid_number="$!"
                echo "${pid_number}" > "${pid_file}"
                local i=0 patience=5
                while test ! -S "$podman_socket_path"
                do printf "%3d" $i; sleep 1
                  i=$((i+1))
                  if test $i -ge $patience
                  then echo
                      progress "nomad-driver-podman" "$(red FATAL):  workbench:  nomad-driver-podman:  patience ran out after ${patience}s, socket $podman_socket_path"
                      fatal "nomad-driver-podman startup did not succeed:  check logs"
                      rm "${pid_file}"
                  fi
                  echo -ne "\b\b\b"
                done >&2
        #      fi
              msg "Podman API service started"
            ;;
########### plugin -> nomad-driver-podman -> stop )#############################
            stop )
              local pid_number
              local pid_file=$(wb_nomad plugin nomad-driver-podman pid-filepath "${name}")
              # Call without `local` to obtain the subcommand's return code.
              if pid_number=$(wb_nomad plugin nomad-driver-podman pid)
              then
                msg "Killing nomad-driver-podman (PID ${pid_number}) ..."
                if ! kill -SIGINT "${pid_number}"
                then
                  fatal \
                    "Killing nomad-driver-podman failed, \
                    is PID \"${pid_number}\" (${pid_file}) running?"
                else
                  # Wait 15 seconds for the process to fully exit or kill it.
                  if ! timeout 15 tail --pid="${pid_number}" -f /dev/null
                  then
                    kill -SIGKILL "${pid_number}" || true
                  fi
                fi
                # Remove PID file
                rm "${pid_file}"
              else
                msg "nomad-driver-podman API service is not running"
                # If a PID file was already there it's not removed!
                false
              fi
            ;;
########### plugin -> nomad-driver-podman -> * )################################
            * )
              usage_nomad
            ;;
          esac  # plugin -> nomad-driver-podman
        ;;
####### plugin -> * )###########################################################
        * )
          usage_nomad
        ;;
      esac # plugin
    ;;
################################################################################
### rsync ) ####################################################################
################################################################################
    rsync )
      local usage="USAGE: wb nomad ${op} start|stop"
      local subop=${1:?$usage}; shift
      # Nomad actions
      case "${subop}" in
        start )
          local usage="USAGE: wb nomad ${op} ${subop} GENESIS"
          local genesis_dir=${1:?$usage}; shift
          local cache_dir=$(envjqr 'cacheDir')
          # https://www.atlantic.net/vps-hosting/how-to-setup-rsync-daemon-linux-server/
          cat > "${cache_dir}"/rsync.conf <<- EOF
pid file = ${cache_dir}/rsyncd.pid
lock file = ${cache_dir}/rsync.lock
log file = ${cache_dir}/rsync.log
port = 12000

[GENESIS]
path = ${genesis_dir}
comment = RSYNC GENESIS FILES
read only = true
timeout = 300
EOF
        rsync --daemon \
          --address=127.0.0.1 \
          --config="${cache_dir}"/rsync.conf \
          --verbose \
           > "${cache_dir}"/rsyncd.stdout \
          2> "${cache_dir}"/rsyncd.stderr \
          &
        ;;
        stop )
          local usage="USAGE: wb nomad ${op} ${subop}"
          local cache_dir=$(envjqr 'cacheDir')
          local pid=$(cat ${cache_dir}/rsyncd.pid)
          kill -9 "${pid}"
        ;;
        * )
        ;;
      esac
    ;;
################################################################################
### webfs ) ####################################################################
################################################################################
    webfs )
      local usage="USAGE: wb nomad ${op} (start|stop)"
      local subop=${1:?$usage}; shift
      # Nomad actions
      case "${subop}" in
        state-dir-path )
          local webfs_dir="$(wb_nomad dir-path webfs)"
          echo "${webfs_dir}"
        ;;
        document-root-path )
          local state_dir=$(wb_nomad webfs state-dir-path)
          echo "${state_dir}"/document-root
        ;;
        add-genesis-dir )
          local usage="USAGE: wb nomad ${op} ${subop} GENESIS-DIR RUN-TAG"
          local genesis_dir=${1:?$usage}; shift
          local run_tag=${1:?$usage}; shift
          local document_root=$(wb_nomad webfs document-root-path)
          mkdir -p "${document_root}"
          # Don't include "./" files and prefix (as "./genesis.alonzo.json")
          find -L "${genesis_dir}" -type f -printf "%P\n"      \
            | tar --create --zstd                              \
              --dereference --hard-dereference                 \
              --file="${document_root}"/"${run_tag}".tar.zst   \
              --owner=65534 --group=65534 --mode="u=rwx"       \
              --directory="${genesis_dir}" --files-from=-
          # And remember the correct permissions when extracting:
          #> VRF private key file at: ../genesis/node-keys/node-vrf0.skey has
          #  "other" file permissions. Please remove all "other" file permissions.
          #> VRF private key file at: ../genesis/node-keys/node-vrf0.skey has
          #  "group" file permissions. Please remove all "group" file permissions.
          true
        ;;
        pid-filepath )
          local usage="USAGE: wb nomad ${op} ${subop}"
          local state_dir=$(wb_nomad webfs state-dir-path)
          echo "${state_dir}"/webfsd.pid
        ;;
        pids-array )
          local usage="USAGE: wb nomad ${op} ${subop}"
          local state_dir=$(wb_nomad webfs state-dir-path)
          # Make it Mac compatible by only using shorthand options:
          # `-d` instead of `--delimiter` and `-f` instead of `--full`
          pgrep -d ' ' -f "webfsd.*${state_dir}"/webfsd.log
          # Clean up is only done by the `stop` subcommand!
          # No `rm "${pid_file}"` if not running.
        ;;
        is-running )
          local usage="USAGE: wb nomad ${op} ${subop}"
          local pid_file=$(wb_nomad webfs pid-filepath)
          local state_dir=$(wb_nomad webfs state-dir-path)
          # It's running if we haven't PROPERLY stopped it or PIDs exist!
          # `pgrep` piped to `wc -l` instead "--count" to make it Mac comptible
          # Also only shorthand options: like `-f` instead of `--full`
          test -f "${pid_file}" && test $(pgrep -f "webfsd.*${state_dir}"/webfsd.log | wc -l) -gt 0
        ;;
        start )
          local usage="USAGE: wb nomad ${op} ${subop}"
          local state_dir=$(wb_nomad webfs state-dir-path)
          local document_root=$(wb_nomad webfs document-root-path)
          local pid_file=$(wb_nomad webfs pid-filepath)
          mkdir -p "${document_root}"
          msg "Starting HTTP server ..."
          # Binding to 127.0.0.1 because it's only used for local runs
          webfsd                            \
            -4 -p 12000 -i 127.0.0.1        \
            -r "${document_root}"           \
            -l "${state_dir}"/webfsd.log    \
             > "${state_dir}"/webfsd.stdout \
            2> "${state_dir}"/webfsd.stderr \
            &
            local pid_number="$!"
            echo "${pid_number}" > "${pid_file}"
        ;;
        stop )
          local usage="USAGE: wb nomad ${op} ${subop}"
          local state_dir=$(wb_nomad webfs state-dir-path)
          local pid_file=$(wb_nomad webfs pid-filepath)
          msg "Stopping HTTP server ..."
          pkill --signal SIGKILL --full "${state_dir}"/webfsd.log >/dev/null 2>&1 || true
          rm "${pid_file}"
        ;;
        * )
        ;;
      esac
    ;;
################################################################################
### nuke ) ######################################################################
################################################################################
    nuke )
      local nomad_servers_dir="$(wb_nomad dir-path server)"
      local nomad_clients_dir="$(wb_nomad dir-path client)"
      # Nuke all Nomad clients
      for client_name in $(ls "${nomad_clients_dir}"); do
        msg "Config folder of Nomad client \"${client_name}\" found"
        if wb_nomad client is-running "${client_name}"
        then
          msg "Nomad client \"${client_name}\" is running"
          if wb_nomad client stop "${client_name}"
          then
            # Only call cleanup if stop did not fail
            wb_nomad client cleanup "${client_name}"
          else
            msg "Failed to stop Nomad client \"${client_name}\", now in unknown state, manual cleanup of processes needed"
          fi
        else
          msg "Nomad client \"${client_name}\" is not running"
        fi
        # Nuke the client's dir
        local state_dir=$(wb_nomad client state-dir-path "${client_name}")
        msg "Removing \"${state_dir}\" ..."
        local root_prefix
        if test -e "${state_dir}"/root
        then
          root_prefix=$(cat "${state_dir}"/root)
        else
          root_prefix=""
        fi
        local cmd_array=("${root_prefix}" "bash" "-c")
        if ! ${cmd_array[@]} "rm -rf ${state_dir}" >/dev/null 2>&1
        then
          msg "Failed to remove config folder of Nomad client \"${client_name}\", now in unknown state, manual cleanup needed"
        fi
      done
      # Nuke the nomad-driver-podman plugin
      if wb_nomad plugin nomad-driver-podman is-running
      then
        wb_nomad plugin nomad-driver-podman stop
      fi
      local podman_socket_path=$(wb_nomad plugin nomad-driver-podman socket-path)
      if test -S "${podman_socket_path}"
      then
        rm "${podman_socket_path}"
      fi
      # Nuke all Nomad servers
      for server_name in $(ls "${nomad_servers_dir}"); do
        msg "Config folder of Nomad server \"${server_name}\" found"
        if wb_nomad server is-running "${server_name}"
        then
          msg "Nomad server \"${server_name}\" is running"
          if wb_nomad server stop "${server_name}"
          then
            # Only call cleanup if stop did not fail
            wb_nomad server cleanup "${server_name}"
          else
            msg "Failed to stop Nomad server \"${server_name}\", now in unknown state, manual cleanup of processes needed"
          fi
        else
          msg "Nomad server \"${server_name}\" is not running"
        fi
        # Nuke the server's dir
        local state_dir=$(wb_nomad server state-dir-path "${server_name}")
        msg "Removing \"${state_dir}\" ..."
        if ! rm -rf "${state_dir}" >/dev/null 2>&1
        then
          msg "Failed to remove config folder of Nomad server \"${server_name}\", now in unknown state, manual cleanup needed"
        fi
      done
      # Nuke the Nomad Agents' .cache dir
      # Keep top level Nomad cache dir because it includes Vault's dirs.
      rm -rf "${nomad_clients_dir}" >/dev/null 2>&1
      rm -rf "${nomad_servers_dir}" >/dev/null 2>&1
      # Bye HTTP server
      if wb_nomad webfs is-running
      then
        wb_nomad webfs stop
      fi
      rm -rf "$(wb_nomad webfs state-dir-path)"
      # TODO: podman ?
      # rm -rf ~/.local/share/containers/cache/
      # rm -rf ~/.local/share/containers/storage/
      # rm -rf ~/.config/containers/podman/
    ;;
################################################################################
### job ) ######################################################################
################################################################################
    job )
      local usage="USAGE: wb nomad ${op} start|check-allocs|stop"
      local subop=${1:?$usage}; shift
      case "${subop}" in
####### job -> start )##########################################################
        start )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          # Post a Nomad job without "monitor" (`-detach`) mode!
          # I don't want to have a `nomad` process attached to my terminal,
          # funny things are happening with the workbench's log output!
          ### -detach
          ### Return immediately instead of entering monitor mode. After job
          ### submission, the evaluation ID will be printed to the screen,
          ### which can be used to examine the evaluation using the eval
          ### status command.
          local job_run_output
          if ! job_run_output=$(nomad job run -detach "${job_file}")
          then
            msg "$(red "FATAL: Failed to post job (\"${job_file}\") to Nomad server")"
            msg "$(yellow "Try \`wb nomad nuke\` if not using cloud Nomad")"
            return 1
          fi
          # Grab the "evaluation" ID from stdout and start monitoring.
          local initial_eval_id=$(echo "${job_run_output}" | grep "^Evaluation ID:" | cut -d ':' -f 2 | tr -d ' ')
          if ! nomad eval status -json "${initial_eval_id}" >/dev/null
          then
            msg "$(red "FATAL: Failed to fetch job's default evaluation status")"
            msg "$(yellow "Trying to stop the job just in case ...")"
            nomad job stop "${job_name}" >/dev/null || true
            return 1
          fi
          if ! wb_nomad job monitor "${job_file}" "${job_name}"
          then
            msg "$(red "FATAL: \"wb nomad job run\" failed!")"
            msg "$(yellow "Trying to stop the job just in case ...")"
            nomad job stop "${job_name}" >/dev/null || true
            return 1
          fi
        ;;
####### job -> monitor )########################################################
        monitor )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME INITIAL-EVAL-ID"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          # Creates a folder to store every response!
          mkdir "${job_file}".run
          # Monitoring flow:
          # IF NOT
          #       (
          #         -> InitialEvaluationID
          #              ("concurrently")
          #         -> DefaultDeploymentID  (for the evaluation deployment)
          #              ("concurrently")
          #         -> JobAllocations       (for all job allocations)
          #       )
          #       OR
          #       (
          #         -> JobEvaluations       (for all job evaluations)
          #              ("concurrently")
          #         -> JobDeployments       (fro all job evaluations)
          #              ("concurrently")
          #         -> JobAllocations       (for all job allocations)
          #       )
          # THEN
          #    FAIL
          # Some docs on the scheduling flow:
          # https://developer.hashicorp.com/nomad/docs/concepts/scheduling/scheduling
          # Notes:
          # 1) In "misterious" cases a new evaluation ID is given later
          #    and the initial one is forgotten!
          #    For example if there are placement errors, like when the
          #    requested task driver is not available, the initial
          #    deployment stays "running" and new evaluation IDs can be
          #    found whith the following message:
          #    "StatusDescription": "created to place remaining allocations"
          #    This is weird/unintuitive to me!
          # 2) An evaluation can be marked as "complete" in the `-json`
          #    response but show "Placement Failures" in the stdout
          #    of the status command.
          # 3) A deployment can stay running while the allocation was
          #    flagged as "dead". For example when an artifact can't be
          #    downloaded.
          local jobs_array=()
          wb_nomad job monitor-job-evals        \
            "${job_file}" "${job_name}" "false" \
            &
          jobs_array+=("$!")
          wb_nomad job monitor-job-allocs       \
            "${job_file}" "${job_name}" "false" \
            &
          jobs_array+=("$!")
          # Wait for all processes to finish or kill them if at least one fails!
          wait_kill_em_all "${jobs_array[@]}" || touch "${job_file}.run/job.error"
          # Check for every possible error
          local return_code=0
          # Any failed evaluation(s)?
          if test -f "${job_file}.run/evaluations.error"
          then
            return_code=1
            msg "$(red "FATAL: One or more Nomad Evaluations failed!")"
            # Due to race conditions the *.error file may not be preset.
            msg \
              "$(yellow \
                "See logs on: $( \
                    ls "${job_file}.run/evaluation.*.error.json" 2>/dev/null \
                  || \
                    echo "${job_file}.run/evaluation.*.error.json" \
                )"\
              )"
          fi
          # Any failed allocation(s)?
          if test -f "${job_file}.run/allocations.error"
          then
            return_code=1
            msg "$(red "FATAL: One or more Nomad Allocations failed!")"
            # Due to race conditions the *.error file may not be preset.
            msg \
              "$(yellow \
                "See logs on: $( \
                    ls "${job_file}.run/allocation.*.error.json" 2>/dev/null \
                  || \
                    echo "${job_file}.run/allocation.*.error.json" \
                )"\
              )"
          fi
          # Any failed allocations' task(s)?
          if test -f "${job_file}.run/tasks.error"
          then
            return_code=1
            msg "$(red "FATAL: One or more Nomad Allocation's Tasks failed!")"
            # Due to race conditions the *.error file may not be preset.
            msg \
              "$(yellow \
                "See logs on: $( \
                    ls "${job_file}.run/task.*.error.json" 2>/dev/null \
                  || \
                    echo "${job_file}.run/task.*.error.json" \
                )"\
              )"
          fi
          # Any other generic error?
          if test -f "${job_file}.run/job.error"
          then
            return_code=1
            msg "$(red "FATAL: Nomad Job startup failed!")"
          fi
          return "${return_code}"
        ;;
####### job -> monitor-job-evals )##############################################
        monitor-job-evals )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME MSGOFF"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          local msgoff=${1:?$usage};   shift
          # Fetch the evaluations IDs and monitor them.
          local nomad_command_stdout # Don't masquerade errors behind `local`
          if ! nomad_command_stdout=$(nomad eval list -json -job "${job_name}")
          then
            "${msgoff}" || msg "$(red "FATAL: Command \"nomad eval list\" failed")"
            # Error messages surely shown on stderr and this variable is empty
            if test -n "${nomad_command_stdout}"
            then
              "${msgoff}" || msg "${nomad_command_stdout}"
            fi
            # Send fatal job error signal after printing this error's messages!
            touch "${job_file}.run/job.error"
            return 1
          fi
          # IDs Bash array with the command result if any (jq query using "?")
          local ids_array # Don't masquerade errors behind `local`
          ids_array=($(echo "${nomad_command_stdout}" | jq --raw-output "map(.ID)? | join (\" \")"))
          # If no evaluations start all over again
          if test -z "${ids_array:-}" # If = () "unbound variable" error
          then
            # But stop if any other cluster failure happens
            if test -f "${job_file}.run/job.error"
            then
              # Monitor was not started, IDs not known, no messages to show
              return 1
            else
              sleep 1
              # Iterate and exit with the return code of the new call
              wb_nomad job monitor-job-evals            \
                "${job_file}" "${job_name}" "${msgoff}"
            fi
          else
            # Iterate through evaluations
            local jobs_array=()
            "${msgoff}" || msg "Entering monitor of Nomad $(yellow "Evaluation(s) [${ids_array[@]}]")"
            for eval_id in ${ids_array[*]}
            do
              # Create file "evaluations.error" is any evaluation fails
                  wb_nomad job monitor-eval-id               \
                    "${job_file}" "${job_name}" "${eval_id}" \
                    "${msgoff}"                              \
                ||                                           \
                  touch "${job_file}.run/evaluations.error"  \
              &
              jobs_array+=("$!")
            done
            # Wait for all processes to finish or kill them if at least one fails!
            if ! wait_kill_em_all "${jobs_array[@]}" || test -f "${job_file}.run/evaluations.error"
            then
              "${msgoff}" || msg "$(red "Exiting monitor of Nomad Evaluation(s) [${ids_array[@]}] due to errors")"
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            else
              touch "${job_file}.run/evaluations.ok"
              "${msgoff}" || msg "Exiting monitor of Nomad Evaluation(s) [${ids_array[@]}]"
              return 0
            fi
          fi
        ;;
####### job -> monitor-job-allocs )#############################################
        monitor-job-allocs )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME MSGOFF"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          local msgoff=${1:?$usage};   shift
          # Fetch the allocations IDs and monitor them.
          local nomad_command_stdout # Don't masquerade errors behind `local`
          if ! nomad_command_stdout=$(nomad job allocs -json "${job_name}")
          then
            "${msgoff}" || msg "$(red "FATAL: Command \"nomad job allocs\" failed")"
            # Error messages surely shown on stderr and this variable is empty
            if test -n "${nomad_command_stdout}"
            then
              "${msgoff}" || msg "${nomad_command_stdout}"
            fi
            # Send fatal job error signal after printing this error's messages!
            touch "${job_file}.run/job.error"
            return 1
          fi
          # IDs Bash array with the command result if any (jq query using "?")
          local ids_array # Don't masquerade errors behind `local`
          ids_array=($(echo "${nomad_command_stdout}" | jq --raw-output "map(.ID)? | join (\" \")"))
          # If no allocations start all over again
          if test -z "${ids_array:-}" # If = () "unbound variable" error
          then
            # But stop if any other cluster failure happens
            if test -f "${job_file}.run/job.error"
            then
              # Monitor was not started, IDs not known, no messages to show
              return 1
            else
              sleep 1
              # Iterate and exit with the return code of the new call
              wb_nomad job monitor-job-allocs           \
                "${job_file}" "${job_name}" "${msgoff}"
            fi
          else
            # Iterate through allocations
            local jobs_array=()
            "${msgoff}" || msg "Entering monitor of Nomad $(yellow "Allocation(s) [${ids_array[@]}]")"
            for alloc_id in ${ids_array[*]}
            do
              # Create file "allocations.error" is any allocation fails
                  wb_nomad job monitor-alloc-id               \
                    "${job_file}" "${job_name}" "${alloc_id}" \
                    "${msgoff}"                               \
                ||                                            \
                  touch "${job_file}.run/allocations.error"   \
              &
              jobs_array+=("$!")
            done
            # Wait for all processes to finish or kill them if at least one fails!
            if ! wait_kill_em_all "${jobs_array[@]}" || test -f "${job_file}.run/allocations.error"
            then
              "${msgoff}" || msg "$(red "Exiting monitor of Nomad Allocation(s) [${ids_array[@]}] due to errors")"
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            else
              touch "${job_file}.run/allocations.ok"
              "${msgoff}" || msg "Exiting monitor of Nomad Allocation(s) [${ids_array[@]}]"
              return 0
            fi
          fi
        ;;
####### job -> monitor-job-alloc-tasks )########################################
        monitor-job-alloc-tasks )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME ALLOC-ID MSGOFF"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          local alloc_id=${1:?$usage}; shift
          local msgoff=${1:?$usage};   shift
          # Fetch the allocation's status and monitor its Tasks.
          local nomad_command_stdout # Don't masquerade errors behind `local`
          if ! nomad_command_stdout=$(nomad alloc status -json "${alloc_id}")
          then
            "${msgoff}" || msg "$(red "FATAL: Command \"nomad alloc status\" failed")"
            # Error messages surely shown on stderr and this variable is empty
            if test -n "${nomad_command_stdout}"
            then
              "${msgoff}" || msg "${nomad_command_stdout}"
            fi
            # Send fatal job error signal after printing this error's messages!
            touch "${job_file}.run/job.error"
            return 1
          fi
          # IDs Bash array with the command result if any (jq query using "?")
          local ids_array # Don't masquerade errors behind `local`
          ids_array=($(echo "${nomad_command_stdout}" | jq --raw-output ".TaskStates? | keys? | join (\" \")"))
          # If no tasks start all over again
          if test -z "${ids_array:-}" # If = () "unbound variable" error
          then
            # But stop if any other cluster failure happens
            if test -f "${job_file}.run/job.error"
            then
              # Monitor was not started, IDs not known, no messages to show
              return 1
            else
              sleep 1
              # Iterate and exit with the return code of the new call
              wb_nomad job monitor-job-alloc-tasks                    \
                "${job_file}" "${job_name}" "${alloc_id}" "${msgoff}"
            fi
          else
            # Iterate through allocation's tasks
            local jobs_array=()
            "${msgoff}" || msg "Entering monitor of Nomad Allocation \"${alloc_id}\" $(yellow "Task(s) [${ids_array[@]}]")"
            for task_name in ${ids_array[*]}
            do
              # Create file "tasks.error" is any task fails
                  wb_nomad job monitor-alloc-id-task-name                    \
                    "${job_file}" "${job_name}" "${alloc_id}" "${task_name}" \
                    "${msgoff}"                                              \
                ||                                                           \
                  touch "${job_file}.run/tasks.error"                        \
              &
              jobs_array+=("$!")
            done
            # Wait for all processes to finish or kill them if at least one fails!
            if ! wait_kill_em_all "${jobs_array[@]}" || test -f "${job_file}.run/tasks.error"
            then
              "${msgoff}" || msg "$(red "Exiting monitor of Nomad Allocation \"${alloc_id}\" Task(s) [${ids_array[@]}] due to errors")"
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            else
              touch "${job_file}.run/tasks.${alloc_id}.ok"
              "${msgoff}" || msg "Exiting monitor of Nomad Allocation \"${alloc_id}\" Task(s) [${ids_array[@]}]"
              return 0
            fi
          fi
        ;;
####### job -> check-eval-id-placement-failures )###############################
        check-eval-id-placement-failures )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME EVAL-ID MSGOFF"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          local eval_id=${1:?$usage};  shift
          local msgoff=${1:?$usage};   shift
          "${msgoff}" || msg "$(blue Checking) for \"Placement Failures\" in Nomad $(blue Evaluation) $(yellow "\"${eval_id}\"") ..."
          local status_response
          if ! status_response=$(nomad eval status "${eval_id}")
          then
            "${msgoff}" || msg "$(red "FATAL: Command \"nomad eval status\" failed")"
            if test -n "${status_response}"
            then
              "${msgoff}" || msg "${status_response}"
            fi
            # Send fatal job error signal after printing this error's messages!
            touch "${job_file}.run/job.error"
            return 1
          else
            if echo "${status_response}" | grep --quiet "^Placement Failures = true"
            then
              "${msgoff}" || msg "$(red "FATAL: Nomad Job Evaluation \"${eval_id}\" has \"Placement Failures\"")"
              "${msgoff}" || nomad eval status -verbose "${eval_id}" 1>&2 || true
              return 1
            fi
          fi
        ;;
####### job -> check-eval-id )##################################################
        monitor-eval-id )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME EVAL-ID MSGOFF"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          local eval_id=${1:?$usage};  shift
          local msgoff=${1:?$usage};   shift
          "${msgoff}" || msg "$(blue Waiting) for Nomad $(yellow "Evaluation \"${eval_id}\"") to be \"complete\" ..."
          local status=""
          local status_response
          while ! test -f "${job_file}.run/job.error" && ( test "${status:-pending}" = "pending" || test "${status:-running}" = "running" )
          do
            if ! status_response=$(nomad eval status -json "${eval_id}")
            then
              "${msgoff}" || msg "$(red "FATAL: Command \"nomad eval status\" failed")"
              if test -n "${status_response}"
              then
                "${msgoff}" || msg "${status_response}"
              fi
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            fi
            status=$(echo "${status_response}" | jq -r .Status)
            echo "${status_response}" > "${job_file}.run/evaluation.${eval_id}.$(date +%Y-%m-%d-%H-%M-%S-%N).json"
            # Monitor deployments "concurrently" (no need for sleeps here)!
            local deploy_id=$(echo "${status_response}" | jq -r .DeploymentID)
            local deploy_output
            # TODO:FIXME: Can an evaluation finish without providing a DeploymentID ???
            if test "${deploy_id:-null}" != "null"
            then
              if ! deploy_output=$(wb_nomad job monitor-deploy-id "${job_file}" "${job_name}" "${deploy_id}" "${msgoff}")
              then
                # Deployments can fail because the tasks did not start before
                # the deadline as they were still building the nix packages.
                # When done building Tasks run just fine but the deployment
                # is already considered failed.
                "${msgoff}" || msg "$(yellow "WARNING: A Nomad Deployment failed while waiting for its Evaluation")"
                if test -n "${deploy_output}"
                then
                  "${msgoff}" || msg "${deploy_output}"
                fi
              fi
            fi
          done
          # Check response that ended the loop!
          if test "${status}" != "complete"
          then
            # Only an evaluation specific error if the loop was not stopped!
            if ! test -f "${job_file}.run/job.error"
            then
              # Store the error response that ended the loop!
              echo "${status_response}" > "${job_file}.run/evaluation.${eval_id}.error.json"
              # Show this error messages.
              "${msgoff}" || msg "$(red "FATAL: Nomad Evaluation \"${eval_id}\" failed")"
              if test -n "${status_response}"
              then
                "${msgoff}" || msg "${status_response}"
              fi
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            fi
          else
            # This can't be obtained from the json and happens even if "complete"!
            local placement_response
            if ! placement_response=$(wb_nomad job check-eval-id-placement-failures "${job_file}" "${job_name}" "${eval_id}" "${msgoff}")
            then
              # Store the error response that ended the loop!
              echo "${placement_response}" > "${job_file}.run/evaluation.${eval_id}.error.json"
              # Show this error messages.
              if test -n "${placement_response}"
              then
                "${msgoff}" || msg "${placement_response}"
              fi
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            else
              # Store the response that made it final!
              echo "${status_response}" > "${job_file}.run/evaluation.${eval_id}.final.json"
              "${msgoff}" || msg "$(green "Nomad Evaluation \"${eval_id}\" is \"complete\"")"
            fi
          fi
        ;;
####### job -> monitor-deploy-id )##############################################
        monitor-deploy-id )
          local usage="USAGE:wb nomad ${op} ${subop} JOB-FILE JOB-NAME DEPLOY-ID MSGOFF"
          local job_file=${1:?$usage};  shift
          local job_name=${1:?$usage};  shift
          local deploy_id=${1:?$usage}; shift
          local msgoff=${1:?$usage};    shift
          "${msgoff}" || msg "$(blue Waiting) for Nomad $(yellow "Deployment \"${deploy_id}\"") to be \"successful\" ..."
          local status=""
          local status_response
          while ! test -f "${job_file}.run/job.error" && ! test -f "${job_file}.run/allocations.ok" && ( test "${status:-pending}" = "pending" || test "${status:-running}" = "running" )
          do
            if ! status_response=$(nomad deployment status -json "${deploy_id}")
            then
              "${msgoff}" || msg "$(red "FATAL: Command \"nomad deployment status\" failed")"
              if test -n "${status_response}"
              then
                "${msgoff}" || msg "${status_response}"
              fi
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            fi
            status=$(echo "${status_response}" | jq -r .Status)
            echo "${status_response}" > "${job_file}.run/deployment.${deploy_id}.$(date +%Y-%m-%d-%H-%M-%S-%N).json"
            sleep 1
          done
          # Check response that ended the loop!
          if test "${status}" != "successful"
          then
            # Only a deployment specific error if the loop was not stopped!
            if ! test -f "${job_file}.run/job.error" && ! test -f "${job_file}.run/allocations.ok"
            then
              # Store the error response that ended the loop!
              echo "${status_response}" > "${job_file}.run/deployment.${deploy_id}.error.json"
              "${msgoff}" || msg "$(yellow "WARNING: Nomad Deployment \"${deploy_id}\" failed")"
              if test -n "${status_response}"
              then
                "${msgoff}" || msg "${status_response}"
              fi
              # Deployment failures are not considered fatal!
            else
              if test -f "${job_file}.run/allocations.ok"
              then
                "${msgoff}" || msg "$(yellow "WARNING: Nomad deployment \"${deploy_id}\" is assumed \"successful\" because all allocations are \"running\"")"
              fi
            fi
          else
            # Store the response that made it final!
            echo "${status_response}" > "${job_file}.run/deployment.${deploy_id}.final.json"
            "${msgoff}" || msg "$(green "Nomad Deployment \"${deploy_id}\" is \"successful\"")"
          fi
        ;;
####### job -> monitor-alloc-id )###############################################
        monitor-alloc-id )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME ALLOC-ID MSGOFF"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          local alloc_id=${1:?$usage}; shift
          local msgoff=${1:?$usage};   shift
          "${msgoff}" || msg "$(blue Waiting) for Nomad $(yellow "Allocation \"${alloc_id}\"") to be \"running\" ..."
          local status=""
          local status_response
          while ! test -f "${job_file}.run/job.error" && test "${status:-pending}" = "pending"
          do
            if ! status_response=$(nomad alloc status -json "${alloc_id}")
            then
              "${msgoff}" || msg "$(red "FATAL: Command \"nomad alloc status\" failed")"
              if test -n "${status_response}"
              then
                "${msgoff}" || msg "${status_response}"
              fi
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            fi
            status=$(echo "${status_response}" | jq -r .ClientStatus)
            echo "${status_response}" > "${job_file}.run/allocation.${alloc_id}.$(date +%Y-%m-%d-%H-%M-%S-%N).json"
            # Monitor tasks "concurrently" (no need for sleeps here)!
            if ! test -f "${job_file}.run/tasks.${alloc_id}.ok" && ! test -f "${job_file}.run/tasks.error"
            then
              local tasks_output
              if ! tasks_output=$(wb_nomad job monitor-job-alloc-tasks "${job_file}" "${job_name}" "${alloc_id}" "false")
              then
                "${msgoff}" || msg "$(red "FATAL: A Nomad Task failed while waiting for its Allocation")"
                if test -n "${tasks_output}"
                then
                  "${msgoff}" || msg "${tasks_output}"
                fi
                msg "$(yellow "INFO: Nomad Allocation \"${alloc_id}\" entrypoint stdout:")"
                nomad alloc logs -verbose         "${alloc_id}" > "${job_file}.run/allocation.${alloc_id}.stdout"
                cat "${job_file}.run/allocation.${alloc_id}.stdout"
                msg "$(yellow "INFO: Nomad Allocation \"${alloc_id}\" entrypoint stderr:")"
                nomad alloc logs -verbose -stderr "${alloc_id}" > "${job_file}.run/allocation.${alloc_id}.stderr"
                cat "${job_file}.run/allocation.${alloc_id}.stderr"
                return 1
              fi
            fi
          done
          # Check response that ended the loop!
          if test "${status}" != "running"
          then
            # Only an allocation specific error if the loop was not stopped!
            if ! test -f "${job_file}.run/job.error"
            then
              # Store the error response that ended the loop!
              echo "${status_response}" > "${job_file}.run/allocation.${alloc_id}.error.json"
              msg "$(red "FATAL: Nomad allocation \"${alloc_id}\" failed")"
              # Don't show the Job spec, too big!
              # FIXME: I want the output to keep `jq`'s default formatting!
              msg $(echo "${status_response}" | jq 'del(.Job)')
              msg "$(yellow "INFO: Nomad allocation \"${alloc_id}\" entrypoint stdout:")"
              nomad alloc logs -verbose         "${alloc_id}" > "${job_file}.run/allocation.${alloc_id}.stdout"
              cat "${job_file}.run/allocation.${alloc_id}.stdout"
              msg "$(yellow "INFO: Nomad allocation \"${alloc_id}\" entrypoint stderr:")"
              nomad alloc logs -verbose -stderr "${alloc_id}" > "${job_file}.run/allocation.${alloc_id}.stderr"
              cat "${job_file}.run/allocation.${alloc_id}.stderr"
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            fi
          else
            # Store the response that made it final!
            echo "${status_response}" > "${job_file}.run/allocation.${alloc_id}.final.json"
            "${msgoff}" || msg "$(green "Nomad Allocation \"${alloc_id}\" is \"running\"")"
          fi
          # - Job (The variable interpolated job?)
          # echo "${alloc_result}" | jq ".Job"                                > "${job_file}".allocated || true
          # echo "${alloc_result}" | jq ".AllocatedResources.Shared.Networks" > "${job_file}".networks  || true
          # echo "${alloc_result}" | jq ".AllocatedResources.Shared.Ports"    > "${job_file}".ports     || true
          # TODO: Also look at
          # - AllocatedResources.Tasks.TASK-NAME.Networks
        ;;
####### job -> monitor-alloc-id-task-name )#####################################
        monitor-alloc-id-task-name )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE JOB-NAME ALLOC-ID MSGOFF"
          local job_file=${1:?$usage};  shift
          local job_name=${1:?$usage};  shift
          local alloc_id=${1:?$usage};  shift
          local task_name=${1:?$usage}; shift
          local msgoff=${1:?$usage};    shift
          "${msgoff}" || msg "$(blue Waiting) for Nomad $(yellow "Task \"${task_name}\"") to be \"running\" ..."
          local status=""
          local status_response
          while ! test -f "${job_file}.run/job.error" && test "${status:-pending}" = "pending"
          do
            if ! status_response=$(nomad alloc status -json "${alloc_id}")
            then
              "${msgoff}" || msg "$(red "FATAL: Command \"nomad alloc status\" failed")"
              if test -n "${status_response}"
              then
                "${msgoff}" || msg "${status_response}"
              fi
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            fi
            status="$(echo "${status_response}" | jq -r .TaskStates.\"${task_name}\".State)"
            # Remove the entire Job (mayus!!!) description from the Task's log.
            echo "${status_response}" | jq '.Job = null' > "${job_file}.run/task.${task_name}.$(date +%Y-%m-%d-%H-%M-%S-%N).json"
            sleep 1
          done
          # Check response that ended the loop!
          if test "${status}" != "running"
          then
            # Only a task specific error if the loop was not stopped!
            if ! test -f "${job_file}.run/job.error"
            then
              # Store the error response that ended the loop!
              # Remove the entire Job (mayus!!!) description from the Task's log.
              echo "${status_response}" | jq '.Job = null' > "${job_file}.run/task.${task_name}.error.json"
              # Show this error messages.
              "${msgoff}" || msg "$(red "FATAL: Nomad Task \"${task_name}\" failed")"
              "${msgoff}" || msg "$(echo ${status_response} | jq .TaskStates.\"${task_name}\")"
              # Send fatal job error signal after printing this error's messages!
              touch "${job_file}.run/job.error"
              return 1
            fi
          else
            # Store the response that made it final!
            # Remove the entire Job (mayus!!!) description from the Task's log.
            echo "${status_response}" | jq '.Job = null' > "${job_file}.run/task.${task_name}.final.json"
            "${msgoff}" || msg "$(green "Nomad Task \"${task_name}\" is \"running\"")"
            local client_id=$(echo "${status_response}"   | jq .NodeID -r)
            local client_name=$(echo "${status_response}" | jq .NodeName -r )
            # TODO: Also contains .DeploymentID , .EvalID and .FollowupEvalID
            "${msgoff}" || msg "Nomad $(yellow "Task \"${task_name}\"") was placed on Nomad $(yellow "client named \"${client_name}\"") with ID \"${client_id}\""
          fi
        ;;
####### job -> task-name-allocation-id )########################################
        task-name-allocation-id )
          local usage="USAGE:wb nomad ${op} ${subop} JOB-FILE TASK-NAME"
          local job_file=${1:?$usage};  shift
          local task_name=${1:?$usage}; shift
          jq -r '.ID' "${job_file}".run/task.${task_name}.final.json
        ;;
####### job -> task-name-node-name )############################################
        task-name-node-name )
          local usage="USAGE:wb nomad ${op} ${subop} JOB-FILE TASK-NAME"
          local job_file=${1:?$usage};  shift
          local task_name=${1:?$usage}; shift
          jq -r '.NodeName' "${job_file}".run/task.${task_name}.final.json
        ;;
####### job -> stop )###########################################################
        stop )
          local usage="USAGE: wb nomad ${op} ${subop} JOB-FILE TASK-NAME"
          local job_file=${1:?$usage}; shift
          local job_name=${1:?$usage}; shift
          # Do the prune, purge, garbage collect thing!
          nomad job stop -global -no-shutdown-delay -purge -yes -verbose "${job_name}" || msg "$(red "Failed to stop Nomad job")"
        ;;
####### job -> node-specs )#####################################################
        node-specs )
          # Creates an ex-post "node-specs.json" like file.
          # It uses Nomad tasks and allocations data plus the files that were
          # actually deployed, these last ones because parts of them are
          # dynamically generated using Nomad templates.
          local usage="USAGE:wb nomad ${op} ${subop} JOB-FILE"
          local job_file=${1:?$usage}; shift
          # The nodes/clients file must exists!
          local clients_file_path="$(dirname "${job_file}")"/clients.json
          local node_specs_path="$(dirname "${job_file}")"/../node-specs.json
          # Top object start
          ##################
          echo "{"
          # Grab all the "i" properties from inside each "node-i" object
          # Why "i" and not "name"? `jq` sorts like this: "node-49", "node-5",
          # "node-50".
          local node_specs_is
          node_specs_is=$(jq --raw-output \
            'map(.i) | join (" ")' \
            "${node_specs_path}" \
          )
          local first_node="true"
          for node_i in ${node_specs_is[*]}
          do
            # Nomad Job Tasks' names are taken from the `node-specs.json` file.
            # Task names are of the form "node-0", "node-1", "node-10" (not
            # "node-04").
            local task_name
            task_name=$(jq --raw-output \
              "map(select(.i == ${node_i})) | .[] | .name" \
              "${node_specs_path}" \
            )
            # Node open "{"
            ###############
            # If not the first one ","
            if test "${first_node}" == "true"
            then
              first_node="false"
              echo "    \"${task_name}\": {"
            else
              echo "  , \"${task_name}\": {"
            fi
            # Fetch from the allocation data the Nomad Client ID, Name and
            # Datacenter/region were this Task was deployed.
            local nomad_client_id
            nomad_client_id=$(jq --raw-output \
              .NodeID \
              "${job_file}".run/task."${task_name}".final.json \
            )
            local nomad_client_name
            nomad_client_name=$(jq --raw-output \
              .NodeName \
              "${job_file}".run/task."${task_name}".final.json \
            )
            local nomad_client_datacenter
            nomad_client_datacenter=$(jq --raw-output \
              ". | map(select(.id == \"${nomad_client_id}\")) | .[0] | .datacenter" \
              "${clients_file_path}" \
            )
            # With the Nomad Client data now fetch AZ and port.
            local nomad_client_az # Client's AWS AZ were this task was deployed!
            nomad_client_az=$(jq --raw-output \
              ". | map(select(.id == \"${nomad_client_id}\")) | .[0] | .attributes.platform.aws.placement[\"availability-zone\"]" \
              "${clients_file_path}" \
            )
            local nomad_task_port # Task's reserved port number!
            nomad_task_port=$(jq --raw-output \
              .Resources.Networks[0].ReservedPorts[0].Value \
              "${job_file}".run/task."${task_name}".final.json \
            )
            local nomad_task_ip
            nomad_task_ip=$(jq --raw-output \
              ". | map(select(.name == \"${nomad_client_name}\")) | .[0] | .attributes.unique.platform.aws[\"public-ipv4\"]" \
              "${clients_file_path}" \
            )
            # Same data as "node-specs.json".
            #################################
            echo "          \"i\":      ${node_i}"
            echo "        , \"name\":   \"${task_name}\""
            echo "        , \"region\": \"${nomad_client_datacenter}\""
            echo "        , \"port\":   ${nomad_task_port}"
            # Extra Nomad client data.
            ##########################
            echo "        , \"nomad-client\": {"
            echo "              \"id\":   \"${nomad_client_id}\""
            echo "            , \"name\": \"${nomad_client_name}\""
            echo "            , \"az\":   \"${nomad_client_az}\""
            echo "            , \"ip\":   \"${nomad_task_ip}\""
            echo "        }"
            # Node close "}"
            ################
            echo "  }"
          done
          # Top object end
          ################
          echo "}"
        ;;
####### job -> topology )#######################################################
        topology )
          # Creates an ex-post "topology.json" like file.
          # It uses Nomad tasks and allocations data plus the files that were
          # actually deployed, these last ones because parts of them are
          # dynamically generated using Nomad templates.
          # The "producers" list of each node is re-constructed using the Nomad
          # services definitions.
          local usage="USAGE:wb nomad ${op} ${subop} JOB-FILE"
          local job_file=${1:?$usage}; shift
          # The nodes/clients file must exists!
          local clients_file_path="$(dirname "${job_file}")"/clients.json
          local topology_path="$(dirname "${job_file}")"/../topology.json
          # Helper, called for "coreNodes" and "relayNodes" separately.
          topology-node-helper() {
            local task_name="$1"
            local node_i="$2"
            # Fetch from the allocation data the Nomad Client ID and
            # Datacenter/region were this Task was deployed.
            local nomad_client_id
            nomad_client_id=$(jq --raw-output \
              .NodeID \
              "${job_file}".run/task."${task_name}".final.json \
            )
            local nomad_client_datacenter
            nomad_client_datacenter=$(jq --raw-output \
              ". | map(select(.id == \"${nomad_client_id}\")) | .[0] | .datacenter" \
              "${clients_file_path}" \
            )
            # Same data as "topology.json".
            ###############################
            echo "             \"name\":   \"${task_name}\""
            echo "           , \"nodeId\": ${node_i}"
            echo "           , \"region\": \"${nomad_client_datacenter}\""
            # producers start
            #################
            echo "           , \"producers\": ["
            local node_topology_file_path
            node_topology_file_path="$(dirname "${job_file}")"/../"${task_name}"/topology.json
            # Grab producers from the fetched, after deployment, "topology.json" files, merging P2P and non-P2P formats.
            local node_producers
            # Merge non-P2P and P2P in the same {addr:"ADDR",port:0} format.
            # P2P uses "address" instead of "addr" and is:
            ### {
            ### "localRoots": [
            ### {
            ###   "accessPoints": [
            ###     {
            ###       "address": "10.0.0.1",
            ###       "port": 30001
            ###     },
            ###     {
            ###       "address": "10.0.0.2",
            ###       "port": 30002
            ###     },
            # non-P2P is:
            ### {
            ### "Producers": [
            ###  {
            ###    "addr": "10.0.0.1",
            ###    "port": 30001
            ###  },
            node_producers=$(jq \
              '.Producers//[] + ((.localRoots[0].accessPoints//[]) | map({addr:.address,port:.port}))' \
              "${node_topology_file_path}"
            )
            local node_producers_keys
            node_producers_keys=$(echo "${node_producers}" | jq --raw-output 'keys | join (" ")')
            local node_producer_i=0
            for node_producer_key in ${node_producers_keys[*]}
            do
              # The topology file, as used by the node, is already formated as
              # {"addr":XX,"port":YY} were the XX and YY values were resolved
              # using Nomad templates.
              local producer_addr producer_port
              producer_addr=$(echo "${node_producers}" | jq -r ".[${node_producer_key}] | .addr")
              producer_port=$(echo "${node_producers}" | jq -r ".[${node_producer_key}] | .port")
              # From the public IP and port look for the node that was deployed
              # with this values by searching thorugh the services definitions.
              local node_specs_path="$(dirname "${job_file}")"/../node-specs.json
              local node_specs_names
              node_specs_names=$(jq --raw-output \
                'map(.name) | join (" ")' \
                "${node_specs_path}" \
              )
              for node_name in ${node_specs_names[*]}
              do
                if jq -e "any(select(.Address == \"${producer_addr}\" and .Port == ${producer_port}))" "$(dirname "${job_file}")"/"${node_name}"/service-info.json >/dev/null
                then
                  if test "${node_producer_i}" == "0"
                  then
                    echo "                  \"${node_name}\""
                  else
                    echo "                , \"${node_name}\""
                  fi
                  node_producer_i=$((node_producer_i + 1))
                fi
              done
            done
            # producers end
            ###############
            echo "            ]"
          }
          # Top object start
          ##################
          echo "{"
          # coreNodes start
          #################
          echo "    \"coreNodes\": ["
          # Grab all the "nodeId" properties from inside each array's objects
          # Why "nodeId" and not "name"? `jq` sorts like this: "node-49",
          # "node-5", "node-50".
          local coreNodes_is
          coreNodes_is=$(jq --raw-output \
            '.coreNodes | map(.nodeId) | join (" ")' \
            "${topology_path}" \
          )
          local first_coreNode="true"
          for node_i in ${coreNodes_is[*]}
          do
            # Nomad Job Tasks' names are taken from the `topology.json` file.
            # Task names are of the form "node-0", "node-1", "node-10" (not "node-04").
            local task_name
            task_name=$(jq --raw-output \
              ".coreNodes | map(select(.nodeId == ${node_i})) | .[] | .name" \
              "${topology_path}" \
            )
            # Node open "{"
            ###############
            # If not the first one ","
            if test "${first_coreNode}" == "true"
            then
              first_coreNode="false"
              echo "        {"
            else
              echo "      , {"
            fi
            topology-node-helper "${task_name}" "${node_i}"
            # Node close "}"
            ################
            echo "        }"
          done
          # coreNodes end
          ###############
          echo "  ]"
          # relayNodes start
          ##################
          echo "  , \"relayNodes\": ["
          # Grab all the "i" properties from inside each "node-i" object
          # Why "i" and not name? `jq` sorts like this: "node-49", "node-5", "node-50"
          local relayNodes_is
          relayNodes_is=$(jq --raw-output \
            '.relayNodes | map(.nodeId) | join (" ")' \
            "${topology_path}" \
          )
          local first_relayNode="true"
          for node_i in ${relayNodes_is[*]}
          do
            # Nomad Job Tasks' names are taken from the `topology.json` file.
            # Task names are of the form "node-0", "node-1", "node-10" (not "node-04").
            local task_name
            task_name=$(jq --raw-output \
              ".relayNodes | map(select(.nodeId == ${node_i})) | .[] | .name" \
              "${topology_path}" \
            )
            # Node open "{"
            ###############
            # If not the first one ","
            if test "${first_relayNode}" == "true"
            then
              first_relayNode="false"
              echo "      {"
            else
              echo "    , {"
            fi
            topology-node-helper "${task_name}" "${node_i}"
            # Node close "}"
            ################
            echo "      }"
          done
          # relayNodes end
          ################
          echo "  ]"
          # Top object end
          ################
          echo "}"
        ;;
####### job -> * )##############################################################
        * )
          usage_nomad
        ;;
      esac # job
    ;;
################################################################################
################################################################################
################################################################################
    * )
      usage_nomad
    ;;

  esac

}

# TODO: `nomad-retry`
# Example errors:
# - Error retrieving deployment: Get "https://nomad.world.dev.cardano.org/v1/deployment/f8921691-abad-567c-dd79-f1fa8ae672eb?namespace=perf": net/http: TLS handshake timeout
# - Error querying allocation: Get "https://nomad.world.dev.cardano.org/v1/allocations?namespace=perf&prefix=4b2b9fad-145e-5221-a89b-5e7ffbe955aa": dial tcp 3.72.231.105:443: connect: connection refused
# - Error querying allocation: Get "https://nomad.world.dev.cardano.org/v1/allocations?namespace=perf&prefix=3e9f7b8e-56ef-3548-3c5b-c6406424608c": net/http: TLS handshake timeout
# - Error querying allocation: Get "https://nomad.world.dev.cardano.org/v1/allocations?namespace=perf&prefix=d23e3485-453e-e2d9-0312-91273c043c0b": tls: failed to verify certificate: x509: certificate is valid for 6574a66487be7c78e86d0a547d1d56ed.3097b591bb3e0e3ad8356f18812c6de1.traefik.default, not nomad.world.dev.cardano.org
# - Error querying allocation: Unexpected response code: 403 (ACL token not found)
# - Error reading file: Unexpected response code: 404 (task "node-50" not started yet. No logs available)
# - failed to exec into task: unexpected EOF
# - failed to exec into task: read tcp 10.0.101.184:58166->3.72.231.105:443: read: connection reset by peer
# - failed to exec into task: websocket closed before receiving exit code: websocket: close 1000 (normal)
