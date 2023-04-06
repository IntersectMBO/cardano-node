usage_publish() {
     usage "publish" "Bench data publish" <<EOF
    $(helpcmd local)
                     Calls 'bench-data-publish' and forwards the parameters.
    $(helpcmd cloud)
                     Forwards parameters to 'bench-data-publish' with host (-h)
                     and port (-P) pointing to the socat|norouter based tunnel,
                     user (-u), password (BENCH_DATA_PASS=) and database (--db)
                     to what SRE provides and schema (-s) to the workbench's
                     default schema.
    $(helpcmd psql-cloud)
                     Forwards parameters to 'psql' with host (--host) and port
                     (--port) pointing to the socat|norouter based tunnel,
                     database (--dbname), user (--username) and password
                     (PGPASSWORD=) to what SRE provides.
    $(helpcmd nomad token)
                     Queries the SRE's Nomad server for your token properties
                     (WARNING: shows secrets!!!).
    $(helpcmd nomad desc)
                     Gets the JSON file of the Nomad job provided by SRE to
                     create a tunnel.
    $(helpcmd nomad job [-verbose] status)
                     Gets the status of the "tunnel" Nomad job.
    $(helpcmd nomad id)
                     Gets the allocation ID of the "tunnel" Nomad job.
    $(helpcmd nomad alloc [-verbose] status)
                     Gets the allocation status of the "tunnel" Nomad job.
    $(helpcmd nomad fs FILE_PATH)
                     Gets file or folder contentents inside the "tunnel" Nomad job.
    $(helpcmd nomad signal)
                     Sends SIGINT to the "tunnel" Nomad job.
    $(helpcmd nomad restart)
                     Restarts the "tunnel" Nomad job.
    $(helpcmd socat start)
                     Creates a socat based tunnel to the Postgres server running
                     on SRE's Nomad.
    $(helpcmd socat stop)
                     Stops the running socat based tunnel.
    $(helpcmd socat configure [LOCAL_PORT=15432])
                     Creates the socat based tunnel configuration.
    $(helpcmd socat local-host)
                     Gets the configured host for the socat based tunnel.
    $(helpcmd socat local-port)
                     Gets the configured port for the socat based tunnel.
    $(helpcmd socat pid-file)
                     Gets the path of the file cotaining the PID of the socat
                     based tunnel process.
    $(helpcmd socat pid)
                     Gets the PID of the socat based tunnel process or fail.
    $(helpcmd socat stdout)
                     Outputs stdout of the process running the socat based
                     tunnel.
    $(helpcmd socat stderr)
                     Outputs stderr of the process running the socat based
                     tunnel.
    $(helpcmd norouter start)
                     Creates a norouter based tunnel to the Postgres server
                     running on SRE's Nomad.
    $(helpcmd norouter stop)
                     Stops the running norouter based tunnel.
    $(helpcmd norouter configure [LOCAL_PORT=15432])
                     Creates the norouter based tunnel configuration.
    $(helpcmd norouter local-host)
                     Gets the configured host for the norouter based tunnel.
    $(helpcmd norouter local-port)
                     Gets the configured port for the norouter based tunnel.
    $(helpcmd norouter pid-file)
                     Gets the path of the file cotaining the PID of the norouter
                     based tunnel.
    $(helpcmd socat pid)
                     Gets the PID of the norouter based tunnel process or fail.
    $(helpcmd norouter stdout)
                     Outputs stdout of the process running the norouter based
                     tunnel.
    $(helpcmd norouter stderr)
                     Outputs stderr of the process running the norouter based
                     tunnel.
EOF
}

publish_default_op='local'

publish() {

  # User's cache dirs:
  # Calling `wb publish XXX` inside a Nix derivation will make everything fail:
  # "mkdir: cannot create directory '/homeless-shelter': Permission denied"
  ## Local cache dirs:
  mkdir -p "$(envjqr 'cacheDir')"/nomad
  ## Tunnel type to publish into the cloud
  ## `norouter` currently hangs the nomad job, DON'T USE IT!
  mkdir -p "$(envjqr 'cacheDir')"/socat
  local socat_config_file="$(envjqr 'cacheDir')"/socat/config.json
  mkdir -p "$(envjqr 'cacheDir')"/norouter
  local norouter_config_file="$(envjqr 'cacheDir')"/norouter/norouter.yaml

  # Publish script global paramaters:
  ## SRE `Nomad` specific cloud parameters:
  ### -address=<addr>
  #### The address of the Nomad server.
  #### Overrides the NOMAD_ADDR environment variable if set.
  #### Default = http://127.0.0.1:4646
  local nomad_address="https://nomad.ci.iog.io"
  ### -namespace=<namespace>
  #### The target namespace for queries and actions bound to a namespace.
  #### Overrides the NOMAD_NAMESPACE environment variable if set.
  #### If set to '*', subcommands which support this functionality query
  #### all namespaces authorized to user.
  #### Defaults to the "default" namespace.
  local nomad_namespace="perf"
  ### -job <job-name>
  ### Use a random allocation from the specified job ID.
  local nomad_job_name="norouter"
  ### -task <task-name>
  #### Sets the task to exec command in
  local nomad_task_name="norouter"
  ## `bench-data-publish`/cardano-automation specific parameters:
  local pg_db="perf"

  # Note on the use of bash's `local`:
  # "Local can only be used within a function; it makes the variable name have a
  # visible scope restricted to that function and its children."
  # "when declaring and setting a local variable in a single command, apparently
  # the order of operations is to first set the variable, and only afterwards
  # restrict it to local scope."
  # https://tldp.org/LDP/abs/html/localvar.html

  local op=${1:-$publish_default_op}; test $# -gt 0 && shift

  case "$op" in

    local )
      local usage="USAGE: wb publish $op"
      msg "Calling 'bench-data-publish $@' ..."
      bench-data-publish $@
    ;;

    cloud )
      local usage="USAGE: wb publish $op"
      # Publish script vars.
      ## Construct $pg_host and $pg_port based on `socat` or `norouter`
      ## And check that either `socat` or `norouter` are configured and running
      local pg_host pg_port
      # socat takes precedence
      if wb publish socat pid >/dev/null
      then
        # Call without `local`, needs to fail everything if they fail.
        pg_host=$(wb publish socat local-host)
        pg_port=$(wb publish socat local-port)
      # Now try with norouter
      elif wb publish norouter pid >/dev/null
      then
        # Call without `local`, needs to fail everything if they fail.
        pg_host=$(wb publish norouter local-host)
        pg_port=$(wb publish norouter local-port)
      else
        fatal "The tunnel was not created, call 'wb publish (socat|norouter) start'"
      fi
      # Workbench/SRE paramaters:
      local pg_user pg_pass
      # Call without `local`, needs to fail everything if they fail.
      pg_user=$(wb nomad vault ci pg-user)
      pg_pass=$(wb nomad vault ci pg-pass)
      # Publish the data
      # TODO: Use of this environment variable is not recommended for security
      # reasons, as some operating systems allow non-root users to see process
      # environment variables via ps; instead consider using a password file.
      msg "Calling 'BENCH_DATA_PASS=\"...\" bench-data-publish -h \"${pg_host}\" -P \"${pg_port}\" -u \"${pg_user}\" --db \"${pg_db}\" $@' ..."
      BENCH_DATA_PASS="${pg_pass}" bench-data-publish -h "${pg_host}" -P "${pg_port}" -u "${pg_user}" --db "${pg_db}" $@
    ;;

    psql-cloud)
      local usage="USAGE: wb publish $op"
      # Publish script vars.
      ## Lookup $pg_host and $pg_port based on `socat` or `norouter`
      ## And check that either `socat` or `norouter` are configured and running
      local pg_host pg_port
      # socat takes precedence
      if wb publish socat pid >/dev/null
      then
        # Call without `local`, needs to fail everything if they fail.
        pg_host=$(wb publish socat local-host)
        pg_port=$(wb publish socat local-port)
      # Now try with norouter
      elif wb publish norouter pid >/dev/null
      then
        # Call without `local`, needs to fail everything if they fail.
        pg_host=$(wb publish norouter local-host)
        pg_port=$(wb publish norouter local-port)
      else
        fatal "The tunnel was not created, call 'wb publish (socat|norouter) start'"
      fi
      # Workbench/SRE paramaters:
      local pg_user pg_pass
      # Call without `local`, needs to fail everything if they fail.
      pg_user=$(wb nomad vault ci pg-user)
      pg_pass=$(wb nomad vault ci pg-pass)
      # postgresql://[user[:password]@][netloc][:port][/dbname][?param1=value1&...]
      # TODO: Use of this environment variable is not recommended for security
      # reasons, as some operating systems allow non-root users to see process
      # environment variables via ps; instead consider using a password file.
      msg "PGPASSWORD=\"...\" psql --host=\"${pg_host}\" --port=\"${pg_port}\" --username=\"${pg_user}\" --dbname=\"${pg_db}\" $@"
      PGPASSWORD="${pg_pass}" psql --host="${pg_host}" --port="${pg_port}" --username="${pg_user}" --dbname="${pg_db}" $@
    ;;

    nomad )
      local usage="USAGE: wb publish $op desc|job|id|alloc|fs|signal|restart"
      local action=${1:?$usage}; shift
      # Publish script vars
      local nomad_token
      ## Call without `local`, needs to fail everything if it fail.
      nomad_token=$(wb nomad vault ci nomad-token)
      # Nomad actions
      case "${action}" in
        token )
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad acl token self
        ;;
        desc ) # wb publish nomad job [-verbose] status
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad job inspect "${nomad_job_name}" | jq .
        ;;
        job ) # wb publish nomad job [-verbose] status
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad job $@ "${nomad_job_name}"
        ;;
        id )
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad job allocs -json "${nomad_job_name}" | jq -r '.[0].ID'
        ;;
        alloc ) # wb publish nomad alloc [-verbose] status
          local nomad_alloc_id
          # Call without `local`, needs to fail everything if it fails.
          nomad_alloc_id=$(wb publish nomad id)
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad alloc $@ "${nomad_alloc_id}"
        ;;
        fs ) # wb publish nomad fs FILE_PATH
          local nomad_alloc_id
          # Call without `local`, needs to fail everything if it fails.
          nomad_alloc_id=$(wb publish nomad id)
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad alloc fs "${nomad_alloc_id}" $@
        ;;
        signal )
          local nomad_alloc_id
          # Call without `local`, needs to fail everything if it fails.
          nomad_alloc_id=$(wb publish nomad id)
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad alloc signal "${nomad_alloc_id}"
        ;;
        restart )
          local nomad_alloc_id
          # Call without `local`, needs to fail everything if it fails.
          nomad_alloc_id=$(wb publish nomad id)
          NOMAD_ADDR="${nomad_address}" \
          NOMAD_TOKEN="${nomad_token}" \
          NOMAD_NAMESPACE="${nomad_namespace}" \
          nomad alloc restart "${nomad_alloc_id}"
        ;;
        * )
          usage_publish
        ;;
      esac
    ;;

    socat )
      local usage="USAGE: wb publish $op configure|start|stop"
      local action=${1:?$usage}; shift
      # Nomad actions
      case "$action" in
        configure )
          local usage="USAGE: wb publish $op configure [LOCAL_PORT=15432]"
          if test $# -gt 0
          then
            local local_port=${1:?$usage}; shift
          else
            local local_port=15432
          fi
          # Checks
          if wb publish socat pid >/dev/null
          then
            fatal "socat is already running, call 'wb publish socat stop' first"
          fi
          # Create config
          echo "{\"local_port\":${local_port}}" > "$socat_config_file"
        ;;
        local-host )
          echo "127.0.0.1"
        ;;
        local-port )
          if test -f $socat_config_file
          then
            jq -r .local_port "${socat_config_file}"
          else
            fatal "socat is not configured, call 'wb publish socat configure [LOCAL_PORT=15432]' first"
          fi
        ;;
        start )
          # Publish script vars
          local socat_pid_file=$(wb publish socat pid-file)
          local socat_stdout_file="$(envjqr 'cacheDir')"/socat/stdout
          local socat_stderr_file="$(envjqr 'cacheDir')"/socat/stderr
          # Checks
          if wb publish socat pid >/dev/null
          then
            fatal "socat is already running, call 'wb publish socat stop' first"
          fi
          # Tunnel vars
          local local_host local_port
          # Call without `local`, needs to fail everything if they fail.
          local_host=$(wb publish socat local-host)
          local_port=$(wb publish socat local-port)
          # SRE vars
          local nomad_token nomad_alloc_id
          # Call without `local`, needs to fail everything if they fail.
          nomad_token=$(wb nomad vault ci nomad-token)
          nomad_alloc_id=$(wb publish nomad id)
          local nomad_exec_cmd="nomad alloc exec \
            -address=$(echo "${nomad_address}" | sed 's/:/\\:/') \
            -namespace=${nomad_namespace} \
            -token ${nomad_token} \
            -i -t=false \
            ${nomad_alloc_id} \
            /bin/socat - TCP4:_prod-database._master.service.eu-central-1.consul:5432"
          socat TCP4-LISTEN:"${local_port}",reuseaddr,fork "EXEC:\"${nomad_exec_cmd}\"" \
          >> "$socat_stdout_file" 2>> "$socat_stderr_file" &
          echo "$!" > "$socat_pid_file"
          # Wait one second and check if socat is running, to check for calling
          # or configuration errors.
          sleep 1
          if ! kill -0 $(cat "$socat_pid_file")
          then
            rm "$socat_pid_file"
            fatal "socat failed to start"
          fi
          # Check if the connection to the postgres server is online
          msg "Waiting for the listening socat port ..."
          local i=0
          local patience=10
          until nc -z "${local_host}" "${local_port}"
          do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge $patience
            then echo
              progress "socat" "$(red FATAL):  workbench:  patience ran out after ${patience}s, ${local_host}:${local_port}"
              rm "$norouter_pid_file"
              fatal "socat startup failed"
            fi
            echo -ne "\b\b\b"
          done >&2
        ;;
        stop )
          # Publish script vars
          local socat_pid_file=$(wb publish socat pid-file)
          # Checks
          if test -f $socat_pid_file
          then
            if ! kill $(cat "$socat_pid_file") 2>&1 >/dev/null
            then
              msg "socat was not running"
            fi
            rm "$socat_pid_file"
          else
            msg "socat is not running"
            false
          fi
        ;;
        pid-file )
          echo "$(envjqr 'cacheDir')"/socat/socat.pid
        ;;
        pid )
          local socat_pid_file=$(wb publish socat pid-file)
          if test -f $socat_pid_file
          then
            # Check if the process is running
            local socat_pid_number=$(cat "${socat_pid_file}")
            if kill -0 "${socat_pid_number}" 2>&1 >/dev/null
            then
              echo "${socat_pid_number}"
            else
              rm "${socat_pid_file}"
              false
            fi
          else
            false
          fi
        ;;
        stdout )
          local socat_stdout_file="$(envjqr 'cacheDir')"/socat/stdout
          if test -f $socat_stdout_file
          then
            cat "${socat_stdout_file}"
          fi
        ;;
        stderr )
          local socat_stderr_file="$(envjqr 'cacheDir')"/socat/stderr
          if test -f $socat_stderr_file
          then
            cat "${socat_stderr_file}"
          fi
        ;;
        * )
          usage_publish
        ;;
      esac
    ;;

    norouter )
      local usage="USAGE: wb publish $op configure|start|stop"
      local action=${1:?$usage}; shift
      # Nomad actions
      case "$action" in
        configure )
          local usage="USAGE: wb publish $op configure [LOCAL_PORT=15432]"
          local local_host=$(wb publish norouter local-host)
          if test $# -gt 0
          then
            local local_port=${1:?$usage}; shift
          else
            local local_port=15432
          fi
          # Checks
          if wb publish norouter pid >/dev/null
          then
            fatal "norouter is already running, call 'wb publish norouter stop' first"
          fi
          # Create config
          # SRE parameters.
          local remote_ip="_prod-database._master.service.eu-central-1.consul"
          local remote_port="5432"
          ## Path to norouter inside container
          local nomad_norouter_bin_path="/bin/norouter"
          # Usage: nomad alloc exec [options] <allocation> <command>
          local nomad_exec_cmd="nomad alloc exec \
            -address=${nomad_address} \
            -namespace=${nomad_namespace} \
            -i -t=false \
            -job ${nomad_job_name} \
            -task ${nomad_task_name} \
            ${nomad_norouter_bin_path}"
          # norouter config file
          cat > "$norouter_config_file" <<- EOF
hosts:
  # host0 is the localhost
  host0:
    vip: "127.0.42.100"
  postgresql:
    cmd: "${nomad_exec_cmd}"
    vip: "${local_host}"
    ports: ["${local_port}:${remote_ip}:${remote_port}"]
EOF
        ;;
        local-host )
          echo "127.0.42.101"
        ;;
        local-port )
          if test -f $norouter_config_file
          then
            yq -r .hosts.postgresql.ports[0] "${norouter_config_file}" | cut -d ":" -f 1
          else
            fatal "norouter is not configured, call 'wb publish norouter configure [LOCAL_PORT=15432]' first"
          fi
        ;;
        start )
          fatal "Sorry, norouter is currently unavailable"
          # Publish script vars
          local norouter_pid_file=$(wb publish norouter pid-file)
          local norouter_stdout_file="$(envjqr 'cacheDir')"/norouter/stdout
          local norouter_stderr_file="$(envjqr 'cacheDir')"/norouter/stderr
          # Checks
          if wb publish norouter pid >/dev/null
          then
            fatal "norouter is already running, call 'wb publish norouter stop' first"
          fi
          # Tunnel vars
          local local_host local_port
          # Call without `local`, needs to fail everything if they fail.
          local_host=$(wb publish norouter local-host)
          local_port=$(wb publish norouter local-port)
          # SRE vars
          local nomad_token
          # Call without `local`, needs to fail everything if they fail.
          nomad_token=$(wb nomad vault ci nomad-token)
          # Start
          ## Using the nomad token:
          ### The SecretID of an ACL token to use to authenticate API requests with.
          ### Overrides the NOMAD_TOKEN environment variable if set.
          NOMAD_TOKEN="${nomad_token}" norouter "$norouter_config_file" >> "$norouter_stdout_file" 2>> "$norouter_stderr_file" &
          echo "$!" > "$norouter_pid_file"
          # Wait one second and check if norouter is running, to check for calling
          # or configuration errors.
          sleep 1
          if ! kill -0 $(cat "$norouter_pid_file")
          then
            rm "$norouter_pid_file"
            fatal "norouter failed to start"
          fi
          # Check if the connection to the postgres server is online
          msg "Waiting for the listening norouter port ..."
          local i=0
          local patience=10
          until nc -z "${local_host}" "${local_port}"
          do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge $patience
            then echo
              progress "norouter" "$(red FATAL):  workbench:  patience ran out after ${patience}s, ${local_host}:${local_port}"
              rm "$norouter_pid_file"
              fatal "norouter startup failed"
            fi
            echo -ne "\b\b\b"
          done >&2
        ;;
        stop )
          # Publish script vars
          local norouter_pid_file=$(wb publish norouter pid-file)
          # Checks
          if test -f $norouter_pid_file
          then
            if ! kill $(cat "$norouter_pid_file") 2>&1 >/dev/null
            then
              msg "norouter was not running"
            fi
            rm "$norouter_pid_file"
          else
            msg "norouter is not running"
            false
          fi
        ;;
        pid-file )
          echo "$(envjqr 'cacheDir')"/norouter/norouter.pid
        ;;
        pid )
          local norouter_pid_file=$(wb publish norouter pid-file)
          if test -f $norouter_pid_file
          then
            # Check if the process is running
            local norouter_pid_number=$(cat "${norouter_pid_file}")
            if kill -0 "${norouter_pid_number}" 2>&1 >/dev/null
            then
              echo "${norouter_pid_number}"
            else
              rm "${norouter_pid_file}"
              false
            fi
          else
            false
          fi
        ;;
        stdout )
          # Publish script vars
          local norouter_stdout_file="$(envjqr 'cacheDir')"/norouter/stdout
          if test -f $norouter_stdout_file
          then
            cat "${norouter_stdout_file}"
          fi
        ;;
        stderr )
          # Publish script vars
          local norouter_stderr_file="$(envjqr 'cacheDir')"/norouter/stderr
          if test -f $norouter_stderr_file
          then
            cat "${norouter_stderr_file}"
          fi
        ;;
        * )
          usage_publish
        ;;
      esac
    ;;

    * )
      usage_publish
      ;;

    esac

}
