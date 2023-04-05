usage_nomad() {
     usage "publish" "Bench data publish" <<EOF
    $(helpcmd dir-path vault\|client\|server\|plugin\|webfs)
                     Gets the corresponding cache directory file path.
    $(helpcmd vault ci\|world login)
                     Login with your GitHub token. First copy the token by doing:
                     Your profile -> Settings -> Developer Settings ->
                     Tokens (Classic) -> Generate New Token (Classic)
                     and create a new token with only the "read:org" permission.
    $(helpcmd vault ci\|world nomad-token)
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
EOF
}

publish_default_op='local'

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
####### dir-path ) #############################################################
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
        * )
          usage_nomad
        ;;
      esac
    ;;
################################################################################
####### vault ) ################################################################
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
            * )
              usage_nomad
            ;;
          esac
        ;;
        * )
          usage_nomad
        ;;
      esac
    ;;
################################################################################
################################################################################
################################################################################
    * )
      usage_nomad
    ;;

  esac

}
