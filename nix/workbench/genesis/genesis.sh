# shellcheck shell=bash
# shellcheck disable=SC2154  # global_basedir is set externally by the sourcing script (wb)

global_genesis_format_version=June-22-2026

# Resolve genesis backend once (at source time, no output).
# Each backend file defines: profile-cache-key-*, profile-cache-key-input-*,
# genesis-create-*, derive-from-cache-*
if   [[ ${WB_GENESIS_RIPPER:-0}  -eq 1 ]]; then genesis_backend=ripper
elif [[ ${WB_MODULAR_GENESIS:-0} -eq 1 ]]; then genesis_backend=modular
else                                            genesis_backend=jq
fi

usage_genesis() {
  usage "genesis" "Genesis" <<EOF
    $(helpcmd prepare-cache-entry [--force] PROFILE-JSON CACHEDIR NODE-SPECS OUTDIR)
                     Prepare a genesis cache entry for the specified profile.
                       Cache entry regeneration can be $(yellow --force)'d

    $(helpcmd derive-from-cache PROFILE-DIR TIMING-JSON-EXPR CACHE-ENTRY-DIR OUTDIR)
                     Instantiate genesis from a cache entry

    $(helpcmd genesis-from-preset PRESET-NAME OUTDIR)
                     ($(red DEV)) Prepare genesis for an environment preset,
                       like $(yellow mainnet)

    $(helpcmd actually-genesis PROFILE-JSON NODE-SPECS OUTDIR CACHE-KEY-INPUT CACHE-KEY)
                     ($(red DEV)) Internal procedure to actually generate genesis
EOF
}

genesis() {
set -euo pipefail

local op=${1:-$(usage_genesis)}; shift

if   [[ $WB_GENESIS_RIPPER -eq 1 ]]; then
    info genesis "$(red using ripper backend)"
elif [[ $WB_MODULAR_GENESIS -eq 1 ]]; then
    info genesis "$(red using modular configuration)"
fi

case "$op" in

    # Called by: run.sh (line 433)
    # [--force]: optional, forces cache regeneration
    # $1: profile JSON file path (e.g. /nix/store/.../workbench-profile-data-ci-test-coay/profile.json)
    # $2: node-specs JSON file path (e.g. /nix/store/.../workbench-profile-data-ci-test-coay/node-specs.json)
    # $3: cache directory (e.g. ~/.cache/cardano-workbench, defaults to envjqr 'cacheDir')
    # Returns: absolute path to the genesis cache entry on stdout
    prepare-cache-entry )
        local usage="USAGE:  wb genesis $op [--force] PROFILE-JSON NODE-SPECS OUTDIR CACHEDIR"

        local regenesis_causes=()
        while test $# -gt 0; do
          case "$1" in
            --force )
              regenesis_causes+=('has--force')
              ;;
            * )
              break
              ;;
          esac
          shift
        done

        local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
        local node_specs=${2:-$WB_SHELL_PROFILE_DATA/node-specs.json}
        local cacheDir=${3:-$(envjqr 'cacheDir')}

        local cache_key_input cache_key cache_path

        mkdir -p "$cacheDir"
        if [[ ! -w "$cacheDir" ]]; then
          fatal "profile | allocate failed to create writable cache directory:  $cacheDir"
        fi

        cache_key_input=$(genesis profile-cache-key-input "$profile_json")
        if [[ -z "$cache_key_input" ]]; then
          fatal "no valid profile JSON in $profile_json"
        fi
        cache_key=$(genesis profile-cache-key "$profile_json")
        cache_path=$cacheDir/genesis/$cache_key
        if [[ "$(realpath "$cacheDir"/genesis)" == "$(realpath "$cache_path")" ]]; then
          fatal "no valid genesis cache key associated with profile in $profile_json"
        fi

        if genesis cache-test "$cache_path"; then
          cache_hit=t
          cache_hit_desc='hit'
        else
          cache_hit=
          cache_hit_desc='miss'
        fi

        progress "genesis | cache"  "preparing entry $cache_key:  $(red $cache_hit_desc) ($cache_path)"

        if [[ -z "$cache_hit" ]]; then
          regenesis_causes+=('cache-miss')
        elif [[ -v __REWRITE_GENESIS_CACHE ]]; then
          regenesis_causes+=('__REWRITE_GENESIS_CACHE-env-var-defined')
        fi

        local preset

        if [[ -n "${regenesis_causes[*]}" ]]; then
          info genesis "generating due to ${regenesis_causes[*]}:  $cache_key @$cache_path"

          jqtest .genesis.single_shot "$profile_json" ||
              fatal "Incremental (non single-shot) genesis is not supported."

          if profile has-preset "$profile_json"; then
            preset=$(jq .preset "$profile_json" -r)
            genesis genesis-from-preset "$preset" "$cache_path"
          else
            genesis actually-genesis "$profile_json" "$node_specs" "$cache_path" "$cache_key_input" "$cache_key"
          fi
        fi

        echo >&2

        realpath "$cache_path";;

    cache-test )
        local usage="USAGE:  wb genesis $op GENESIS-CACHE-DIR"
        local cache_dir=${1:?$usage}
        local version

        version=$(cat "$cache_dir"/layout.version 2>/dev/null || echo "unknown")

        if [[ ! "$version" == "$global_genesis_format_version" ]]; then
          info genesis "cache entry at $cache_dir is incompatible:  layout version '$version' does not match current: $global_genesis_format_version"
          return 1;
        fi
        return 0
        ;;

    genesis-from-preset )
        local usage="USAGE: wb genesis $op PRESET GENESIS-DIR"
        local preset=${1:?$usage}
        local dir=${2:?$usage}

        info genesis "profile uses preset genesis: $preset"

        rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
        mkdir -p "$dir"

        # Copy preset files into the cache entry, normalizing the source.
        cp -f "$(profile preset-get-file "$preset" 'genesis file' genesis/byron/genesis.json)"   "$dir/genesis.byron.json"
        cp -f "$(profile preset-get-file "$preset" 'genesis file' genesis/genesis-shelley.json)" "$dir/genesis.shelley.json"
        cp -f "$(profile preset-get-file "$preset" 'genesis file' genesis/genesis.alonzo.json)"  "$dir/genesis.alonzo.json"
        cp -f "$(profile preset-get-file "$preset" 'genesis file' genesis/genesis.conway.json)"  "$dir/genesis.conway.json"
        ;;

    actually-genesis )
        local usage="USAGE: wb genesis $op PROFILE-JSON NODE-SPECS DIR"
        local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
        local node_specs=${2:-$WB_SHELL_PROFILE_DATA/node-specs.json}
        local dir=${3:-$(mktemp -d)}
        local cache_key_input=${4:-$(genesis profile-cache-key-input "$WB_SHELL_PROFILE_DATA"/profile.json)}
        local cache_key=${5:-$(genesis profile-cache-key "$WB_SHELL_PROFILE_DATA"/profile.json)}

        progress "genesis" "new one: $(yellow profile) $(blue "$profile_json") $(yellow node_specs) $(blue "$node_specs") $(yellow dir) $(blue "$dir") $(yellow cache_key) $(blue "$cache_key") $(yellow cache_key_input) $(blue "$cache_key_input")"

        genesis create "$profile_json" "$dir"

        info genesis "sealing"
        cat <<<"$cache_key_input"               > "$dir"/cache.key.input
        cat <<<"$cache_key"                     > "$dir"/cache.key
        cat <<<"$global_genesis_format_version" > "$dir"/layout.version
        ;;

    # -- Dispatched to backend -------------------------------------------------

    # Called by: genesis.sh prepare-cache-entry.
    # $1: profile JSON file path (e.g. /nix/store/.../profile.json).
    # Returns: JSON or string written to cache.key.input for debugging.
    # Profiles with a preset short-circuit here to the preset name.
    profile-cache-key-input )
        local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
        if profile has-preset "$profile_json"; then
          jq -r .preset "$profile_json"
        else
          "profile-cache-key-input-$genesis_backend" "$@"
        fi
        ;;

    # Called by: genesis.sh prepare-cache-entry.
    # $1: profile JSON file path (e.g. /nix/store/.../profile.json).
    # Returns: short cache directory name (e.g. "ci-test-coay-1c5c3ba-8444286").
    # Profiles with a preset short-circuit here to "preset-<name>".
    profile-cache-key )
        local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
        if profile has-preset "$profile_json"; then
          echo "preset-$(jq -r .preset "$profile_json")"
        else
          "profile-cache-key-$genesis_backend" "$@"
        fi
        ;;

    # Called by: genesis.sh (actually-genesis).
    # $1: profile JSON file path.
    # $2: output directory.
    # Creates the genesis files in the directory.
    # Profiles with a preset never reach here, prepare-cache-entry handles it.
    create )
        "genesis-create-$genesis_backend" "$@"
        ;;

    # Called by: run.sh.
    # $1: profile JSON file path (run dir's profile.json, e.g. run/current/profile.json).
    # $2: timing JSON string (from `profile allocate-time`).
    # $3: genesis cache entry path (e.g. ~/.cache/cardano-workbench/genesis/ci-test-coay-...).
    # $4: output directory (run dir's genesis/, e.g. run/current/genesis).
    # Profiles with a preset short-circuit here.
    derive-from-cache )
        local usage="USAGE: wb genesis derive-from-cache PROFILE-JSON TIMING CACHE-ENTRY OUTDIR"
        local profile_json=${1:?$usage}
        local cache_entry=${3:?$usage}
        local outdir=${4:?$usage}
        # Skip the backend and copy genesis*.json from the cache entry into $outdir.
        if profile has-preset "$profile_json"; then
          local preset
          preset=$(jq -r .preset "$profile_json")
          mkdir -p "$outdir"
          progress "genesis" "instantiating from preset $(with_color white "$preset"):  $cache_entry"
          cp -f "$cache_entry"/genesis*.json "$outdir"
          return
        fi
        "derive-from-cache-$genesis_backend" "$@"
        ;;

    # ==========================================================================
    # Shared protocol stubs for eras the profile leaves null.
    # ==========================================================================
    #
    # cardano-node's config parser requires ConwayGenesisFile and
    # DijkstraGenesisFile unconditionally. Profiles that, for example, target a
    # pre-Chang (or pre-Dijkstra) protocol version leave ".genesis.conway" or
    # ".genesis.dijkstra" null on purpose.
    # The stubs below satisfy the parser independent of the activation of those
    # eras: the workbench must omit the matching TestConwayHardForkAtEpoch and
    # TestDijkstraHardForkAtEpoch from the node config in that case (see
    # nix/workbench/service/nodes.nix), so the stub content is loaded but never
    # as live parameters.
    #
    # Stubs are sourced from the zero preset (single canonical source of truth
    # for "schema-valid but inert" genesis content). Output is compact (single
    # line) because the ripper backend's assembly step uses sed 's/}$//' to
    # strip the trailing brace before splicing in the dataset side
    # (initialDReps for conway).

    conway-stub-spec )
      jq -c . "$global_basedir/genesis/zero/genesis/conway.json"
      ;;

    dijkstra-stub-spec )
      jq -c . "$global_basedir/genesis/zero/genesis/dijkstra.json"
      ;;

    * )
      usage_genesis
      ;;

    esac
}

