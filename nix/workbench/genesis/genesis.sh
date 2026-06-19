# shellcheck shell=bash
# shellcheck disable=SC2154  # global_basedir is set externally by the sourcing script (wb)

# Resolve genesis backend once (at source time, no output).
# Each backend file defines: profile-cache-key-*, profile-cache-key-input-*,
# genesis-cache-hit-*, genesis-create-cache-*, derive-from-cache-*
# Cache layout/versioning is each backend's concern (jq: layout.version;
# ripper: a version folded into its cache keys). Atomic commit (build in a temp
# dir, mv into place) is shared, in prepare-cache-entry.
if   [[ ${WB_GENESIS_RIPPER:-0}  -eq 1 ]]; then genesis_backend=ripper
elif [[ ${WB_MODULAR_GENESIS:-0} -eq 1 ]]; then genesis_backend=modular
else                                            genesis_backend=jq
fi

usage_genesis() {
  usage "genesis" "Genesis" <<EOF
    $(helpcmd prepare-cache-entry '[--force]' PROFILE-JSON '[CACHEDIR]')
                     Prepare a genesis cache entry for the specified profile.
                       Prints the cache entry path. Regeneration can be $(yellow --force)'d.

    $(helpcmd derive-from-cache PROFILE-JSON CACHE-ENTRY-DIR OUTDIR TIMING-JSON-EXPR)
                     Instantiate genesis from a cache entry

    $(helpcmd genesis-from-preset PRESET-NAME OUTDIR)
                     ($(red DEV)) Prepare genesis for an environment preset,
                       like $(yellow mainnet)

    $(helpcmd create-cache PROFILE-JSON DIR '[GENESIS-CACHE-DIR]')
                     ($(red DEV)) Materialize a complete genesis entry into DIR
EOF
}

genesis() {
set -euo pipefail

local op=${1:-$(usage_genesis)}; shift

case "$op" in

    # Called by: run.sh.
    # [--force]: optional, forces cache regeneration
    # $1: profile JSON file path (e.g. /nix/store/.../profile.json)
    # $2: cache directory (e.g. ~/.cache/cardano-workbench, defaults to envjqr 'cacheDir')
    # Returns: absolute path to the genesis cache entry on stdout
    prepare-cache-entry )
        local usage="USAGE:  wb genesis $op [--force] PROFILE-JSON [CACHEDIR]"

        local force=
        while test $# -gt 0; do
          case "$1" in
            --force ) force=t ;;
            * )       break   ;;
          esac
          shift
        done
        # __REWRITE_GENESIS_CACHE forces regeneration, same as --force.
        if [[ -v __REWRITE_GENESIS_CACHE ]]; then force=t; fi
        local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
        local cacheDir=${2:-$(envjqr 'cacheDir')}

        mkdir -p "$cacheDir"
        if [[ ! -w "$cacheDir" ]]; then
          fatal "profile | allocate failed to create writable cache directory:  $cacheDir"
        fi
        # The genesis cache root: backends own everything under it (their own
        # entry plus, for the ripper, the dataset/ and protocol/ sub-caches).
        local genesis_cache_dir="$cacheDir"/genesis
        mkdir -p "$genesis_cache_dir"

        local cache_key cache_path
        cache_key=$(genesis profile-cache-key "$profile_json")
        if [[ -z "$cache_key" ]]; then
          fatal "no valid profile JSON in $profile_json"
        fi
        cache_path="$genesis_cache_dir/$cache_key"
        if [[ "$(realpath "$genesis_cache_dir")" == "$(realpath "$cache_path")" ]]; then
          fatal "no valid genesis cache key associated with profile in $profile_json"
        fi

        jqtest .genesis.single_shot "$profile_json" ||
            fatal "Incremental (non single-shot) genesis is not supported."

        if profile has-preset "$profile_json"; then
          # Presets are materialized directly: no backend, always refreshed.
          local preset
          preset=$(jq .preset "$profile_json" -r)
          progress "genesis | cache" "preparing preset entry $cache_key  ($cache_path)"
          genesis genesis-from-preset "$preset" "$cache_path"
        elif test -z "$force" && genesis cache-hit "$profile_json" "$genesis_cache_dir"; then
          progress "genesis | cache" "entry $cache_key:  $(red hit) ($cache_path)"
        else
          # Miss or --force: the backend fills a temp dir, then we commit it
          # with an atomic mv (shared by every backend). The ripper's
          # dataset/protocol sub-caches commit themselves independently under
          # genesis_cache_dir.
          progress "genesis | cache" "entry $cache_key:  $(red miss) ($cache_path)"
          local tmpdir
          tmpdir=$(mktemp -d)
          trap 'rm -rf "$tmpdir"' EXIT
          genesis create-cache "$profile_json" "$tmpdir" "$genesis_cache_dir"
          rm -rf "$cache_path"
          mkdir -p "$(dirname "$cache_path")"
          mv "$tmpdir" "$cache_path"
          trap - EXIT
          info genesis "available in $cache_path"
        fi

        echo >&2

        realpath "$cache_path";;

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

    # Called by: genesis.sh prepare-cache-entry.
    # $1: profile JSON.
    # $2: genesis cache root.
    # The backend resolves its own cache key and returns 0 if a valid
    # (current-format) entry exists, 1 otherwise.
    cache-hit )
        "genesis-cache-hit-$genesis_backend" "$@"
        ;;

    # Called by: genesis.sh prepare-cache-entry (into a temp dir) and runner.nix
    # (into its $out).
    # $1: profile JSON.
    # $2: output dir.
    # $3: genesis cache root ($cacheDir/genesis, for the ripper sub-caches).
    # Runs the backend create (genesis files/symlinks + the backend's own
    # marker), then writes the shared cache.key / cache.key.input (derived from
    # the profile).
    # The caller commits $2. Presets never reach here, prepare-cache-entry handles them.
    create-cache )
        local profile_json=${1:?}
        local outdir=${2:?}
        local genesis_cache_dir=${3:-$(envjqr 'cacheDir')/genesis}
        "genesis-create-cache-$genesis_backend" "$profile_json"   "$outdir" "$genesis_cache_dir"
        genesis profile-cache-key-input   "$profile_json" > "$outdir"/cache.key.input
        genesis profile-cache-key         "$profile_json" > "$outdir"/cache.key
        ;;

    # Called by: run.sh.
    # $1: profile JSON file path (run dir's profile.json, e.g. run/current/profile.json).
    # $2: genesis cache entry path (e.g. ~/.cache/cardano-workbench/genesis/ci-test-coay-...).
    # $3: output directory (run dir's genesis/, e.g. run/current/genesis).
    # $4: timing JSON string (from `profile allocate-time`).
    # Profiles with a preset short-circuit here.
    derive-from-cache )
        local usage="USAGE: wb genesis derive-from-cache PROFILE-JSON CACHE-ENTRY-DIR OUTDIR TIMING"
        local profile_json=${1:?$usage}
        local cache_entry=${2:?$usage}
        local outdir=${3:?$usage}
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
    # Per-era zero-spec emitters (compact JSON from genesis/zero/genesis/).
    # ==========================================================================
    #
    # The single access point to the zero preset specs (named after the genesis
    # files: zero-spec-ERA), so backends never touch global_basedir. Two uses:
    #  - null-era stub (conway/dijkstra): cardano-node's parser requires those
    #    genesis files unconditionally; a profile targeting a pre-Chang/pre-Dijkstra
    #    version leaves ".genesis.{conway,dijkstra}" null, so we emit an inert spec
    #    that parses but is never activated (the matching TestConwayHardForkAtEpoch
    #    / TestDijkstraHardForkAtEpoch is omitted, see service/nodes.nix).
    #  - ripper dataset base (shelley/alonzo/conway): the neutral specs the ripper
    #    feeds to create-testnet-data; dataset-cache-ensure injects minUTxOValue /
    #    dRepDeposit into them (these emitters themselves never inject).
    # Output is compact (single line) because the ripper's assembly uses
    # sed 's/}$//'. No byron: it has no create-testnet-data spec input (cli defaults).

    zero-spec-shelley )
      jq -c . "$global_basedir/genesis/zero/genesis/shelley.json"
      ;;

    zero-spec-alonzo )
      jq -c . "$global_basedir/genesis/zero/genesis/alonzo.json"
      ;;

    zero-spec-conway )
      jq -c . "$global_basedir/genesis/zero/genesis/conway.json"
      ;;

    zero-spec-dijkstra )
      jq -c . "$global_basedir/genesis/zero/genesis/dijkstra.json"
      ;;

    * )
      usage_genesis
      ;;

    esac
}

