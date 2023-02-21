usage_manifest() {
     usage "manifest" "Manifest" <<EOF
    collect-from-checkout Collect software manifest from the current 'cardano-node' checkout

EOF
}

manifest() {
local op=${1:-collect-from-checkout}; if test $# -ge 1; then shift; fi

case "${op}" in
    collect-from-checkout )
        local usage="USAGE: wb manifest $0 CARDANO-NODE-CHECKOUT"
        local dir=${1:-.}; if test $# -ge 1; then shift; fi
        local node_rev=${1:-$(manifest_git_head_commit "$dir")}
        local real_dir=$(realpath "$dir")

        local args=(
            --slurp --raw-input
            --arg dir               "$real_dir"
            --arg node              "$node_rev"
            --arg node_branch       $(manifest_local_repo_branch          "$dir")
            --arg node_status       $(manifest_git_checkout_state_desc    "$dir")
            --arg ouroboros_network $(manifest_cabal_project_dep_pin_hash "$dir" ouroboros-network)
            --arg cardano_ledger    $(manifest_cabal_project_dep_pin_hash "$dir" cardano-ledger)
            --arg plutus            $(manifest_cabal_project_dep_pin_hash "$dir" plutus)
            --arg cardano_crypto    $(manifest_cabal_project_dep_pin_hash "$dir" cardano-crypto)
            --arg cardano_base      $(manifest_cabal_project_dep_pin_hash "$dir" cardano-base)
            --arg cardano_prelude   $(manifest_cabal_project_dep_pin_hash "$dir" cardano-prelude)
        )
        manifest_cabal_package_localisations "$dir" | jq '
        { "cardano-node":        $node
        , "cardano-node-branch": $node_branch
        , "cardano-node-status": $node_status
        , "cardano-node-package-localisations": (. | split("\n") | unique | map(select(. != "")))
        , "ouroboros-network":   $ouroboros_network
        , "cardano-ledger":      $cardano_ledger
        , "plutus":              $plutus
        , "cardano-crypto":      $cardano_crypto
        , "cardano-base":        $cardano_base
        , "cardano-prelude":     $cardano_prelude
        } as $manifest
        | ($manifest
          | del(."cardano-node-status")
          | del(."cardano-node-branch")
          | del(."cardano-node-package-localisations")
          | to_entries
          | map(if .value | length | (. == 40) then . else
                error ([ "While collecting software manifest from \"\($dir)\":  "
                       , "wrong checkout hash for software component \(.key):  \(.value)"
                       ] | add) end)
          )
        | $manifest
        ' "${args[@]}"
        ;;

    report )
        local usage="USAGE: wb manifest $0 MANIFEST-JSON-VALUE"
        local json=${1:?$usage}

        jq 'include "lib";

        def repo_color($repo):
          { "cardano-node":      "yellow"
          , "ouroboros-network": "white"
          , "cardano-ledger":    "red"
          , "plutus":            "cyan"
          }[$repo] // "off";
        def repo_colorly($repo; $hash):
          colorly(repo_color($repo); $hash[:5]) + $hash[5:];
        def repo_status($status):
          { modified:   colorly("red";   $status)
          , clean:      colorly("cyan"; $status)
          }[$status];
        def repo_comment($manifest; $repo):
          { "cardano-node":
              " (branch \(colorly("yellow"; $manifest."cardano-node-branch")) - \(repo_status($manifest."cardano-node-status")))"
          }[$repo] // "";

          (."cardano-node-package-localisations") as $localisations
        | . as $manifest
          | del(."cardano-node-status")
          | del(."cardano-node-branch")
          | del(."cardano-node-package-localisations")
          | to_entries
          | (map(.key | length) | max | . + 1) as $maxlen
          | map([ "   \(.key): "
                , " " * (.key | $maxlen - length)
                , repo_colorly(.key; .value)
                , repo_comment($manifest; .key)
                , "\n"
                ] | add)
        | . +
          if $localisations == [] then []
          else "\n" +
               "   \(colorly("yellow"; "localised packages")): " +
               "\(colorly("red";    $localisations | join(" ")))"
               | [.]
          end +
          ["\n"]
        | add
        ' --raw-output -L$global_basedir <<<$json
        ;;

    collect-and-report )
        local usage="USAGE: wb manifest $0 CARDANO-NODE-CHECKOUT"
        local dir=${1:-.}; if test $# -ge 1; then shift; fi

        manifest report "$(manifest collect-from-checkout "$dir")";;

    * ) usage_manifest;; esac
}

manifest_git_head_commit() {
    local dir=$1
    if test -d "$dir"/.git
    then git -C "$dir" rev-parse HEAD
    else echo      -n "0000000000000000000000000000000000000000"
    fi
}

manifest_git_checkout_state_desc() {
    local dir=$1
    if test -d "$dir"/.git
    then if git -C "$dir" diff --quiet --exit-code
         then echo -n "clean"
         else echo -n "modified"
         fi
    else echo      -n "not-a-git-checkout"
    fi
}

manifest_cabal_project_dep_pin_hash() {
    # FIXME
    echo 0123456789ABCDEF0123456789ABCDEF01234567
}

manifest_local_repo_branch() {
    local dir=$1 rev=${2:-HEAD}
    if test -d "$dir"/.git
    then git -C "$dir" describe --all "$rev" |
            sed 's_^\(.*/\|\)\([^/]*\)$_\2_'
    else echo      -n "unknown-branch"
    fi
}

manifest_cabal_package_localisations() {
    local dir=$1
    if ! git -C "$dir" diff --exit-code --quiet          -- cabal.project || \
       ! git -C "$dir" diff --exit-code --quiet --staged -- cabal.project
    then
        git -C "$dir" diff --exit-code          -- cabal.project
        git -C "$dir" diff --exit-code --staged -- cabal.project
    fi | grep -F '+    ../' | cut -d/ -f2-3
}
