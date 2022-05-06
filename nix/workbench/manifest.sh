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
        local real_dir=$(realpath "$dir")

        local args=(
            --null-input
            --arg dir               "$real_dir"
            --arg node              $(manifest_git_head_commit            "$dir")
            --arg node_branch       $(manifest_local_repo_branch          "$dir")
            --arg node_status       $(manifest_git_checkout_state_desc    "$dir")
            --arg ouroboros_network $(manifest_cabal_project_dep_pin_hash "$dir" ouroboros-network)
            --arg cardano_ledger    $(manifest_cabal_project_dep_pin_hash "$dir" cardano-ledger)
            --arg plutus            $(manifest_cabal_project_dep_pin_hash "$dir" plutus)
            --arg cardano_crypto    $(manifest_cabal_project_dep_pin_hash "$dir" cardano-crypto)
            --arg cardano_base      $(manifest_cabal_project_dep_pin_hash "$dir" cardano-base)
            --arg cardano_prelude   $(manifest_cabal_project_dep_pin_hash "$dir" cardano-prelude)
        )
        jq '
        { "cardano-node":        $node
        , "cardano-node-branch": $node_branch
        , "cardano-node-status": $node_status
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

        msg "component manifest:"
        jq '
        . as $manifest
        | ($manifest
          | del(."cardano-node-status")
          | del(."cardano-node-branch")
          | to_entries
          | (map(.key | length) | max | . + 1) as $maxlen
          | map( (if .key == "cardano-node" then " (branch \($manifest."cardano-node-branch"), \($manifest."cardano-node-status"))" else "" end)
                 as $mod
               | "   \(.key): \(" " * (.key | $maxlen - length))\(.value)\($mod)\n")
          | add
          )
        ' --raw-output <<<$json
        ;;

    * ) usage_manifest;; esac
}

manifest_git_head_commit() {
    local dir=$1
    git -C "$dir" rev-parse HEAD
}

manifest_git_checkout_state_desc() {
    local dir=$1
    if git -C "$dir" diff --quiet --exit-code
    then echo -n "clean"
    else echo -n "modified"
    fi
}

manifest_cabal_project_dep_pin_hash() {
    local project_file=$1/cabal.project
    local dep=$2
    grep "^[ ]*location: .*/${dep}\$" "${project_file}" -A1 \
        | tail -n-1 | sed 's/^.* tag: //'
}

manifest_local_repo_branch() {
        local dir=$1 rev=${2:-HEAD}
        git -C "$dir" describe --all "$rev" |
                sed 's_^\(.*/\|\)\([^/]*\)$_\2_'
}
