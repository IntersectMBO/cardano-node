usage_manifest() {
     usage "manifest" "Manifest" <<EOF
    collect-from-checkout Collect software manifest from the current 'cardano-node' checkout

EOF
}

# We need an explicit list of packages that we are interested
# in, since packages from CHaP are indistinguishable from packages
# from Hackage, and we don't want to get information about all of the
# Hackage packages also!
plutus_pkgs=(
  plutus-core 
  plutus-ledger-api
)
ledger_pkgs=(
  cardano-ledger-byron 
  cardano-ledger-shelley 
  cardano-ledger-shelley-ma 
  cardano-ledger-alonzo 
  cardano-ledger-babbage 
  cardano-ledger-conway
  cardano-ledger-api
  cardano-ledger-binary 
  cardano-ledger-core 
  cardano-ledger-pretty
  cardano-protocol-tpraos
  cardano-data
  ledger-state
  vector-map
  small-steps
  set-algebra
  non-integral
)
ouroboros_pkgs=(
  ouroboros-consensus-byron 
  ouroboros-consensus-cardano 
  ouroboros-consensus-protocol 
  ouroboros-consensus-shelley 
  ouroboros-consensus 
  ouroboros-network-framework 
  ouroboros-network
)
manifest_pkgs=("${plutus_pkgs[@]}" "${ledger_pkgs[@]}" "${ouroboros_pkgs[@]}")

manifest() {
local op=${1:-collect-from-checkout}; if test $# -ge 1; then shift; fi

case "${op}" in
    collect-from-checkout )
        local usage="USAGE: wb manifest $0 CARDANO-NODE-CHECKOUT"
        local dir=${1:-.}; if test $# -ge 1; then shift; fi
        local node_rev=${1:-$(manifest_git_head_commit "$dir")}
        local real_dir=$(realpath "$dir")

        # Get the plan summary
        pushd "$real_dir" > /dev/null
        # make sure there's a plan.json for cabal-plan-summary to read
        cabal build all --dry-run >&2
        popd > /dev/null

        plan_summary=$($(dirname $0)/cabal-plan-summary.sh "$real_dir")

        # Construct a filter that filters out entries that aren't in the list of pkgs,
        # and then construct the list of pkgs as a json object to pass to jq
        pkg_filter='select(.name | IN( $pkgs[] ))'
        pkgs_json=$(jq --compact-output --null-input '$ARGS.positional' --args -- "${manifest_pkgs[@]}")

        # Groups the info together so it's a map from packages to info.
        #
        # Note that this is only safe to do if we assume that:
        # - A given package component only appears once in the plan (not sure this is guaranteed!)
        # - The version is the same across all components of a package (not sure this is guaranteed either!)
        # However, it makes the information much more readable, so we'll do it for now.
        #
        # We do this in a dumb way:
        # - Rewrite '{ component: foo }' to '{ components: { foo: true } }' so jq will merge the components
        # - Group by name
        # - Merge each group (the components merge thanks to the rewrite above)
        # - Turn the silly component object back into an array of its keys
        grouper='map(.components = { (.component): true })
          | map(del(.component))
          | group_by(.name) 
          | map(reduce .[] as $i ({}; . *= $i)) 
          | map(.components = (.components | keys))'

        # Run everything through the pkg_filter, then gather it back into a list
        # to go through the grouper
        combined_filter="[.[] | $pkg_filter] | $grouper"

        echo "$plan_summary" | jq --argjson pkgs "$pkgs_json" "$combined_filter"
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
