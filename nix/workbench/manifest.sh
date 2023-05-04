usage_manifest() {
    usage "manifest" "Manifest" <<EOF
    $(helpcmd collect-from-checkout DIR [NODE_REV=HEAD] [PKGS_TO_MENTION..])
      $(blk c collect)           Collect software manifest from the current 'cardano-node' checkout
    $(helpcmd contributions-by-repository)
      $(blk by byrepo)           Show per-repository list of git hashes involved.
    $(helpcmd render JSON)            Render the JSON manifest.

EOF
}

## WARNING:  Keep in sync with Cardano.Analysis.API.Context.manifestPackages.
##    Better yet, move manifest collection _into_ locli.
##    ..but since the manifest is part of meta.json, that'll cause more thinking.
WB_MANIFEST_PACKAGES=(
    'cardano-node'
    'ouroboros-consensus'
    'ouroboros-network'
    'cardano-ledger-core'
    'plutus-core'
    'cardano-crypto'
    'cardano-prelude'
)

manifest() {
local op=${1:-collect-from-checkout}; if test $# -ge 1; then shift; fi

case "${op}" in
      contributions-by-repository | byrepo | by )
        local usage="USAGE: wb manifest $0"

        jq '
        def pkg_id: ."pkg-name" + "-" + ."pkg-version";
        def repo_hash: .url | split("?")[0];

        def distill_package_data:
            (.url |  split("?")[0] | split("/")) as $url_parts
          | { pkg_id: pkg_id
            , repository: $url_parts[0:2] | join("/")
            , commit: $url_parts[2]
            }
          ;

        def group_nicely:
            group_by(.repository)
          | map({
            repository: .[0].repository,
            contributions: map({key: .pkg_id, value: .commit}) | from_entries
          })
          ;

        # key chap packages by package id, so we can match them easily in the next step
          $chap
        | map({ key: pkg_id, value: distill_package_data })
        | from_entries as $package_origin

        | $plan."install-plan"
        | map(select(."pkg-src".repo.uri == "https://input-output-hk.github.io/cardano-haskell-packages"))
        | unique_by(pkg_id)
        | map($package_origin[pkg_id])
        | group_nicely[]
        ' --null-input                              \
          --argfile plan          $WB_NIX_PLAN      \
          --argfile chap          $WB_CHAP_PACKAGES
        ;;
    collect-from-checkout | collect | c )
        local usage="USAGE: wb manifest $op CARDANO-NODE-CHECKOUT NODE-REV [PACKAGE...]"
        local dir=${1:?$usage}; shift
        local node_rev=${1:-}; shift || true
        local real_dir=$(realpath "$dir")
        local pkgnames=($*)

        if test -z "$node_rev"
        then node_rev=$(manifest_git_head_commit "$dir"); fi

        # TODO put this back
        #, "cardano-node-package-localisations": (. | split("\n") | unique | map(select(. != "")))
        # where . is the output of manifest_cabal_package_localisations "$dir"

        # If we don't have a local plan, it will just use the one that nix would build.
        local local_plan_path="$real_dir/dist-newstyle/cache/plan.json"
        if [[ ! -f "$local_plan_path" ]]; then
          local_plan_path=$WB_NIX_PLAN
        fi

        for pkg in ${pkgnames[*]}; do echo \"$pkg\"; done |
        jq --slurp '
        def pkg_id:
          ."pkg-name" + "-" + ."pkg-version"
          ;

        def pkg_commit:
          .url |  split("?")[0] | split("/")[2]
          ;

        # key chap packages by package id, so we can match them easily in the next step
        .
        # | debug
        | (map({ key: ., value: true }) | from_entries) as $pkgs
        | $chap
        | map({ key: pkg_id, value: pkg_commit })
        | from_entries as $chap_data

        # map each package in the plan to chap data (if present)
        | $plan."install-plan"
        | map({ key: ."pkg-name"
              , value: { name: ."pkg-name"
                       , version: ."pkg-version"
                       , commit: $chap_data[pkg_id]
                       }
              })
        | from_entries as $full_package_data
        | $full_package_data
        | map_values (select(.name | in($pkgs))) as $package_data

        # assemble the manifest
        | $package_data *
          { "cardano-node": { commit: $node_rev
                            , branch: $node_branch
                            , status: $node_status
                            } }
        ' --arg     node_rev      $node_rev                                  \
          --arg     node_branch   $(manifest_local_repo_branch       "$dir") \
          --arg     node_status   $(manifest_git_checkout_state_desc "$dir") \
          --arg     pkgs          $(manifest_git_checkout_state_desc "$dir") \
          --argfile plan          $local_plan_path                           \
          --argfile chap          $WB_CHAP_PACKAGES
        ;;

    render )
        local usage="USAGE: wb manifest $0 MANIFEST-JSON-VALUE"
        local json=${1:?$usage}

        jq 'include "lib";

        def unwords: join(" ");
        def unlines: join("\n");

        def leftpad($n): map([" " * $n] + .);

        def align:
          . as $input
          | transpose
          | map(
              (map(length) | max) as $max_length
            | map(. + " " * ($max_length - length))
          ) | transpose
            | map(unwords)
            | unlines
          ;

        def package_color($repo):
          { "node":    "yellow"
          , "network": "white"
          , "ledger":  "red"
          , "plutus":  "cyan"
          }[$repo] // "off";

        def package_colorly($repo; $hash):
          colorly(package_color($repo); $hash[:5]) + $hash[5:];

        def status_colorly($status):
          { modified:   colorly("red";  $status)
          , clean:      colorly("cyan"; $status)
          }[$status];

        # FIXME put this back
        # (."cardano-node-package-localisations") as $localisations

        to_entries
        | map(
          [ .key
          , .value.version
          , package_colorly(.key; .value.commit)
          , (.value | select(.branch) | colorly("yellow"; .branch))
          , (.value | select(.status) | status_colorly(.status))
          ]
          )
        | leftpad(2)
        | align
        | . + "\n"
        ' --raw-output -L$global_basedir <<<$json
        ;;

    collect-and-report )
        local usage="USAGE: wb manifest $0 CARDANO-NODE-CHECKOUT"
        local dir=${1:-.}; if test $# -ge 1; then shift; fi

        manifest render "$(manifest collect-from-checkout "$dir")";;

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
