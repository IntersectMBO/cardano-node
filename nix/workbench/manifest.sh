usage_manifest() {
     usage "manifest" "Manifest" <<EOF
    collect-from-checkout Collect software manifest from the current 'cardano-node' checkout

EOF
}

manifest() {
local op=${1:-collect-from-checkout}; if test $# -ge 1; then shift; fi

case "${op}" in
      contributions-by-repository)
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
    collect-from-checkout )
        local usage="USAGE: wb manifest $0 CARDANO-NODE-CHECKOUT"
        local dir=${1:-.}; if test $# -ge 1; then shift; fi
        local node_rev=${1:-$(manifest_git_head_commit "$dir")}
        local real_dir=$(realpath "$dir")

        # TODO put this back
        #, "cardano-node-package-localisations": (. | split("\n") | unique | map(select(. != "")))
        # where . is the output of manifest_cabal_package_localisations "$dir"

        jq '
        def pkg_id:
          ."pkg-name" + "-" + ."pkg-version"
          ;

        def pkg_commit:
          .url |  split("?")[0] | split("/")[2]
          ;

        # key chap packages by package id, so we can match them easily in the next step
          $chap
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
        | from_entries as $package_data

        # assemble the manifest
        # NOTE: The keys where are "components" and are hardcoded in the
        #       reporting framework. What we do here is mapping each component
        #       to a particular package, representative of that component.
        # NOTE: The node component is a bit ad-hoc here since it does not come
        #       from a package. Nevertheless we use the same schema for all
        #       components so we can keep the reporting framework simple.
        | { "node":      { name: "cardano-node"
                         , commit: $node_rev
                         , branch: $node_branch
                         , status: $node_status
                         , version: $package_data["cardano-node"].version
                         }
          , "network":   $package_data["ouroboros-network"]
          , "ledger":    $package_data["cardano-ledger-core"]
          , "plutus":    $package_data["plutus-core"]
          , "crypto":    $package_data["cardano-crypto"]
          , "prelude":   $package_data["cardano-prelude"]
          }
        ' --null-input                                                       \
          --arg     node_rev      "$node_rev"                                \
          --arg     node_branch   $(manifest_local_repo_branch       "$dir") \
          --arg     node_status   $(manifest_git_checkout_state_desc "$dir") \
          --argfile plan          $WB_NIX_PLAN                               \
          --argfile chap          $WB_CHAP_PACKAGES
        ;;

    report )
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
          { "cardano-node":      "yellow"
          , "ouroboros-network": "white"
          , "cardano-ledger":    "red"
          , "plutus":            "cyan"
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
