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
            , hash: $url_parts[2]
            }
          ;

        def group_nicely:
            group_by(.repository)
          | map({
            repository: .[0].repository,
            contributions: map({key: .pkg_id, value: .hash}) | from_entries
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

        def distill_package_data:
            (.url |  split("?")[0] | split("/")) as $url_parts
          | { name: ."pkg-name"
            , version: ."pkg-version"
            , repository: $url_parts[1]
            , hash: $url_parts[2]
            }
          ;

        def is_interesting:
          # change this list to add more packages
          # you can also change the logic of choosing what is 
          ( "ouroboros-consensus"
          , "ouroboros-network"
          , "cardano-ledger-core"
          , "cardano-ledger-conway"
          , "plutus-core"
          ) as $representative_packages
          # you can also change the logic here, so far we select intersting
          # packages by their name
          | . as $input
          | any($representative_packages; . == $input."pkg-name")
          ;

        # key chap packages by package id, so we can match them easily in the next step
          $chap
        | map({ key: pkg_id, value: distill_package_data })
        | from_entries as $package_data

        | $plan."install-plan"
        # select the interesting parts (see logic above)
        | map(select(is_interesting))
        # deduplicate the list per package-id (in a per-component build, the same
        # package id can appear multiple times in an install-plan)
        | unique_by(pkg_id)
        # get package data from chap
        | map($package_data[pkg_id]) as $dependencies

        | { "cardano-node":        $node_rev
          , "cardano-node-branch": $node_branch
          , "cardano-node-status": $node_status
          , "dependencies":        $dependencies
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

        def repo_color($repo):
          { "cardano-node":      "yellow"
          , "ouroboros-network": "white"
          , "cardano-ledger":    "red"
          , "plutus":            "cyan"
          }[$repo] // "off";

        def repo_colorly($repo; $hash):
          colorly(repo_color($repo); $hash[:5]) + $hash[5:];

        def repo_status($status):
          { modified:   colorly("red";  $status)
          , clean:      colorly("cyan"; $status)
          }[$status];

        def repo_comment($manifest; $repo):
          { "cardano-node":
              "(branch \(colorly("yellow"; $manifest."cardano-node-branch")) - \(repo_status($manifest."cardano-node-status")))"
          }[$repo] // "";

          (."cardano-node-package-localisations") as $localisations

        | . as $manifest
        | [ [ "cardano-node"
            , ""
            , repo_colorly("cardano-node"; $manifest."cardano-node")
            , repo_comment($manifest; "cardano-node")
            ]
          , ( $manifest.dependencies[]
            | [ .name
              , .version
              , repo_colorly(.repository; .hash)
              , repo_comment($manifest; .repository)
              ]
            )
          ]
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
