#shellcheck shell=bash
#
usage_manifest() {
    usage "manifest" "Manifest" <<EOF
    $(helpcmd collect-from-checkout DIR "[NODE_REV=HEAD]" "[PKGS_TO_MENTION..]")
      $(blk c collect)           Collect software manifest from the current 'cardano-node' checkout
    $(helpcmd contributions-by-repository)
      $(blk by byrepo)           Show per-repository list of git hashes involved.
    $(helpcmd render JSON)            Render the JSON manifest.

EOF
}

# WARNING: Keep in sync with Cardano.Analysis.API.Context.manifestPackages.
# Better yet, move manifest collection _into_ locli.
# ..but since the manifest is part of meta.json, that'll cause more thinking.
#
# WB_MANIFEST_PACKAGES is defined here and used in ./wb
# shellcheck disable=SC2034
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
        def pkg_id:
          ."pkg-name" + "-" + ."pkg-version"
          ;

        def is_from_chap:
          ."pkg-src".repo?.uri?
          | strings
          | . == "https://chap.intersectmbo.org/" or startswith("file:" + $chappath)
          ;

        def extract_package_data:
          (.url | split("?")[0] | split("/")) as $url_parts
          | { pkg_id: pkg_id
            , repository: $url_parts[0:2] | join("/")
            , commit: $url_parts[2]
            }
          ;

        # index chap packages by package-id
          $chapfile[0]
        | INDEX(pkg_id) as $chapdata

        # take the packages in the plan that come from chap,
        # match them with chapdata by package-id
        # and extract the data
        | [ JOIN(
              $chapdata;
              $planfile[0]."install-plan"[] | select(is_from_chap);
              pkg_id
            )
          | .[1] # metadata from chap
          | extract_package_data
          ]
        | group_by(.repository)
        | map({
            repository: .[0].repository,
            contributions: INDEX(.pkg_id) | map_values(.commit)
          })
        ' --null-input \
          --slurpfile planfile "$WB_NIX_PLAN" \
          --slurpfile chapfile "$WB_CHAP_PATH/foliage/packages.json" \
          --arg       chappath "$WB_CHAP_PATH"

        ;;

    collect-from-checkout | collect | c )
        local usage="USAGE: wb manifest $op CARDANO-NODE-CHECKOUT NODE-REV [PACKAGE...]"
        local dir=${1:?$usage}; shift
        local node_rev=${1:-}; shift || true
        local real_dir=$(realpath "$dir")

        # If WB_MODE_CABAL is "t", the workbench configured to use a local cabal build, therefore
        # we fetch the plan from cabal's dist-newstyle directory.
        # Otherwise, the workbench is using a nix build, and the location of plan.json is
        # provided by $WB_NIX_PLAN.
        # It is important to distinguish these two cases because there could be a stray
        # dist-newstyle directory.
        local plan_path
        if [[ ${WB_MODE_CABAL:-f} == t ]]; then
            plan_path="$real_dir/dist-newstyle/cache/plan.json"
        else
            plan_path=$WB_NIX_PLAN
        fi

        if test -z "$node_rev"
        then node_rev=$(manifest_git_head_commit "$dir"); fi

        jq '
        def pkg_id:
          ."pkg-name" + "-" + ."pkg-version"
          ;

        def commit_from_chap($chap_data):
            $chap_data[pkg_id]
          # parse the commit from the url
          | .url
          | strings
          | split("?")[0]
          | split("/")[2]
          ;

        def commit_from_srp:
          ."pkg-src"."source-repo".tag
          ;

        # index chap packages by package-id
          $chapfile[0]
        | INDEX(pkg_id) as $chap_data

        # index the plan by package name
        | $planfile[0]."install-plan"
        | INDEX(."pkg-name") as $plan

        # take the list of packages from the command line
        | $ARGS.positional
        # find them in the plan
        | JOIN($plan; .)
        # produce the desired data
        | map(
            .[1] # the plan entry
            | { name: ."pkg-name"
              , version: ."pkg-version"
              , commit: (
                  # if a package is pre-existing we cannot tell where it came from
                  # to simplify, we take the following approach:
                  # - if it is from a source-repo use that
                  # - otherwise see if it matches any package on chap
                  # - otherwise return null
                  # Note: in case a package exists on both hackage and chap, this
                  # will always report the commit from chap
                  commit_from_srp // commit_from_chap($chap_data) // null
                )
              }
          )
        # index again by package name
        | INDEX(.name)
        # add cardano-node data
        * { "cardano-node": { commit: $node_rev
                            , branch: $node_branch
                            , status: $node_status
                            } }
        ' --null-input \
          --arg       node_rev      "$node_rev" \
          --arg       node_branch   "$(manifest_local_repo_branch       "$dir")" \
          --arg       node_status   "$(manifest_git_checkout_state_desc "$dir")" \
          --arg       pkgs          "$(manifest_git_checkout_state_desc "$dir")" \
          --slurpfile planfile      "$plan_path" \
          --slurpfile chapfile      "$WB_CHAP_PATH/foliage/packages.json" \
          --args                    "$@"
        ;;

    render )
        local usage="USAGE: wb manifest $0 MANIFEST-JSON-VALUE"
        local json=${1:?$usage}

        jq '
        def color:
          { red:     "\u001b[31m"
          , green:   "\u001b[32m"
          , yellow:  "\u001b[33m"
          , blue:    "\u001b[34m"
          , magenta: "\u001b[35m"
          , cyan:    "\u001b[36m"
          , white:   "\u001b[37m"
          , off:     "\u001b[39m"
          };

        def colorly($col; $x):
          "\(color[$col])\($x)\(color["off"])";

        def drop_nulls:
          map(select(. != null));

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
