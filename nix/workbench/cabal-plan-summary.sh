#! /usr/bin/env bash

# for some reason you can't just put a heredoc in a variable...
read -r -d '' USAGE << EOF
cabal-plan-summary.sh PLAN-JSON
  Prints a JSON document giving versions and sources for all the packages (and their components) in the cabal build plan.

  The PLAN-JSON file must exist, you need to run some cabal command that creates it first (e.g. 'cabal build all --dry-run').
EOF

if [ "$#" == "0" ]; then
	echo "$USAGE"
	exit 1
fi

PLAN_FILE=$1

if [[ -f $PLAN_FILE ]]; then
  CABAL_PLAN=$(cat "$PLAN_FILE")
else
  echo "Cabal plan file $PLAN_FILE does not exist, create it (e.g. by running 'cabal build all --dry-run') before starting" >&2
  exit 1
fi

# The main jq filter to get stuff out of plan.json
# - The packages live in `install-plan`
# - Strip "pkg-" prefixes from a few fields
# - Process the `src` field:
#    - If it's a source-repo, then just replace it with the
#      description of the repo
#    - If it's local, strip it out (not interesting)
#    - Otherwise say "unknown" since it could come from anywhere
FILTER='."install-plan"
  | .[]
  | { name: ."pkg-name", version: ."pkg-version", src: ."pkg-src", component: ."component-name" }
  | if .src.type == "source-repo"
    then .src = .src."source-repo"
    else if .src.type == "local"
    then empty
    else .src = "unknown"
    end end'

MANIFEST=$(echo "$CABAL_PLAN" | jq "[$FILTER]")

echo "$MANIFEST"
