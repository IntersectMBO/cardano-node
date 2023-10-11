#!/usr/bin/env bash
#
# Find the path to the built executable in the cabal plan. For example:
#
# ./scripts/bin-path.sh cardano-node
#
# Must be executed from the repository root

if [[ -z "$1" ]]
then
  echo "This script expects exactly one argument: the name of the executable whose path is searched for."
  echo "For example pass \"cardano-node\""
  exit 1
fi

[[ -e "scripts" ]] || { echo "This script must be executed from the repository's root (i.e. execute ./scripts/bin-path.sh)"; exit 1; }

exe="$1"

if which jq > /dev/null; then
  bin="$(jq -r '."install-plan"[] | select(."component-name" == "exe:'$exe'") | ."bin-file"' dist-newstyle/cache/plan.json | head -n 1)"

  if [ -f "$bin" ]; then
    echo "$bin"
  else
    echo "Error: $exe not built" 1>&2
    exit 1
  fi
else
  echo "Error: jq not installed" 1>&2
  exit 1
fi
