#!/usr/bin/env bash

if [ $# -eq 0 ]; then
    >&2 echo "gen-merge-summary.sh [SINCE] [UNTIL]"
    >&2 echo "Generate a merge summary between the SINCE and UNTIL date"
    >&2 echo ""
    >&2 echo "Examples:"
    >&2 echo "  gen-merge-summary.sh 2023-01-01 2023-01-08"
    >&2 echo "  gen-merge-summary.sh 2023-01-01 '2023-01-01 +7 days'"
    exit 1
fi

since="$(date -d "$1" +%Y-%m-%d)"
until="$(date -d "$2" +%Y-%m-%d)"

echo "# Merge summary for $since to $until"

git log --merges --since $since --until $until --format=fuller \
  | jc --git-log -p \
  | jq -r '
      .[]
      | select(.author != "iohk-bors[bot]")
      | (.subject = (.message | split("\n"))[0])
      | (.body =  (.message | split("\n") | del(.[0]) | del(.[0]) | join("\n")))
      | select(.subject | startswith("Merge pull request #"))
      | (.subject |= sub("^.*#(?<a>[0-9]+) .*$"; .a))
      | ("- [" + (.body | gsub("\n.*"; "")) + "](https://github.com/intersectmbo/cardano-node/pull/" + .subject + ") (" + .author + ")")
    '
