#!/usr/bin/env bash

since="$1"
until="$(date -d "$since +7 days" +%Y-%m-%d)"

echo "# Merge summary for $since to $until"

git log --merges --since $since --until $until --format=fuller \
  | jc --git-log -p \
  | jq -r '
      .[]
      | select(.author != "iohk-bors[bot]")
      | (.subject = (.message | split("\n"))[0])
      | (.body =  (.message | split("\n") | del(.[0]) | del(.[0]) | join("\n")))
      | select(.subject | startswith("Merge pull request #"))
      | (.subject |= sub("^.*#"; ""))
      | ("- [" + (.body | gsub("\n.*"; "")) + "](https://github.com/input-output-hk/cardano-node/pull/" + .subject + ") (" + .author + ")")
    '
