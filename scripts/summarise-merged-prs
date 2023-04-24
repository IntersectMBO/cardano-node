#!/usr/bin/env bash

set -euo pipefail

(yq --version | grep https://github.com/mikefarah/yq/ > /dev/null) || {
  echo "Please install yq from https://github.com/mikefarah/yq/" > /dev/stderr
  exit 1
}

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 repository output_subdir" >&2
  echo "Example: $0 input-output-hk/cardano-node v8.0.0" >&2
  exit 1
fi

repository="$1"
subdir="$2"

work_dir="gen/$repository"
work_subdir="$work_dir/$subdir"
detail_dir="$work_subdir/detail"
summary_dir="$work_subdir/summary"

mkdir -p "$work_dir"
mkdir -p "$work_subdir"
mkdir -p "$detail_dir"
mkdir -p "$summary_dir"

(yq --version | grep https://github.com/mikefarah/yq/ > /dev/null) || {
  echo "Please install yq from https://github.com/mikefarah/yq/" > /dev/stderr
  exit 1
}

for path in "$detail_dir"/*.yaml; do
  echo "$path"
  file="$(basename $path)"
  target="$summary_dir/${file%.yaml}.md"
  rm -f "$target"

  for pr in $(
      cat "$detail_dir/$file" \
        | yq -o json \
        | jq '
              .[]
            | select(.include or .include == "undecided")
            | .number
          '); do
    tmp_file="$(mktemp).json"
    cat "$detail_dir/$file" | yq -o json | jq '.[] | select(.number == '$pr')' > "$tmp_file"
    cat "$tmp_file" | jq -r '"- [\(.title)](\(.url)) by \(.author)"' >> "$target"
  done
done
