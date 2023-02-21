#!/bin/sh

overlay=${1:?USAGE: $0 OVERLAY-NAME}; shift
base=$(mktemp)

./pparamsWithOverlay.sh "$overlay" |
    jq . --sort-keys >$base

diff --unified          \
     --ignore-all-space \
     $base "$@"

rm "$base"
