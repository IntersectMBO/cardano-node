#!/bin/sh

base=$(mktemp)

./epochPParams.sh |
    jq . --sort-keys >$base

diff --unified          \
     --ignore-all-space \
     $base "$@"

rm "$base"
