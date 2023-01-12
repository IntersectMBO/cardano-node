#!/usr/bin/env bash

for x in $(find . -name '*.cabal' | grep -v dist-newstyle | cut -c 3-); do
  (
    d=$(dirname $x)
    echo "== $d =="
    cd $d
    cabal check
  )
done
