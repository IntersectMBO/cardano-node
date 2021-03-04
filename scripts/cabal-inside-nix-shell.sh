#!/usr/bin/env bash

case $1 in
    --exit | --undo | --restore )
        git checkout HEAD cabal.project;;
    * )
        sed -ni '1,/--- 8< ---/ p' "$(git rev-parse --show-toplevel)"/cabal.project
esac
