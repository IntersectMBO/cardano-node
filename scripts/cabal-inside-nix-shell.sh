#!/usr/bin/env bash

sed -ni '1,/--- 8< ---/ p' "$(git rev-parse --show-toplevel)"/cabal.project
