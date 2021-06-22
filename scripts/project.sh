#!/usr/bin/env bash

for cabal_project_files in $(find . -name 'cabal.project' | grep  -v dist-newstyle | sort); do
  cabal_project_path="$(dirname "$cabal_project_files")"

  (
    echo "Building in $cabal_project_path"
    cd "$cabal_project_path"
    cabal "$@"
  )
done
