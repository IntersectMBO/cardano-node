#!/usr/bin/env bash

if ! { [ "${BUILDKITE_BRANCH:-}" == "master" ] || [[ "${BUILDKITE_BRANCH:-}" = release* ]]; }; then
  exit 0;
else
  nix run .\#dockerImages/push --override-input hostNixpkgs "path:$(nix eval --impure -I $NIX_PATH --expr '(import <nixpkgs> {}).path')"
fi
