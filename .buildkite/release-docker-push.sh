#!/usr/bin/env bash

if ! { [ "${BUILDKITE_BRANCH:-}" == "master" ] || [[ "${BUILDKITE_BRANCH:-}" = release* ]]; }; then
  exit 0;
else
  nix-build .buildkite/docker-build-push.nix -o docker-build-push
  ./docker-build-push
fi
