#!/bin/bash

cabal -v0 run cardano-profile names-noera | jq -r .[] | xargs -n 1 sh -c "nix-shell -A \"workbench-shell\" --max-jobs 256 --cores 256 --quiet --argstr profileName \"\$0-coay\" --argstr backendName supervisor --command \"mkdir -p /tmp/supervisor/\$0-coay/{profile,backend}; cp -r \$WB_SHELL_PROFILE_DATA/* /tmp/supervisor/\$0-coay/profile/; cp -r \$WB_BACKEND_DATA/* /tmp/supervisor/\$0-coay/backend/;\" || exit 255"

# Makefile
# test-supervisor:
#        jq -r .[] all-profiles-names.noera.json | grep --invert-match "\-nomadperf" | xargs -n 1 sh -c 'nix-shell -A "workbench-shell" --max-jobs 256 --cores 256 --quiet --argstr profileName "$$0-coay" --argstr backendName supervisor --command "mkdir -p /tmp/supervisor/$$0-coay/{profile,backend}; cp -r \$$WB_SHELL_PROFILE_DATA/* /tmp/supervisor/$$0-coay/profile/; cp -r \$$WB_BACKEND_DATA/* /tmp/supervisor/$$0-coay/backend/;" || exit 255'
# test-nomadcloud:
#        jq -r .[] all-profiles-names.noera.json | grep                "\-nomadperf" | xargs -n 1 sh -c 'nix-shell -A "workbench-shell" --max-jobs 256 --cores 256 --quiet --argstr profileName "$$0-coay" --argstr backendName nomadcloud --command "mkdir -p /tmp/nomadcloud/$$0-coay/{profile,backend}; cp -r \$$WB_SHELL_PROFILE_DATA/* /tmp/nomadcloud/$$0-coay/profile/; cp -r \$$WB_BACKEND_DATA/* /tmp/nomadcloud/$$0-coay/backend/;" || exit 255'
