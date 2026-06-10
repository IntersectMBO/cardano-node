#!/bin/bash

cabal -v0 run cardano-profile names | jq -r .[] | xargs -n 1 sh -c "nix-shell -A \"workbench-shell\" --max-jobs 8 --cores 0 --quiet --argstr profileName \"\$0\" --argstr eraName conway --argstr backendName supervisor --command \"mkdir -p /tmp/supervisor/\$0/{profile,backend}; cp -r \$WB_SHELL_PROFILE_DATA/* /tmp/supervisor/\$0/profile/; cp -r \$WB_BACKEND_DATA/* /tmp/supervisor/\$0/backend/;\" || exit 255"

# Makefile
# test-supervisor:
#        jq -r .[] all-profiles-names.json | grep --invert-match "\-nomadperf" | xargs -n 1 sh -c 'nix-shell -A "workbench-shell" --max-jobs 8 --cores 0 --quiet --argstr profileName "$$0" --argstr eraName conway --argstr backendName supervisor --command "mkdir -p /tmp/supervisor/$$0/{profile,backend}; cp -r \$$WB_SHELL_PROFILE_DATA/* /tmp/supervisor/$$0/profile/; cp -r \$$WB_BACKEND_DATA/* /tmp/supervisor/$$0/backend/;" || exit 255'
# test-nomadcloud:
#        jq -r .[] all-profiles-names.json | grep                "\-nomadperf" | xargs -n 1 sh -c 'nix-shell -A "workbench-shell" --max-jobs 8 --cores 0 --quiet --argstr profileName "$$0" --argstr eraName conway --argstr backendName nomadcloud --command "mkdir -p /tmp/nomadcloud/$$0/{profile,backend}; cp -r \$$WB_SHELL_PROFILE_DATA/* /tmp/nomadcloud/$$0/profile/; cp -r \$$WB_BACKEND_DATA/* /tmp/nomadcloud/$$0/backend/;" || exit 255'
