bump-cardano-node-workbench:
	nix flake lock --update-input cardano-node-workbench
bump-node-measured:
	nix flake lock --update-input node-measured
bump-cardano-deployment: ## Sync the flake.lock to the CI check
	nix run nixpkgs#nixUnstable -- build .#hydraJobs.cardano-deployment

CI_TEST_NIXATTR = workbench-ci-test
workbench-ci-test smoke: ## Workbench:  test a-la Hydra, the ci-test profile, full Nix engaged
	nix build --out-link result-ci-test '.#hydraJobs.native.$(CI_TEST_NIXATTR)' --cores 0 ${EXTRA_ARGS}
	ID=`jq -r .meta.tag result-ci-test/meta.json`; test -e "run/$$ID" || mv result-ci-test "run/$$ID"

##  Use the -keep targets to debug failures:
##    /tmp/nix-build-workbench-run-supervisor-ci-test-bage.drv-*
##  ..will contain the run directory.
workbench-ci-test-trace: CI_TEST_NIXATTR = workbench-ci-test-trace
workbench-ci-test-trace: workbench-ci-test

smoke-trace: workbench-ci-test-trace
