bump-cardano-node-workbench:
	nix flake lock --update-input cardano-node-workbench
bump-node-measured:
	nix flake lock --update-input node-measured
bump-cardano-deployment: ## Sync the flake.lock to the CI check
	nix run nixpkgs#nixUnstable -- build .#hydraJobs.cardano-deployment
membench-1:    ## Membench:  one iteration, current commit
	nix build .#membench-node-this-1.batch-report      --out-link result-batch-1-report
membench-1-at: ## Membench:  one iteration, set commit by:  make membench-1-at REV=[master]
	nix build .#membench-node-measured-1.batch-report  --out-link result-batch-1-report --override-input node-measured github:intersectmbo/cardano-node/${REV}
membench-5:    ## Membench:  5 iterations, current commit
	nix build .#membench-node-this-5.batch-report      --out-link result-batch-5-report
membench-5-at: ## Membench:  5 iterations, set commit by:  make membench-5-at REV=[master]
	nix build .#membench-node-this-5.batch-report      --out-link result-batch-5-report --override-input node-measured github:intersectmbo/cardano-node/${REV}

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
