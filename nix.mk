bump-cardano-node-workbench:
	nix flake lock --update-input cardano-node-workbench
bump-node-measured:
	nix flake lock --update-input node-measured
bump-cardano-deployment: ## Sync the flake.lock to the CI check
	nix run nixpkgs#nixUnstable -- build .#hydraJobs.cardano-deployment
membench-1:    ## Membench:  one iteration, current commit
	nix build .#membench-node-this-1.batch-report      --out-link result-batch-1-report
membench-1-at: ## Membench:  one iteration, set commit by:  make membench-1-at REV=[master]
	nix build .#membench-node-measured-1.batch-report  --out-link result-batch-1-report --override-input node-measured github:input-output-hk/cardano-node/${REV}
membench-5:    ## Membench:  5 iterations, current commit
	nix build .#membench-node-this-5.batch-report      --out-link result-batch-5-report
membench-5-at: ## Membench:  5 iterations, set commit by:  make membench-5-at REV=[master]
	nix build .#membench-node-this-5.batch-report      --out-link result-batch-5-report --override-input node-measured github:input-output-hk/cardano-node/${REV}

workbench-ci-test smoke: ## Workbench:  test a-la Hydra, the ci-test profile, full Nix engaged
	nix build --out-link result-ci-test '.#hydraJobsPr.linux.native.workbench-ci-test' --cores 0
	ID=`jq -r .meta.tag result-ci-test/meta.json`; test -e "run/$$ID" || mv result-ci-test "run/$$ID"
