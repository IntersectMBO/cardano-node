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

test-smoke: smoke ## Build the 'workbench-smoke-test' flake attr, same as the Hydra job
smoke:
	nix build -f 'default.nix' 'workbench-smoke-test'     --out-link result-smoke-run      --cores 0

test-analysis: smoke-analysis ## Build the 'workbench-smoke-analysis' flake attr, same as the Hydra job
smoke-analysis:
	nix build -f 'default.nix' 'workbench-smoke-analysis' --out-link result-smoke-analysis --cores 0 --show-trace

ci-analysis:
	nix build -f 'default.nix' 'workbench-ci-analysis'    --out-link result-ci-analysis    --cores 0 --show-trace
