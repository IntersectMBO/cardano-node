PROJECT_NAME = cardano-node
NUM_PROC     = $(nproc --all)

## One of:  shey alra mary alzo
ERA             ?= alzo

PROFILE ?= default-${ERA}
REV     ?= master
ARGS    ?=
CMD     ?=

help: ## Print documentation
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

stylish-haskell: ## Apply stylish-haskell on all *.hs files
	@find . -type f -name "*.hs" -not -path '.git' -print0 | xargs -0 stylish-haskell -i

cabal-hashes:
	nix run .#checkCabalProject

## TODO: migrate to `nix develop`
cluster-shell: CMD = start-cluster ## Enter Nix shell and start the workbench cluster
cluster-shell:
	nix-shell --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE}

shell-dev:                   ARGS += --arg 'workbenchDevMode' true ## Enter Nix shell, dev mode (workbench run from checkout)
fixed:                       PROFILE = fixed-${ERA}
forge-stress:                PROFILE = forge-stress-${ERA}
forge-stress-plutus:         PROFILE = forge-stress-plutus-${ERA}
forge-stress-oldtracing:     PROFILE = forge-stress-oldtracing-${ERA}
quick:                       PROFILE = quick-${ERA}
quick-oldtracing:            PROFILE = quick-oldtracing-${ERA}
chainsync-byron:             PROFILE = chainsync-early-byron-${ERA}
chainsync-byron-oldtracing:  PROFILE = chainsync-early-byron-oldtracing-${ERA}
chainsync-alonzo:            PROFILE = chainsync-early-alonzo-${ERA}
chainsync-alonzo-oldtracing: PROFILE = chainsync-early-alonzo-oldtracing-${ERA}
forge-stress forge-stress-plutus forge-stress-oldtracing quick quick-oldtracing chainsync-byron chainsync-byron-oldtracing chainsync-alonzo chainsync-alonzo-oldtracing: ARGS += --arg 'workbenchDevMode' true
shell-dev cluster-shell-dev cluster-shell-trace cluster-shell-dev-trace fixed forge-stress forge-stress-plutus forge-stress-oldtracing quick quick-oldtracing chainsync-byron chainsync-byron-oldtracing chainsync-alonzo chainsync-alonzo-oldtracing: shell

test-smoke: smoke ## Build the 'workbench-smoke-test', same as the Hydra job
smoke:
	nix build -f 'default.nix' 'workbench-smoke-test'     --out-link result-smoke-run      --cores 0
test-analysis: smoke-analysis ## Build the 'workbench-smoke-analysis', same as the Hydra job
smoke-analysis:
	nix build -f 'default.nix' 'workbench-smoke-analysis' --out-link result-smoke-analysis --cores 0 --show-trace
ci-analysis:
	nix build -f 'default.nix' 'workbench-ci-analysis'    --out-link result-ci-analysis    --cores 0 --show-trace

list-profiles: ## List workbench profiles
	nix build .#workbench.profile-names-json --json | jq '.[0].outputs.out' -r | xargs jq .
show-profile: ## NAME=profile-name
	@test -n "${NAME}" || { echo 'HELP:  to specify profile to show, add NAME=profle-name' && exit 1; }
	nix build .#all-profiles-json --json --option substitute false | jq '.[0].outputs.out' -r | xargs jq ".\"${NAME}\" | if . == null then error(\"\n###\n### Error:  unknown profile: ${NAME}  Please consult:  make list-profiles\n###\") else . end"
ps: list-profiles

bump-cardano-node-workbench: ## Update the cardano-node-workbench flake input
	nix flake lock --update-input cardano-node-workbench
bump-node-measured: ## Update the node-measured flake input
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

shell: ## Enter Nix shell, CI mode (workbench run from Nix store)
	nix-shell --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE} ${ARGS} ${if ${CMD},--run "${CMD}"}

cli node:
	cabal --ghc-options="+RTS -qn8 -A32M -RTS" build cardano-$@

trace-documentation:
	cabal run -- exe:cardano-node trace-documentation --config 'configuration/cardano/mainnet-config-new-tracing.yaml' --output-file 'doc/new-tracing/tracers_doc_generated.md'

BENCH_REPEATS ?= 3
BENCH_CONFIG ?= both
BENCH_TAG ?= HEAD
BENCH_XARGS ?=

profile-chainsync:
	scripts/mainnet-via-fetcher.sh ${BENCH_XARGS}  --node-config-${BENCH_CONFIG} --repeats ${BENCH_REPEATS} --nix --profile time --tag ${BENCH_TAG}

profile-chainsync-fast: BENCH_XARGS=--skip-prefetch
profile-chainsync-fast: profile-chainsync

clean-profile proclean:
	rm -f *.html *.prof *.hp *.stats *.eventlog

clean: clean-profile
	rm -rf logs/ socket/ cluster.*

full-clean: clean
	rm -rf db dist-newstyle $(shell find . -name '*~' -or -name '*.swp')

cls:
	echo -en "\ec"

.PHONY: cabal-hashes clean cli cls cluster-profiles cluster-shell cluster-shell-dev cluster-shell-dev-trace cluster-shell-trace ghci ghcid help node run-test shell shell-dev stylish-haskell test-ghci test-ghcid test-ghcid-nix
