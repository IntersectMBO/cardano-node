PROJECT_NAME = cardano-node
NUM_PROC     = $(nproc --all)

## One of:  shey alra mary alzo
ERA             ?= alzo

CLUSTER_PROFILE ?= default-${ERA}
ifneq "${CLUSTER_PROFILE}" "default-${ERA}"
$(warning DEPRECATED:  CLUSTER_PROFILE is deprecated, please use PROFILE)
endif

PROFILE ?= ${CLUSTER_PROFILE}
REV     ?= master
ARGS    ?=


help: ## Print documentation
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

stylish-haskell: ## Apply stylish-haskell on all *.hs files
	@find . -type f -name "*.hs" -not -path '.git' -not -path '*.stack-work*' -print0 | xargs -0 stylish-haskell -i

cabal-hashes:
	nix run .#checkCabalProject

ghci: ## Run repl
	@stack ghci $(PROJECT_NAME):lib --haddock-deps --ghci-options=-fobject-code --nix

ghcid:  ## Run ghcid
	@ghcid --command "stack ghci $(PROJECT_NAME):lib --nix -j12 --ghci-options=-fobject-code"

run-test: ## Build & run test
	@stack build --fast --nix && \
	stack test --fast --nix

test-ghci: ## Run repl on test suites
	@stack ghci $(PROJECT_NAME):lib $(PROJECT_NAME):test:$(PROJECT_NAME)-test --ghci-options=-fobject-code --nix

test-ghcid: ## Run ghcid on test suites
	@ghcid --command "stack ghci --nix $(PROJECT_NAME):lib $(PROJECT_NAME):test:$(PROJECT_NAME)-test --ghci-options=-fobject-code"

test-ghcid-nix: ## Run ghcid on test suites with Nix
	@ghcid --command="stack ghci --test --main-is $(PROJECT_NAME):test:$(PROJECT_NAME)-test --nix -j$(NUM_PROC)"

bench-chainsync: PROFILE=chainsync-${ERA}
bench-chainsync: cluster-shell-dev ## Enter Nix shell and start the chainsync benchmark

## TODO: migrate to `nix develop`
cluster-shell: ## Enter Nix shell and start the workbench cluster
	nix-shell --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE} --arg 'autoStartCluster' true

shell-dev:               ARGS += --arg 'workbenchDevMode' true ## Enter Nix shell, dev mode (workbench run from checkout)
cluster-shell:           ARGS += --arg 'autoStartCluster' true ## Enter Nix shell, and start workbench cluster
cluster-shell-dev:       ARGS += --arg 'autoStartCluster' true --arg 'workbenchDevMode' true ## Enter Nix shell, dev mode, and start workbench cluster
cluster-shell-trace:     ARGS += --arg 'autoStartCluster' true --argstr 'autoStartClusterArgs' '--trace --trace-workbench' ## Enter Nix shell, start workbench cluster, with shell tracing
cluster-shell-dev-trace: ARGS += --arg 'autoStartCluster' true --arg 'workbenchDevMode' true --argstr 'autoStartClusterArgs' '--trace --trace-workbench' ## Enter Nix shell, dev mode, start workbench cluster, with shell tracing
fixed:                   ARGS += --arg 'autoStartCluster' true
fixed:                   PROFILE = fixed-alzo
shell-dev cluster-shell-dev cluster-shell-trace cluster-shell-dev-trace fixed: shell

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
bump-node-measured: ## Update the node-measured flake input
	nix flake lock --update-input node-measured
bump-node-snapshot: ## Update the node-snapshot flake input
	nix flake lock --update-input node-snapshot
bump-node-workbench: ## Update the node-workbench flake input
	nix flake lock --update-input node-workbench
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
	nix-shell --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE} ${ARGS}

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
	rm -rf db dist-newstyle .stack-work $(shell find . -name '*~' -or -name '*.swp')

cls:
	echo -en "\ec"

.PHONY: bench-chainsync cabal-hashes clean cli cls cluster-profiles cluster-shell cluster-shell-dev cluster-shell-dev-trace cluster-shell-trace ghci ghcid help node run-test shell shell-dev stylish-haskell test-ghci test-ghcid test-ghcid-nix
