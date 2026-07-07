help: ## Print documentation
	@grep -hE '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sed 's/^ //' | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-33s\033[0m %s\n", $$1, $$2}'

include nix.mk

PROJECT_NAME = cardano-node
NUM_PROC     = $(nproc --all)

lint hlint: ## Run the CI version of hlint
	nix build --no-link '.#checks/hlint' --cores 0
haddock-hoogle haddocks hoogle:
	if test -z "$$IN_NIX_SHELL"; then nix-shell --run 'cabal update && cabal haddock all --haddock-hoogle'; else cabal update && cabal haddock all --haddock-hoogle; fi
host-hlint: ## Run the system (not Nix) version of hlint
	hlint bench cardano-{api,cli,node,node-capi,node-chairman,submit-api,testnet,tracer}

stylish-haskell: ## Apply stylish-haskell on all *.hs files
	@find . -type f -name "*.hs" -not -path '.git' -print0 | xargs -0 stylish-haskell -i

cabal-hashes:
	nix run .#checkCabalProject

cli node:
	cabal --ghc-options="+RTS -qn8 -A32M -RTS" build cardano-$@

trace-documentation:
	cabal run -- exe:cardano-node trace-documentation --config 'configuration/cardano/mainnet-config.yaml' --output-file 'doc/new-tracing/tracers_doc_generated.md'

trace-schemas-regenerate: ## Regenerate trace schemas, apply overrides, validate
	bash bench/trace-schemas/scripts/schema-gen/RegenerateTraceSchemas.sh

trace-schemas-overrides-check: ## Check whether all schema overrides are applied
	nix run .#apply-schema-overrides -- --check --verbose

trace-schemas-overrides-coverage: ## Fail when generated schema files change without matching override sidecars (use RANGE=origin/master...HEAD in CI)
	nix run .#check-override-coverage -- ${if ${RANGE},--range ${RANGE}}

trace-schemas-validate: ## Validate trace message schemas against meta.schema.json
	nix run .#validate-trace-schemas

###
### Workbench:  cluster shells
###
## `make shell` (and -nix/-prof/-dev) opens a workbench dev shell for $(PROFILE) -- the usual
## entry point (below). `make <profile>[-VARIANT]` are per-profile aliases; `make ps` lists
## profile names. Targets, flags and the (generated) profile lists all live in
## nix/workbench/lib.mk (which pulls in profiles.mk itself).
include nix/workbench/lib.mk

## Dev shells -- the usual entry point; each runs $(WB_ENTER) (defined in nix/workbench/lib.mk).
shell: ## workbench dev shell for PROFILE (cabal build-on-demand); vars: PROFILE ERA BACKEND CMD RUN
	$(WB_ENTER)
shell-nix: ## like shell, but run the Nix-store binary
	$(WB_ENTER) --arg useCabalRun false
shell-prof: ## like shell, profiled build, run with -hT
	$(WB_ENTER) --arg profiledBuild true --arg profilingType '"space-heap"'
shell-dev:
	$(WB_ENTER)
.PHONY: shell shell-nix shell-prof shell-dev

## CI entrypoints (drive the generated variant targets + Nix ci-test):
workbench-ci: workbench-ci-test ci-test-auto ci-test-autonix
CI_TARGETS := hlint workbench-ci haddock-hoogle
ci:  ci-report ci-targets
ci-report:
	@echo -e "\033[34mGoals under test\033[0m:  \033[33m$(CI_TARGETS)\033[0m"
ci-targets:  $(CI_TARGETS)

###
### Misc
###
clean: clean-profile

full-clean: clean
	rm -rf db dist-newstyle $(shell find . -name '*~' -or -name '*.swp')

cls:
	echo -en "\ec"

.PHONY: help lint hlint host-hlint haddock-hoogle stylish-haskell cabal-hashes cli node \
        trace-documentation trace-schemas-regenerate trace-schemas-overrides-check \
        trace-schemas-overrides-coverage trace-schemas-validate \
        workbench-ci ci ci-report ci-targets \
        clean full-clean cls
