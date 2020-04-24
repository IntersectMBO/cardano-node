PROJECT_NAME = cardano-node
NUM_PROC     = $(nproc --all)


help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

stylish-haskell: ## Apply stylish-haskell on all *.hs files
	@find . -type f -name "*.hs" -not -path '.git' -not -path '*.stack-work*' -print0 | xargs -0 stylish-haskell -i

cabal-hashes:
	$$(nix-build default.nix -A checkCabalProject --no-out-link)

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

test-chairmans-cluster:
	@scripts/cluster-test.sh

BENCH_REPEATS ?= 5
BENCH_CONFIG ?= mainnet-benchmark
BENCH_TAG ?= HEAD
BENCH_XARGS ?=

profile-chainsync:
	scripts/mainnet-benchmark.sh ${BENCH_XARGS}  --node-config-${BENCH_CONFIG} --repeats ${BENCH_REPEATS} --nix --profile time --tag ${BENCH_TAG}

clean-profile proclean:
	rm -f *.html *.prof *.hp *.stats *.eventlog

clean: clean-profile
	rm -rf logs/ socket/ cluster.*

full-clean: clean
	rm -rf db dist-newstyle .stack-work $(shell find . -name '*~' -or -name '*.swp')

.PHONY: stylish-haskell cabal-hashes ghcid ghcid-test run-test test-ghci test-ghcid help clean clean-profile proclean
