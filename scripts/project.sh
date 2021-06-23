#!/usr/bin/env bash

CYAN='\033[0;36m'
NC='\033[0m' # No Color

build() {
  echo -e "${CYAN}[ Building project ]${NC}"
  cabal build all --write-ghc-environment-files=ghc8.4.4+ --enable-tests

  echo -e "${CYAN}[ Building plutus-example] ${NC}"
  cd plutus-example
  cabal build all --write-ghc-environment-files=ghc8.4.4+ --enable-tests
}

test() {
  echo -e "${CYAN}[ Testing project ]${NC}"
  cabal test all --write-ghc-environment-files=ghc8.4.4+ --enable-tests --test-show-details=direct

  echo -e "${CYAN}[ Testing plutus-example ]${NC}"
  cd plutus-example
  cabal test all --write-ghc-environment-files=ghc8.4.4+ --enable-tests --test-show-details=direct
}

case "$1" in
  build ) build ;;
  test  ) test  ;;
esac
