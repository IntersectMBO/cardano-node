#!/bin/sh
exec cabal --jobs=256 --keep-going --disable-documentation repl \
	--repl-options="-ghci-script=$(pwd)/cardano-tracer/test/ghci.script" \
	--repl-options=-fprint-potential-instances \
	--repl-options=-Wno-error=unused-packages \
	--repl-options=-Wno-unused-packages \
	--build-depends bytestring \
	--build-depends cborg \
	--build-depends network-mux \
	--build-depends optparse-applicative-fork \
	--build-depends ouroboros-network-framework \
	--build-depends trace-dispatcher \
	--build-depends typed-protocols \
	--build-depends typed-protocols-examples \
	cardano-tracer:exe:cardano-tracer 
