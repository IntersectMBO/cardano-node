# network-mux

Multiplexing library. It allows to run multiple network applications over
a single bearer. The multiplexer cuts messages in chunks of some maximal size
and sends them over a bearer channel. The current version of this library
relies on reliable and ordered delivery of messages. The multiplexer should run
alongside an incremental decoder.

Example protocol with an incremental
decoder is implemented in
[Test.Mux.ReqResp](https://github.com/intersectmbo/ouroboros-network/blob/master/network-mux/test/Test/Mux/ReqResp.hs)
for other examples of protocols see 'typed-protocols' or 'ouroboros-network'
packages.

## tests

To run the test suite:
```
cabal new-run test-network-mux
```
or
```
nix-build -A haskellPackages.network-mux.checks
```
