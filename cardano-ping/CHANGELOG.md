# Revision history for cardano-ping

## next version

### Breaking changes

### Non-breaking changes

## 0.8.0.1 -- 2025-05-13

### Breaking changes

### Non-breaking changes

* Adapt to buffered socket bearers from network-mux 0.8

## 0.8.0.0 -- 2025-02-25

### Breaking changes

* Added `NodeToClientVersionV20`

## 0.7.0.0 -- 2024-10-17

### Breaking changes

* Updated dependencies.

## 0.6.0.0 -- 2024-10-17

### Breaking changes

* Support `NodeToClientV_19`

### Non-breaking changes

* Use `network-mux-0.5`.

### Non-breaking changes

## 0.5.0.0 -- 2024-10-11

### Breaking changes

* Added `NodeToClientVersionV18`
* Added `NodeToNodeVersion14`

## 0.4.0.1 -- 2024-08-27

### Breaking changes

### Non-breaking changes

* bump for bad ref in chap for 0.4.0.0

## 0.4.0.0 - 2024-08-22

### Breaking changes

* Log remote address and port in tip message
* Added `NodeToClientVersionV17`

### Non-breaking changes

* Make it error whenever there's a decoding error or similar

## 0.3.0.0 -- 2024-08-07

### Breaking changes

* Add support for requesting tip from remote peer.

### Non-breaking changes

* Make it build with ghc-9.10

## 0.2.0.14 -- 2024-06-07

### Breaking changes

* Addapted to `network-mux` changes in https://github.com/IntersectMBO/ouroboros-network/pull/4997

### Non-breaking changes

* Bump io-sim and io-classes

## 0.2.0.13

### Non-breaking changes

* Add support for decoding peersharing support
* Add support for NodeToNodeVersionV13

## 0.2.0.12

### Non-breaking changes

* Use `io-sim-1.4.1`

## 0.2.0.11

### Non-breaking changes

* ghc-9.8 support.

## 0.2.0.10 -- 2023-12-08

### Non-breaking changes

* Expose more `InitiatorOnly`, `handshakeDec`, `handshakeReq` and `isSameVersionAndMagic` from `Cardano.Network.Ping`.

## 0.2.0.9 -- 2023-11-16

### Non-breaking changes

* Use `io-sim-1.3.0.0`.
* ghc-9.8 support.

## 0.2.0.8 -- 2023-11-02

### Breaking changes

### Non-breaking changes

* Use `NonEmpty` for `handshakeReqEnc`'s parameter to eliminate an impossible
  `error`.

## 0.2.0.7 -- 2023-10-20

* In presence of flag `-j`, output json when printing
  `network_rtt`, `handshake_rtt`, `negotiated_version` and `queried_versions`.

## 0.2.0.6 -- 2023-08-09

* Use `io-classes-1.2`

## 0.2.0.5 -- 2023-06-15

* Fixed support of `node-to-client` protocol on Unix sockets.
* Fixed encoding of `NodeToClientVersionV16` version data.
* Fixed decoding of `NodeToClientVersionV16` and `NodeToNodeVersionV12`.

## 0.2.0.4 -- 2023-06-12

* Using `ISO8601` time format.
* Only print negotiated version, if negotiation took place on the remote side.
* Fixed formatting of ping messages.

## 0.2.0.3 -- 2023-06-09

* For versions strictly lower than `NodeToNodeV_11`, send
  `InitiatorAndResponder` flag when quering.  For these versions querying is
  not recognised by the remote side, and thus it will do handshake negotiation.
* Only print the query result if querying is supported by the remote side.

## 0.2.0.2 -- 2023-06-08

* Support `NodeToNodeV_11`, `NodeToNodeV_12` and `NodeToClientV_16`.
* Fix delay/timeout bugs (miliseconds were used instead of seconds).
* Print query even if --quiet flag is given.
* Instead of a boolean flag print `InitiatorOnly` or `InitiatorAndResponder`.
* Fixed encoding of `NodeToNodeV_11`.


## 0.2.0.1 -- 2023-05-26

* Support `ghc-9.6`.

## 0.2.0.0 -- 2023-05-08

* Support for `NodeToNodeV_12` and `NodeToClientV_16`, e.g. support for
  querying `NodeToNodeVersionData` / `NodeToClientVersionData`.
* Support `NodeToNodeV_11` and `NodeToClientV_15` (peer sharing).

## 0.1.0.0 -- 2022-12-14

* This code was originally from the cardano-ping executable component of the `network-mux` package.
