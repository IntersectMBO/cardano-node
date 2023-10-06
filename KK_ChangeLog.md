There is no reason to run this version on a BP.
Don't run it on a relay which has a local client that sends TXs to it.

8.1.2 with the following changes:

- Don't attempt to insert TXs that already are in the mempool. This provides an exceptional
  performance improvement. See https://github.com/karknu/ouroboros-consensus/tree/karknu/dont_dupe .
- Timeout for server side chainsync and keepalive so that stale connections are removed.
  See https://github.com/input-output-hk/ouroboros-network/pull/4648 .
- Improve inbound peer state transition.
  See https://github.com/input-output-hk/ouroboros-network/pull/4684

