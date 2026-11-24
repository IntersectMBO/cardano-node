# cardano-submit-api is deprecated

> cardano-submit-api is deprecated and will be removed in a future release.
> Use the UTxO RPC interface built into cardano-node (cardano-rpc) instead.

To move away from cardano-submit-api, follow these three steps:

1. Enable cardano-rpc in your node.
2. Switch your clients to cardano-rpc.
3. Remove cardano-submit-api.

## Why

cardano-rpc is the gRPC server built into cardano-node, and it implements the UTxO RPC standard (`utxorpc.v1beta`).
gRPC is a remote call protocol that needs HTTP/2, so you cannot call it with plain HTTP/1.1 tools.
Because cardano-rpc is part of cardano-node, there is no extra process to run.

## What this means now

The API of cardano-submit-api is unchanged and it still works, and there is no removal date yet.
The binary prints a framed `WARNING: cardano-submit-api IS DEPRECATED` block to stderr once at start-up, also for `--help` and `--version`.
It logs the same warning through its logger at Warning severity (namespace `TxSubmitApi.Application.Deprecated`), and you cannot turn the warning off.
Every HTTP response has a `Deprecation` header (RFC 9745) and a `Link` header with `rel="deprecation"` that points to this guide.
The `--help` text starts with `DEPRECATED:`.

## Before you start

* cardano-rpc over a Unix socket is in cardano-node 10.7.0 and later.
* cardano-rpc over TCP needs a cardano-node release that is newer than 11.1.3.
* Read the release notes of your release to find out if it has TCP support.
* The RPC options are marked `[EXPERIMENTAL]`.
  The interface can change between releases.
* cardano-rpc implements the `utxorpc.v1beta` specification only.
* cardano-rpc decodes a transaction only in the current era of the node.
  It rejects a transaction built for an earlier era with `Failed to decode transaction:`, so your clients must build transactions for the era the node is in, especially around a hard fork.

## Step 1: Enable cardano-rpc

The RPC server is off by default.
Add these keys to the node configuration file.
Only `EnableRpc` is always needed.

| Key | Required | Meaning |
|---|---|---|
| `EnableRpc` | Yes | Set to `true` to start the RPC server. Default: `false`. |
| `RpcSocketPath` | No | Path of the Unix socket. Default: `rpc.sock` next to the node socket. |
| `RpcListenPort` | For TCP | TCP port. Setting it selects TCP instead of the Unix socket. |
| `RpcListenAddress` | No | IP address for TCP. Default: `127.0.0.1`. Needs `RpcListenPort`. |
| `RpcTlsCertificateFile` | For TLS | TLS certificate file. Needs `RpcTlsPrivateKeyFile` and `RpcListenPort`. |
| `RpcTlsPrivateKeyFile` | For TLS | TLS private key file. Needs `RpcTlsCertificateFile`. |
| `RpcTlsChainCertificateFiles` | No | List of more certificate files for the TLS chain. Needs the two TLS keys above. Default: empty list. |

You can use the Unix socket or TCP, not both.
`RpcSocketPath` cannot be set together with `RpcListenAddress`, `RpcListenPort` or the TLS keys.
Without TLS keys, the TCP server uses h2c, which is plaintext HTTP/2 without TLS.
The node starts with an error if you give a wrong combination, for example `RpcListenAddress requires RpcListenPort to be set`.
The node also needs its own socket path (`SocketPath` in the configuration file, or `--socket-path`), and it refuses to start with `EnableRpc` and no node socket path.

This example uses the Unix socket:

```json
{ "EnableRpc": true }
```

This example uses TCP on port 50051:

```json
{ "EnableRpc": true, "RpcListenPort": 50051 }
```

Each key has a command-line flag with the same meaning:

* `--grpc-enable`
* `--grpc-socket-path`
* `--grpc-listen-address`
* `--grpc-listen-port`
* `--grpc-tls-certificate`
* `--grpc-tls-private-key`
* `--grpc-tls-chain-certificate`

Restart the node after you change these options.

### Security

cardano-rpc has no client authentication.
The only TLS options are a server certificate, a key and a chain, so there are no client-certificate keys.
Any client that can reach the port can submit transactions and query the node.
Keep the default `127.0.0.1` or use the Unix socket.
If other hosts need access, use TLS and a firewall allow-list or a reverse proxy, and never expose the port to the internet.
The node logs a warning at start-up when cardano-rpc listens on an address other than loopback.

### NixOS

The NixOS module of cardano-node (`nix/nixos/cardano-node-service.nix`) has no RPC options, so set the keys with its `extraNodeConfig` option:

```nix
services.cardano-node.extraNodeConfig = { EnableRpc = true; };
```

### Docker

To use the Unix socket, share the socket directory with the client container through a volume.
To use TCP inside a container, set `RpcListenAddress` to `0.0.0.0` and publish the port only on the host loopback, for example `-p 127.0.0.1:50051:50051`.

### Check that cardano-rpc is running

To list the services of the server, run the command below for TCP.
For the Unix socket, add `-unix` and use `/path/to/rpc.sock` as the address.

```bash
grpcurl -plaintext localhost:50051 list
```

Good output lists `utxorpc.v1beta.*` services.

### Monitoring

cardano-rpc has no metrics port of its own.
The node exports a request counter per method, for example `rpc.request.SubmitService.SubmitTx`, through its existing metrics backends.
These are EKG and the Prometheus endpoint, which the default configuration serves on `127.0.0.1:12798`.
Search the endpoint output for `SubmitTx`, and rebuild any dashboards that used the cardano-submit-api port 8081.
The counters count requests started, not results, and there are no latency or status-code metrics.
There is also no equivalent of the `tx_submit` and `tx_submit_fail` counters, so measure success, failure and latency in your client for now.

The node log carries the RPC events under the namespace prefix `RPC`.
A rejected transaction is `RPC.SubmitService.TxValidationError` and a decode failure is `RPC.SubmitService.TxDecodingError`, both at Debug severity.
An N2C connection error is `RPC.SubmitService.N2cConnectionError` at Warning severity.
The default minimum severity is Notice, so the node does not log rejected transactions by default, while cardano-submit-api logged every submit at Info.
To log them, add these two entries to the existing `TraceOptions` object in the node configuration file, and do not replace the object:

```json
{
  "TraceOptions": {
    "RPC.SubmitService.TxValidationError": { "severity": "Debug" },
    "RPC.SubmitService.TxDecodingError": { "severity": "Debug" }
  }
}
```

There is no `grpc.health.v1` service, so for a liveness probe call `ReadTip` or use the `list` check above:

```bash
grpcurl -plaintext localhost:50051 utxorpc.v1beta.sync.SyncService/ReadTip
```

## Step 2: Replace the HTTP call

cardano-rpc has no gRPC-Web, Connect or REST transport, so you cannot call it with plain `curl`.
Use a gRPC client, for example `grpcurl`, or a client library (Step 3).

| | cardano-submit-api | cardano-rpc |
|---|---|---|
| Process | Separate process | Part of cardano-node |
| Configuration | `--socket-path`, `--config` and network flags | Node configuration, see Step 1 |
| Endpoint | `POST /api/submit/tx` | `utxorpc.v1beta.submit.SubmitService/SubmitTx` |
| Transport | HTTP on `127.0.0.1:8090` by default | gRPC on a Unix socket, TCP (h2c) or TLS |
| Request | Raw transaction CBOR, `Content-Type: application/cbor` | `SubmitTxRequest`, with the CBOR in the `tx.raw` field |
| Success | HTTP 202 and the transaction ID as a JSON hex string | `SubmitTxResponse` with the raw 32-byte transaction ID in `ref` (base64 in JSON) |
| Failure | HTTP 400 and JSON `{"tag":"TxSubmitFail","contents":...}` | gRPC error. The message starts with `Failed to submit transaction:` or `Failed to decode transaction:` |
| Eras | Every Shelley-based era | The current era of the node only |
| Metrics | Prometheus on port 8081 (`--metrics-port`) | See Monitoring |

### Before

This is the call from the cardano-submit-api README:

```bash
xxd -r -p <<< "$(jq -r .cborHex tx.signed)" > tx.signed.cbor
curl --header "Content-Type: application/cbor" -X POST \
  http://localhost:8090/api/submit/tx --data-binary @tx.signed.cbor
```

### After

The file `tx.signed` is the JSON envelope that `cardano-cli` writes, and the request needs the CBOR as base64.
Make the request body:

```bash
jq -n --arg raw "$(jq -r .cborHex tx.signed | xxd -r -p | base64 | tr -d '\n')" \
  '{tx: {raw: $raw}}' > request.json
```

Submit `request.json` over TCP.
`grpcurl` uses server reflection, so it needs no proto files.
Replace `localhost:50051` with your `RpcListenAddress` and `RpcListenPort`.

```bash
grpcurl -plaintext -d @ localhost:50051 \
  utxorpc.v1beta.submit.SubmitService/SubmitTx < request.json
```

Submit `request.json` over the default Unix socket.
The `-unix` flag makes `grpcurl` read the address as a socket path.
Replace `/path/to/rpc.sock` with the path of your `rpc.sock` file.

```bash
grpcurl -plaintext -unix -d @ /path/to/rpc.sock \
  utxorpc.v1beta.submit.SubmitService/SubmitTx < request.json
```

The response has the transaction ID as base64:

```json
{ "ref": "<base64>" }
```

To get the hex transaction ID that cardano-submit-api returned, convert it like this:

```bash
grpcurl -plaintext -d @ localhost:50051 utxorpc.v1beta.submit.SubmitService/SubmitTx < request.json | jq -r .ref | base64 -d | xxd -p -c 64
```

For the Unix socket, add `-unix` and use the socket path as the address.

### If the node rejects the transaction

grpcurl prints an error and exits with a non-zero code.
This example shows a transaction whose inputs are already spent:

```
ERROR:
  Code: Unknown
  Message: Failed to submit transaction: TxValidationErrorInCardanoMode (ShelleyTxValidationError ShelleyBasedEraConway (ConwayApplyTxError (ConwayMempoolFailure "All inputs are spent. Transaction has probably already been included" :| [])))
```

All `SubmitTx` failures currently return the gRPC status `Unknown`, so a client must tell them apart by the message text.
These are the known message prefixes:

* `Failed to submit transaction:` means the ledger or the mempool rejected the transaction.
* `Failed to decode transaction:` means the CBOR is bad or the transaction is for the wrong era.
* `N2C connection error while trying to submit a transaction:` means cardano-rpc could not reach the node, and it is usually worth a retry.
* `Internal error while processing the request.` means an internal failure in cardano-rpc.

If you resubmit a transaction that the node already accepted, you get an error such as `All inputs are spent...`, not the transaction ID.
Check the status of the transaction before you treat that error as a failure.

## Step 3 (optional): Use a client library

UTxO RPC has client SDKs.
The cardano-rpc quickstarts have a working example for each language:

* [Rust](https://github.com/IntersectMBO/cardano-api/blob/master/cardano-rpc/quickstart/rust/README.md)
* [TypeScript](https://github.com/IntersectMBO/cardano-api/blob/master/cardano-rpc/quickstart/typescript/README.md)
* [Go](https://github.com/IntersectMBO/cardano-api/blob/master/cardano-rpc/quickstart/go/README.md)
* [Python](https://github.com/IntersectMBO/cardano-api/blob/master/cardano-rpc/quickstart/python/README.md)
* [Haskell](https://github.com/IntersectMBO/cardano-api/blob/master/cardano-rpc/quickstart/haskell/README.md)

The Python, Rust, Haskell and TypeScript quickstarts have a send-lovelace example that calls `SubmitTx`.
The full description of the server is in the [cardano-rpc README](https://github.com/IntersectMBO/cardano-api/blob/master/cardano-rpc/README.md).

## Switch over

You can run cardano-submit-api and cardano-rpc side by side, because both submit to the same node.
Enabling cardano-rpc needs a node restart, so restart one node at a time.
Test your clients against cardano-rpc, move the traffic, and only then stop cardano-submit-api.
To roll back, point the clients at cardano-submit-api again, which keeps working until it is removed.
The server allows at most 64 concurrent streams per HTTP/2 connection, so high-volume clients may need more than one connection.

## Step 4: Remove cardano-submit-api

When all clients use cardano-rpc, stop the cardano-submit-api service and disable it so that it does not start again.
Then remove the service definition, the cardano-submit-api configuration file and the Prometheus scrape job for port 8081, if you have one.

## Get help

Open an issue in the [cardano-node repository](https://github.com/IntersectMBO/cardano-node/issues).
