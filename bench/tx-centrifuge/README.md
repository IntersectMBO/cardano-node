# Tx Centrifuge & Pull-Fiction

`tx-centrifuge` is a high-performance load generator for Cardano, built on top of the protocol-agnostic **Pull-Fiction** library.

Unlike traditional load generators that "push" data at a fixed rate, this system is designed for **pull-based protocols**. It does not generate load by itself; instead, it acts as a **policer** that reacts to requests from downstream consumers, admitting or delaying them to enforce a configured rate ceiling.

### Minimal Configuration Example

A basic configuration defines how to load initial resources, how to build payloads, the desired rate, and where to send the results:

```json
{
  "initial_inputs": {
    "type": "genesis_utxo_keys",
    "params": {
      "network_magic": 42,
      "signing_keys_file": "funds.json"
    }
  },
  "builder": {
    "type": "value",
    "params": { "fee": 1000000 },
    "recycle": { "type": "on_pull" }
  },
  "rate_limit": { "type": "token_bucket", "params": { "tps": 10 } },
  "workloads": {
    "group-A": {
      "targets": {
        "node-1": { "addr": "127.0.0.1", "port": 30000 },
        "node-2": { "addr": "127.0.0.1", "port": 30001 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

## Core Concepts: The Pull-Fiction Engine

The underlying `pull-fiction` library implements a reactive rate-limiting strategy. It only produces data when a consumer asks for it, and only as fast as the rate limiter allows.

### Architecture

```
                                    ┌───────────────┐
                                    │ Initial UTxOs │
                                    └───────┬───────┘
                                            │
                                      (partitioned)
                                            │
              ┌─────────────────────────────┼─────────────────────────────┐
              │                             │                             │
              ▼                             ▼                             ▼
┌─────────────────────────┐   ┌─────────────────────────┐   ┌─────────────────────────┐
│       Workload A        │   │       Workload B        │   │       Workload N        │
├─────────────────────────┤   ├─────────────────────────┤   ├─────────────────────────┤
│                         │   │                         │   │                         │
│   ┌─────────────────┐   │   │   ┌─────────────────┐   │   │   ┌─────────────────┐   │
│   │   Input Queue   │   │   │   │   Input Queue   │   │   │   │   Input Queue   │   │
│   │   (unbounded)   │   │   │   │   (unbounded)   │   │   │   │   (unbounded)   │   │
│   └────────┬────────┘   │   │   └────────┬────────┘   │   │   └────────┬────────┘   │
│            │            │   │            │            │   │            │            │
│            ▼            │   │            ▼            │   │            ▼            │
│   ┌─────────────────┐   │   │   ┌─────────────────┐   │   │   ┌─────────────────┐   │
│   │     Builder     │   │   │   │     Builder     │   │   │   │     Builder     │   │
│   │ (build & sign)  │   │   │   │ (build & sign)  │   │   │   │ (build & sign)  │   │
│   └────────┬────────┘   │   │   └────────┬────────┘   │   │   └────────┬────────┘   │
│            │            │   │            │            │   │            │            │
│            ▼            │   │            ▼            │   │            ▼            │
│   ┌─────────────────┐   │   │   ┌─────────────────┐   │   │   ┌─────────────────┐   │
│   │  Payload Queue  │   │   │   │  Payload Queue  │   │   │   │  Payload Queue  │   │
│   │   (bounded)     │   │   │   │   (bounded)     │   │   │   │   (bounded)     │   │
│   └────────┬────────┘   │   │   └────────┬────────┘   │   │   └────────┬────────┘   │
│            │            │   │            │            │   │            │            │
│      ┌─────┴─────┐      │   │      ┌─────┴─────┐      │   │      ┌─────┴─────┐      │
│      ▼           ▼      │   │      ▼           ▼      │   │      ▼           ▼      │
│  ┌────────┐ ┌────────┐  │   │  ┌────────┐ ┌────────┐  │   │  ┌────────┐ ┌────────┐  │
│  │Worker 1│ │Worker 2│  │   │  │Worker 1│ │Worker 2│  │   │  │Worker 1│ │Worker 2│  │
│  └───┬────┘ └───┬────┘  │   │  └───┬────┘ └───┬────┘  │   │  └───┬────┘ └───┬────┘  │
│      │          │       │   │      │          │       │   │      │          │       │
│      ▼          ▼       │   │      ▼          ▼       │   │      ▼          ▼       │
│  NodeToNode     ...     │   │  NodeToNode     ...     │   │  NodeToNode     ...     │
│  (multiplexed)          │   │  (multiplexed)          │   │  (multiplexed)          │
│      │          │       │   │      │          │       │   │      │          │       │
│      ▼          ▼       │   │      ▼          ▼       │   │      ▼          ▼       │
│  ┌──────┐   ┌──────┐    │   │  ┌──────┐   ┌──────┐    │   │  ┌──────┐   ┌──────┐    │
│  │Node 1│   │Node 2│    │   │  │Node 3│   │Node 4│    │   │  │Node 5│   │Node 6│    │
│  └──────┘   └──────┘    │   │  └──────┘   └──────┘    │   │  └──────┘   └──────┘    │
│                         │   │                         │   │                         │
│  ◄── recycle outputs ───┤   │  ◄── recycle outputs ───┤   │  ◄── recycle outputs ───┤
│                         │   │                         │   │                         │
└─────────────────────────┘   └─────────────────────────┘   └─────────────────────────┘
```

**Pipeline flow:**
1. **Initial UTxOs** are loaded and **partitioned** across workloads
2. Each workload's share enters its **Input Queue** (unbounded)
3. A **Builder** (one per workload) pulls inputs, assembles and signs transactions, and pushes `(tx, outputs)` to the **Payload Queue** (bounded — sole source of backpressure)
4. **Workers** (one per target) pull from the Payload Queue via rate-limited fetchers
5. Workers connect to Cardano nodes via a **multiplexed NodeToNode** connection running **TxSubmission2** and **KeepAlive** mini-protocols (optionally **ChainSync** + **BlockFetch** for confirmation-based recycling)
6. **Outputs are recycled** back to the workload's Input Queue according to the configured `recycle` strategy, enabling indefinite-duration runs

### Reactive Rate Limiting
- **Downstream Driven**: Load is only dispensed in response to an explicit pull from a target. If the target doesn't ask, the engine stays idle.
- **Ceiling Enforcement**: The rate limiter enforces a tokens-per-second (TPS) ceiling. Even if a consumer pulls aggressively, the engine ensures the dispensed items never exceed the configured limit.
- **Fairness**: Token slots are claimed in a single atomic STM transaction, providing FIFO-fair scheduling across multiple workers sharing the same limiter.

### Workloads and Targets
The configuration is organized into a hierarchy that defines the concurrency model:

- **Target**: A single network endpoint (e.g., a Cardano node). Each target has a dedicated **Worker thread** that manages the network connection and handles requests.
- **Workload**: A logical grouping of targets.
  - All targets within a workload share the same **Builder thread** and the same **Payload Queue**.
  - **Transaction Profiles**: Each workload can define its own `builder` configuration. This allows you to generate different "profiles" of transactions (e.g., different sizes, complexities, or fees) for different groups of nodes.
  - **Isolation**: By using multiple workloads, you can isolate different groups of targets. For example, one workload could simulate high-volume "small" transactions for one group of nodes, while another generates "heavy" transactions for another.

### Pipeline Architecture
The engine operates as a decoupled production pipeline using generic `input` and `payload` types:
1.  **Initial Inputs**: Starting resources are partitioned across workloads and bulk-loaded into each workload's Input Queue at startup.
2.  **Input Queue (Unbounded)**: Holds available inputs.
3.  **Builder (One per Workload)**: A dedicated thread that pulls inputs, produces a payload, and handles recycling according to the configured strategy (see [Resource Recycling](#resource-recycling)). It pushes the payload to the payload queue.
4.  **Payload Queue (Bounded)**: The sole source of **backpressure**. The builder blocks here if consumers are slower than the production rate.
5.  **Workers (One per Target)**: Threads that manage the consumer connection. They pull from the payload queue via a rate-limited fetcher.

### Resource Recycling
To enable indefinite-duration runs with finite resources, inputs must be returned to the `Input Queue`. The `recycle` field on the `builder` selects when this happens. There are three strategies:

1.  **`on_build`** — The builder immediately returns resources to the `Input Queue` as soon as the payload is constructed, before it even enters the payload queue. This is the highest-throughput mode but assumes the payload will be successfully processed downstream.
    ```json
    "recycle": { "type": "on_build" }
    ```
2.  **`on_pull`** — The recycler remembers the payload's resources when it is built and returns them to the `Input Queue` when a worker **dequeues** the payload from the pipe (triggered by a downstream request), on its own thread. Recycling happens on dequeue, not on downstream acknowledgement.

    > **TODO:** rename the config value `on_pull` to `on_dequeue` — the strategy recycles when the payload is *dequeued* from the pipe, not on a TxSubmission "pull". Kept as `on_pull` for backward compatibility (the code type is already `RecycleOnDequeue`).

    ```json
    "recycle": { "type": "on_pull" }
    ```
3.  **`on_confirm`** — Resources stay in a pending map until an **observer** confirms the transaction on-chain at the configured confirmation depth. The builder enqueues the payload without any inputs; a background recycler async reads confirmations from the observer's broadcast channel and recycles matching inputs. This is the safest mode for long-running benchmarks where mempool eviction is a concern.
    ```json
    "recycle": { "type": "on_confirm", "params": "my-observer" }
    ```

## Configuration

### Initial Inputs (`initial_inputs`)
The generator requires a set of initial UTxOs, configured in the `initial_inputs` section of the main configuration file.

The `type` field selects the loader. Two variants are available:

**`genesis_utxo_keys`** — load funds from a JSON file. `params`:
- **`network_magic`**: Required for deriving UTxO references from keys (e.g., `42` for testnet).
- **`signing_keys_file`**: Path to a JSON file (e.g., `funds.json`) containing the actual fund data.

#### `funds.json` entry types
The file contains an array of fund objects. Each object has two required fields (`signing_key`, `value`) and one optional field (`tx_in`):

| Field         | Required | Description |
|---------------|----------|-------------|
| `signing_key` | Yes      | Path to a `.skey` file (payment or genesis UTxO key) |
| `value`       | Yes      | Lovelace amount |
| `tx_in`       | No       | Explicit UTxO reference in `"txid#ix"` format |

When `tx_in` is **present**, the fund uses the explicit UTxO reference:
```json
{ "signing_key": "payment.skey", "value": 1000000, "tx_in": "df6...#0" }
```

When `tx_in` is **omitted**, the fund is treated as a genesis UTxO: the `TxId` is derived deterministically from the signing key's verification key hash via `genesisUTxOPseudoTxIn`, and the `TxIx` is always 0.
```json
{ "signing_key": "genesis.skey", "value": 1500000000000 }
```

**Design Note**: The `funds.json` format is designed to be compatible with the output of `cardano-cli conway create-testnet-data --utxo-keys`. This allows you to immediately use an arbitrary large set of Shelley genesis keys created during testnet bootstrapping as the initial fund pool for the generator, without needing to manually create UTxOs once the network is live.

#### `local_utxo_query`
Discover the starting funds **on chain** at startup instead of from a file. The generator queries the local node (over its NodeToClient socket) for the UTxOs at one or more signing keys' addresses. This makes restarts **stateless**: each builder recycles its outputs back to its `destination_signing_key` address, so pointing this at those same keys re-discovers whatever a previous run left on chain.

`params`:
- **`socket_path`**: Path to the local node's NodeToClient socket.
- **`signing_keys`**: Array of `.skey` file paths. Each key's address is queried, and every UTxO found there becomes an initial fund tagged with that key (so it can be spent).

The query era is detected from the node at runtime, so it follows the chain across the Shelley-based eras cardano-api supports (Shelley through Conway today). If no UTxOs are found at any queried address, the generator exits with an error rather than starting. Fund the address of at least one configured `signing_keys` entry and restart.

```json
"initial_inputs": {
  "type": "local_utxo_query",
  "params": {
    "socket_path": "/run/node/node.sock",
    "signing_keys": ["dest.skey"]
  }
}
```

### Rate Limiting (`rate_limit`)
The `rate_limit` field can be set at the **top level** or at the **workload level** (but not both — setting it at both levels is a validation error). If omitted entirely, targets run **unlimited** (no rate ceiling).

The `scope` determines the granularity of the TPS ceiling. Available scopes depend on where the rate limit is defined:

**Top-level scopes:**
- **`shared`** (default): A single rate limiter shared by all targets across all workloads. The configured TPS is the aggregate ceiling.
- **`per_workload`**: Each workload gets its own independent rate limiter at the full configured TPS (shared by its targets).
- **`per_target`**: Every target gets its own independent rate limiter at the full configured TPS. E.g., 10 TPS with 50 targets = 500 TPS aggregate.

**Workload-level scopes:**
- **`shared`** (default): One rate limiter shared by all targets in the workload. The configured TPS is the aggregate ceiling for the workload.
- **`per_target`**: Every target in the workload gets its own independent rate limiter at the full configured TPS.

### Cascading Defaults

Most configuration fields can be set at multiple levels. The most specific value wins:

- **`builder`**: workload > top-level. Setting it at **both** levels is a validation error. At least one must be set (no default).
- **`rate_limit`**: workload > top-level > **unlimited**. Setting it at **both** levels is a validation error.
- **`max_batch_size`**: target > workload > top-level > **0 (unlimited)**.
- **`on_exhaustion`**: target > workload > top-level > **`block`**.

Workload and target names must be non-empty and must not contain `.` or start with `@` (reserved for internal rate-limiter cache keys).

### Batching and Flow Control
- **`max_batch_size`**: Limits the number of items (e.g., transactions) the generator will announce to a target in a single protocol request. **0 means unlimited** (use whatever the node requests). Defaults to 0.
  - This acts as a safety cap: even if a target's protocol allows for 500 items, a `max_batch_size` of 100 ensures the generator doesn't commit too much capacity to a single connection at once.
  - This helps distribute the available "payload queue" more evenly across multiple targets and prevents a single aggressive node from starving others.
- **`on_exhaustion`**:
  - `block`: The worker thread waits until the builder produces a new payload.
  - `error`: The generator fails immediately if the builder cannot keep up with the requested TPS.

### Startup Delay (`startup_delay_seconds`)
An optional **top-level** field that delays the start of transmission. Waits this many seconds before the workers open their connections to the targets but after the builders are spawned and filling the payload queues.

- **Value**: a non-negative integer number of **seconds**.
- **Default**: `0` (also the effect when the field is absent or `null`). Workers connect as soon as they are spawned (no delay, no log line).
- **Scope**: top level only. It applies to the whole run and does **not** cascade to workloads or targets.

```json
{
  "rate_limit": { "type": "token_bucket", "params": { "tps": 100000 } },
  "startup_delay_seconds": 300,
  "workloads": { "...": {} }
}
```

### Tracing

The generator emits structured traces through `trace-dispatcher` (the same
library the node uses). Trace settings live in the **same config file** you pass
to `tx-centrifuge`, under a `TraceOptions` object keyed by namespace: `""` is the
root default and every other key overrides one namespace. When `TraceOptions` is
absent, a default applies that writes every namespace to **stdout** in machine
(JSON) format at `Debug` severity, so all traces are on out of the box.

```json
{
  "rate_limit": { "type": "token_bucket", "params": { "tps": 100000 } },
  "workloads": { "...": {} },

  "TraceOptions": {
    "": {
      "severity": "Info",
      "detail": "DNormal",
      "backends": ["Stdout MachineFormat"]
    },
    "TxCentrifuge.Pipe": { "severity": "Silence" },
    "TxCentrifuge.Recycler": { "maxFrequency": 1.0 },
    "TxCentrifuge.Observer": { "severity": "Silence" },
    "TxCentrifuge.Builder.NewTx": { "detail": "DMaximum" },
    "TxCentrifuge.TxSubmission": { "detail": "DDetailed" },
    "TxSubmission2": { "severity": "Silence" },
    "KeepAlive": { "severity": "Silence" }
  }
}
```

Per namespace you can set `severity` (the minimum level to emit, or `Silence` to
drop it), `detail` (`DMinimal`, `DNormal`, `DDetailed`, `DMaximum`), `backends`
(such as `Stdout MachineFormat`, `Stdout HumanFormatColoured`, or `Forwarder`),
and `maxFrequency` (a cap in messages per second). The application-level traces
(severity `Info`) are:

| Namespace | What it reports | Detail levels |
| :--- | :--- | :--- |
| `TxCentrifuge.Builder.NewTx` | A new transaction was built from input UTxOs, producing output UTxOs. Carries the builder name and the TxId. Fires once per built transaction. | `DDetailed` adds the `inputs`/`outputs` as UTxO reference strings. `DMaximum` renders each fund in full (`utxo` + `lovelace`) and adds the `destination` address the tx pays to. |
| `TxCentrifuge.Pipe.InputsEnqueued` | Inputs were added to a pipe's input queue (initial funds or recycled ones). Carries the pipe name and the resulting queue `depth`. | `DDetailed` adds a `count` of the inputs. `DMaximum` adds the `inputs` array, each fund with its `utxo` reference and `lovelace` value. |
| `TxCentrifuge.Pipe.InputsDequeued` | Inputs were taken off the input queue for the builder. Carries the pipe name and the resulting queue `depth`. | `DDetailed` adds a `count` of the inputs. `DMaximum` adds the `inputs` array, each fund with its `utxo` reference and `lovelace` value. |
| `TxCentrifuge.Pipe.PayloadEnqueued` | A payload was added to the bounded payload queue. Carries the pipe name and the resulting queue `depth`. | `DMaximum` adds the payload's `txId` (payload events carry no `count`). |
| `TxCentrifuge.Pipe.PayloadDequeued` | A payload was pulled off the payload queue by a worker. Carries the pipe name and the observed queue `depth`. | `DMaximum` adds the payload's `txId` (payload events carry no `count`). |
| `TxCentrifuge.Recycler.Pending` | A payload entered the recycler's pending backlog (built or released first), its recyclable inputs held until its confirm, dequeue, or orphan signal matches it. Carries the recycler name and the resulting `pending` (the number of payloads held but not yet matched). | `DDetailed` adds a `count` of the held inputs. `DMaximum` adds the `inputs` array, each fund with its `utxo` reference and `lovelace` value. |
| `TxCentrifuge.Recycler.AddToPipe` | The recycler added a held payload's inputs back onto a pipe's input queue (via `Pipe.addInputs`), closing the loop. Carries the recycler name, the pipe name, and the resulting `pending`. | `DDetailed` adds a `count` of the recycled inputs. `DMaximum` adds the `inputs` array, each fund with its `utxo` reference and `lovelace` value. |
| `TxCentrifuge.Observer.Announce` | The observer saw a transaction confirmed or orphaned (rolled back), carrying the observer name, the TxId, and an `isOrphan` flag. Fires only for confirmation-based workloads, but there it is currently **unfiltered**: it reports every transaction in the confirmed blocks its node sees, not just this generator's, so its volume is roughly the whole chain's throughput. | All fields are shown at every level. |
| `TxCentrifuge.TxSubmission.RequestTxIds` | The node requested TxId announcements (blocking or non-blocking). Carries the `target` node and the ACK and REQ counts. | `DDetailed` and up add the list of currently unacked TxIds. |
| `TxCentrifuge.TxSubmission.ReplyTxIds` | We replied with TxId announcements and their sizes. Carries the `target`. | `DDetailed` and up add the ACK and REQ counts, the announced TxIds with sizes, and the updated unacked list. |
| `TxCentrifuge.TxSubmission.RequestTxs` | The node requested full transactions by TxId. Carries the `target`. | `DDetailed` and up add the list of requested TxIds. |
| `TxCentrifuge.TxSubmission.ReplyTxs` | We sent the requested transactions. Carries the `target`. | `DDetailed` and up add the sent TxIds with sizes, plus the list the node requested. |

Every `TxSubmission` trace carries a `target` field naming the remote node, so
submission activity can be attributed per target. The `Pipe`, `Recycler`, and
`Observer` traces fire on roughly every transaction. Each `Pipe` event carries
its queue `depth` and each `Recycler` event its `pending` count (the number of
payloads held but not yet matched) on every event and at every detail level.
Raising `detail` to `DDetailed` adds a `count` of the items involved (input and
recycler events only, since payload events carry no count), and `DMaximum` adds
the items themselves (the `inputs` funds, or a payload's `txId`). At
high TPS that is a firehose, so you will usually want to silence them or cap them
with `maxFrequency`. Because a `maxFrequency` cap samples a namespace at a fixed
rate, setting it on these depth-carrying traces gives a periodic depth readout
(the equivalent of a periodic queue-depth tracer) straight from config, with no
code change. The `Observer` trace is unfiltered today (see the table above), so
it too runs at roughly chain throughput. Below the application traces, the raw
`ouroboros-network`
protocol is traced verbosely under the `TxSubmission2` and `KeepAlive`
namespaces.

## Cardano Implementation (`tx-centrifuge`)

### Value Builder Parameters
These parameters define the **transaction profile** for a workload:
- `inputs_per_tx` / `outputs_per_tx`: Controls the transaction structure (size and complexity).
- `fee`: Fixed Lovelace fee per transaction.
- `destination_signing_key` (optional): Path to a `.skey` file (a payment or genesis UTxO key, like the `signing_key` entries in funds.json) whose address receives every output this builder produces and which spends the recycled UTxOs. When omitted, a built-in per-builder key is derived instead. Supplying your own key lets you fund and inspect a known address, which is printed to stderr at startup.
- `recycle` (optional): Controls when output UTxOs are returned to the input queue. See [Resource Recycling](#resource-recycling) for the three strategies (`on_build`, `on_pull`, `on_confirm`). When omitted, outputs are **not recycled** — the generator consumes initial funds and eventually exhausts them.

## Usage

```bash
tx-centrifuge config.json
```

## Detailed Examples

### 1. High-Throughput (On-Build Recycling)
Optimized for maximum TPS using simple 1-in/1-out transactions. Outputs are recycled immediately after building (`on_build`), before the transaction enters the payload queue.

**`config.json` snippet:**
```json
{
  "initial_inputs": {
    "type": "genesis_utxo_keys",
    "params": {
      "network_magic": 42,
      "signing_keys_file": "funds.1.json"
    }
  },
  "builder": {
    "type": "value",
    "params": {
      "inputs_per_tx": 1,
      "outputs_per_tx": 1,
      "fee": 1000000
    },
    "recycle": { "type": "on_build" }
  },
  "rate_limit": {
    "type": "token_bucket",
    "scope": "shared",
    "params": { "tps": 1000 }
  },
  "workloads": {
    "simulation": {
      "targets": {
        "node-0": { "addr": "127.0.0.1", "port": 30000 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

**`funds.1.json` snippet:**
```json
[
  {"signing_key": "utxo1.skey", "value": 1500000000000},
  {"signing_key": "utxo2.skey", "value": 1500000000000}
]
```

### 2. Large Transactions (Target-Specific Limits)
Uses complex transactions with independent rate limits for each target connection. Outputs are recycled on fetch (`on_pull`).

**`config.json` snippet:**
```json
{
  "initial_inputs": {
    "type": "genesis_utxo_keys",
    "params": {
      "network_magic": 42,
      "signing_keys_file": "funds.2.json"
    }
  },
  "builder": {
    "type": "value",
    "params": {
      "inputs_per_tx": 5,
      "outputs_per_tx": 5,
      "fee": 2000000
    },
    "recycle": { "type": "on_pull" }
  },
  "rate_limit": {
    "type": "token_bucket",
    "scope": "per_target",
    "params": { "tps": 5 }
  },
  "max_batch_size": 50,
  "on_exhaustion": "block",
  "workloads": {
    "heavy-load": {
      "targets": {
        "edge-node": { "addr": "192.168.1.10", "port": 30001 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

**`funds.2.json` snippet:**
```json
[
  {"signing_key": "utxo1.skey", "value": 1000000000},
  {"signing_key": "utxo2.skey", "value": 1000000000},
  {"signing_key": "utxo3.skey", "value": 1000000000}
]
```

### 3. Confirmation-Based Recycling (On-Confirm with Observer)
Uses an observer that follows the chain to track when submitted transactions are confirmed on-chain. Outputs are only recycled back to the input queue after the transaction reaches a configured confirmation depth, protecting against mempool eviction and short rollbacks. Rolled-back transactions are held in limbo and orphaned (original inputs recycled) if they do not reappear within 2×`confirmation_depth` blocks.

Two observer types are supported:

#### `nodetonode` — N2N ChainSync + BlockFetch

Connects to a remote node over TCP. The observer follows the chain via ChainSync (headers) and fetches block bodies via BlockFetch to extract transaction IDs.

```json
"observers": {
  "chain-follower": {
    "type": "nodetonode",
    "params": {
      "addr": "127.0.0.1",
      "port": 30000,
      "confirmation_depth": 2
    }
  }
}
```

| Field                | Type   | Description |
|----------------------|--------|-------------|
| `addr`               | string | IP address of the node |
| `port`               | int    | Node-to-node port |
| `confirmation_depth` | int    | Blocks to wait before confirming (0 = immediate) |

#### `nodetoclient` — N2C LocalChainSync

Connects to the local node over a Unix domain socket. The observer follows the chain via LocalChainSync, which delivers full blocks directly — no separate BlockFetch needed. This is simpler, and forward-compatible with Leios (see `docs/Leios.md`).

```json
"observers": {
  "local-follower": {
    "type": "nodetoclient",
    "params": {
      "socket_path": "/tmp/node.socket",
      "confirmation_depth": 2
    }
  }
}
```

| Field                | Type   | Description |
|----------------------|--------|-------------|
| `socket_path`        | string | Path to the node's local Unix domain socket |
| `confirmation_depth` | int    | Blocks to wait before confirming (0 = immediate) |

#### Choosing between the two

Both emit the same `BlockTx` events on the same broadcast channel type, so the downstream recycler and workloads are unaffected by the choice. The builder references the observer by name regardless of type:

```json
"recycle": { "type": "on_confirm", "params": "local-follower" }
```

| Concern                  | `nodetonode`                        | `nodetoclient`                   |
|--------------------------|-------------------------------------|----------------------------------|
| Transport                | TCP (any reachable node)            | Unix socket (co-located node)    |
| Leios forward-compatible | No — needs new IB/EB relay clients  | Yes — node serves merged blocks  |
| Protocols on the mux     | ChainSync + BlockFetch + KeepAlive  | ChainSync only                   |
| Requires local node      | No                                  | Yes                              |

#### Full example (`nodetoclient`)

```json
{
  "initial_inputs": {
    "type": "genesis_utxo_keys",
    "params": {
      "network_magic": 42,
      "signing_keys_file": "funds.3.json"
    }
  },
  "observers": {
    "local-follower": {
      "type": "nodetoclient",
      "params": {
        "socket_path": "/tmp/node.socket",
        "confirmation_depth": 2
      }
    }
  },
  "builder": {
    "type": "value",
    "params": {
      "inputs_per_tx": 2,
      "outputs_per_tx": 2,
      "fee": 1000000
    },
    "recycle": { "type": "on_confirm", "params": "local-follower" }
  },
  "rate_limit": {
    "type": "token_bucket",
    "scope": "shared",
    "params": { "tps": 50 }
  },
  "max_batch_size": 500,
  "on_exhaustion": "error",
  "workloads": {
    "confirmed-load": {
      "targets": {
        "node-0": { "addr": "127.0.0.1", "port": 30000 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

**`funds.3.json` snippet:**
```json
[
  {"signing_key": "utxo1.skey", "value": 1500000000000},
  {"signing_key": "utxo2.skey", "value": 1500000000000},
  {"signing_key": "utxo3.skey", "value": 1500000000000}
]
```

With `on_confirm`, the generator needs enough initial funds to cover the in-flight transactions between submission and confirmation. At 50 TPS with a 2-block confirmation depth (~40 seconds on a 20-second slot), roughly 2000 transactions will be pending at any time, so the initial fund pool should have at least that many UTxOs.

## Internals

### Package Structure

```
tx-centrifuge/
├── tx-centrifuge.cabal          # Package definition
├── app/
│   └── Main.hs                  # Executable entry point
├── lib/
│   ├── pull-fiction/            # Domain-independent load generation library
│   │   └── Cardano/Benchmarking/PullFiction/
│   │       ├── Config/
│   │       │   ├── Raw.hs           # JSON parsing (no validation)
│   │       │   ├── Validated.hs     # Validation + cascading defaults
│   │       │   └── Runtime.hs       # Resolves config into named pools + limiters
│   │       ├── Clock.hs             # Monotonic time source
│   │       ├── WorkloadRunner.hs    # Rate-limited per-target workers
│   │       └── Internal/
│   │           ├── Pipe.hs          # Generic input + payload queue pair
│   │           ├── RateLimiter.hs   # GCRA token bucket
│   │           └── Recycler.hs      # Closed-loop input recycling (worker + strategy)
│   │
│   └── tx-centrifuge/           # Cardano-specific library
│       └── Cardano/Benchmarking/TxCentrifuge/
│           ├── Block.hs             # Shared block types and tx extraction
│           ├── Fund.hs              # UTxO/fund loading from JSON
│           ├── NodeToClient.hs      # Multiplexed N2C connection (local socket)
│           ├── NodeToClient/
│           │   ├── TxIdSync.hs      # LocalChainSync tx confirmation (full blocks)
│           │   └── TxSubmission.hs  # LocalTxSubmission client
│           ├── NodeToNode.hs        # Multiplexed N2N connection (TCP)
│           ├── NodeToNode/
│           │   ├── KeepAlive.hs     # KeepAlive mini-protocol client
│           │   ├── TxIdSync.hs      # ChainSync + BlockFetch tx confirmation
│           │   └── TxSubmission.hs  # TxSubmission2 mini-protocol client
│           ├── TxAssembly.hs        # Transaction building and signing
│           ├── Tracing.hs           # Structured logging via trace-dispatcher
│           └── Tracing/
│               └── Orphans.hs       # LogFormatting/MetaTrace instances
│
├── test/                        # Test suites
│   ├── lib/                     # Shared test harness (private library)
│   │   └── Test/PullFiction/
│   │       └── Harness.hs
│   ├── pull-fiction/            # Pull-fiction unit tests
│   └── tx-centrifuge/           # Tx-centrifuge unit tests
│
└── bench/                       # Benchmarks
    └── Bench.hs
```

### Data Flow

```
Raw JSON → Validated Config → Runtime (STM queues, rate limiters, builder asyncs)
                                        │
         ┌──────────────────────────────┘
         ↓
   [Builder Async]         per workload
     reads TQueue(inputs) → buildTx → TBQueue(payloads, 8192 cap)
         ↓
   [Worker Asyncs]         per target
     GCRA rate-limited fetch → TxSubmission2 pull protocol → cardano-node
         ↓
   [Recycler]              closed-loop
     outputs → back to TQueue(inputs)
```
