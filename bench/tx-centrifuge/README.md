# Tx Centrifuge & Pull-Fiction

`tx-centrifuge` is a high-performance load generator for Cardano, built on top of the protocol-agnostic **Pull-Fiction** library.

Unlike traditional load generators that "push" data at a fixed rate, this system is designed for **pull-based protocols**. It does not generate load by itself; instead, it acts as a **policer** that reacts to requests from downstream consumers, admitting or delaying them to enforce a configured rate ceiling.

### Minimal Configuration Example

A basic configuration defines the recycle signing key, the local node socket used for UTxO discovery, how to build payloads, the desired rate, and where to send the results:

```json
{
  "signing_key_file": "/run/secrets/tx-centrifuge.skey",
  "observers": {
    "local-follower": {
      "type": "nodetoclient",
      "params": {
        "socket_path": "/run/cardano-node/node.socket",
        "confirmation_depth": 2
      }
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

> **Initial UTxOs are discovered on-chain at startup**, not loaded from a JSON file. The operator funds the recycle addresses derived from `signing_key_file`; tx-centrifuge queries the local node (via the `nodetoclient` observer's socket) for the live UTxO set at every restart. See [Recycle Signing Key](#recycle-signing-key) and [Funding Recycle Addresses](#funding-recycle-addresses) below.

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
2.  **`on_pull`** — The builder pairs the payload with the resources to be recycled as a `(payload, [input])` tuple in the payload queue. When a worker **fetches** this tuple (triggered by a downstream request), the library returns those resources to the `Input Queue` in a separate STM transaction before handing the payload to the worker. Note: recycling happens on fetch, not on downstream acknowledgement — if the worker is killed between fetch and delivery, those inputs are lost.
    ```json
    "recycle": { "type": "on_pull" }
    ```
3.  **`on_confirm`** — Resources stay in a pending map until an **observer** confirms the transaction on-chain at the configured confirmation depth. The builder enqueues the payload without any inputs; a background recycler async reads confirmations from the observer's broadcast channel and recycles matching inputs. This is the safest mode for long-running benchmarks where mempool eviction is a concern.
    ```json
    "recycle": { "type": "on_confirm", "params": "my-observer" }
    ```

## Configuration

### Recycle Signing Key (`signing_key_file`)

The operator supplies a single secret payment key in a standard cardano-cli text envelope (`PaymentSigningKey_ed25519` or `GenesisUTxOSigningKey_ed25519`). tx-centrifuge uses it as the **recycle key**: the key signing every transaction's outputs, and whose addresses hold the live UTxO pool between restarts.

- **Format**: standard cardano-cli `.skey` file.
- **Required suffix**: the raw 32-byte secret must end in `0x000` (last 3 hex characters of the hex-encoded key). This guarantees workload 0's derived key is the operator-supplied key exactly.
- **Per-workload derivation**: tx-centrifuge replaces the trailing 3 hex characters with the workload index (`%03d`), yielding distinct keys for workloads 0..999. Multiple workloads thus get distinct recycle addresses, derived deterministically from the one seed key.
- **Deployment tip**: store at a path the service has read access to but is otherwise restricted, e.g. `/run/secrets/tx-centrifuge.skey` for systemd `LoadCredential` or container secret mounts.

### Initial UTxOs (on-chain discovery)

tx-centrifuge does **not** read initial UTxOs from a config file. At every startup it derives all 1000 recycle addresses from the seed key (`000..999`) and issues a single `QueryUTxOByAddress` against the local node to find every spendable UTxO across those addresses. This makes restarts stateless — there is no on-disk state to corrupt, and any UTxOs at any of the 1000 addresses are picked up automatically (including leftovers from a previous configuration with more workloads).

- **Where the query socket comes from**: the `socket_path` of the **first** `nodetoclient` observer in the `observers` map, alphabetical by observer name. Declare at least one such observer; tx-centrifuge dies at startup if none is present and prints a config example.
- **Empty UTxO set behaviour**: if *no* UTxOs are found at any of the 1000 addresses, tx-centrifuge dies with a clear message including workload 0's bech32 address and a sample `cardano-cli` funding command. Partial funding (e.g. only `000` funded with 3 workloads declared) is fine — the round-robin partition spreads the available UTxOs across active workloads.

### Funding Recycle Addresses

To bootstrap or top up, send ADA to one or more recycle addresses derived from the seed. Workload 0's address is the natural default. tx-centrifuge prints all derived addresses with non-zero balances at startup, so you can read them straight from the logs.

```bash
# Fund workload 0's address.
cardano-cli conway transaction build \
  --tx-in <some-funded-utxo> \
  --tx-out <addr from tx-centrifuge logs>+<lovelace> \
  ...
cardano-cli conway transaction sign ...
cardano-cli conway transaction submit ...
```

After the funding transaction confirms, restart tx-centrifuge; it will discover the new UTxOs on the next query.

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

## Cardano Implementation (`tx-centrifuge`)

### Value Builder Parameters
These parameters define the **transaction profile** for a workload:
- `inputs_per_tx` / `outputs_per_tx`: Controls the transaction structure (size and complexity).
- `fee`: Fixed Lovelace fee per transaction.
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
  "signing_key_file": "/run/secrets/tx-centrifuge.skey",
  "observers": {
    "local-follower": {
      "type": "nodetoclient",
      "params": {
        "socket_path": "/run/cardano-node/node.socket",
        "confirmation_depth": 0
      }
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

### 2. Large Transactions (Target-Specific Limits)
Uses complex transactions with independent rate limits for each target connection. Outputs are recycled on fetch (`on_pull`).

**`config.json` snippet:**
```json
{
  "signing_key_file": "/run/secrets/tx-centrifuge.skey",
  "observers": {
    "local-follower": {
      "type": "nodetoclient",
      "params": {
        "socket_path": "/run/cardano-node/node.socket",
        "confirmation_depth": 0
      }
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
  "signing_key_file": "/run/secrets/tx-centrifuge.skey",
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

With `on_confirm`, the generator needs enough initial funds at its recycle addresses to cover the in-flight transactions between submission and confirmation. At 50 TPS with a 2-block confirmation depth (~40 seconds on a 20-second slot), roughly 2000 transactions will be pending at any time, so the on-chain UTxO pool should have at least that many entries.

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
│   │       │   └── Runtime.hs       # STM resources, rate limiters, builder spawning
│   │       ├── Clock.hs             # Monotonic time source
│   │       ├── WorkloadRunner.hs    # Rate-limited workload execution
│   │       └── Internal/
│   │           └── RateLimiter.hs   # GCRA token bucket
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
