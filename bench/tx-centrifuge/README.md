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
  "nodeConfig": "node-config.json",
  "protocol_parameters": {
    "epoch_length": 600,
    "min_fee_a": 44,
    "min_fee_b": 155381
  }
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
2.  **`on_pull`** — The builder pairs the payload with the resources to be recycled as a `(payload, [input])` tuple in the payload queue. When a worker **fetches** this tuple (triggered by a downstream request), the library returns those resources to the `Input Queue` in a separate STM transaction before handing the payload to the worker. Note: recycling happens on fetch, not on downstream acknowledgement — if the worker is killed between fetch and delivery, those inputs are lost.
    ```json
    "recycle": { "type": "on_pull" }
    ```
3.  **`on_confirm`** — Resources stay in a pending map until an **observer** (ChainSync + BlockFetch) confirms the transaction on-chain at the configured confirmation depth. The builder enqueues the payload without any inputs; a background recycler async reads confirmations from the observer's broadcast channel and recycles matching inputs. This is the safest mode for long-running benchmarks where mempool eviction is a concern.
    ```json
    "recycle": { "type": "on_confirm", "params": "my-observer" }
    ```

## Configuration

### Initial Inputs (`initial_inputs`)
The generator requires a set of initial UTxOs, configured in the `initial_inputs` section of the main configuration file.

- **`type`**: The input loader variant (e.g., `"genesis_utxo_keys"`).
- **`params`**:
  - **`network_magic`**: Required for deriving UTxO references from keys (e.g., `42` for testnet).
  - **`signing_keys_file`**: Path to a JSON file (e.g., `funds.json`) containing the actual fund data.

#### `funds.json` entry types
The file contains an array of fund objects. There are two distinct types:

1.  **Genesis Funds** (Key-only): Identified only by their signing key. The `TxIn` is derived automatically.
    ```json
    { "signing_key": "genesis.skey", "value": 1500000000000 }
    ```
2.  **Payment Funds** (Explicit UTxO): Requires a specific transaction reference.
    ```json
    { "signing_key": "payment.skey", "value": 1000000, "tx_in": "df6...#0" }
    ```

**Design Note**: The `funds.json` format is designed to be compatible with the output of `cardano-cli conway create-testnet-data --utxo-keys`. This allows you to immediately use an arbitrary large set of Shelley genesis keys created during testnet bootstrapping as the initial fund pool for the generator, without needing to manually create UTxOs once the network is live.

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
- **`max_batch_size`**: target > workload > top-level > **1**.
- **`on_exhaustion`**: target > workload > top-level > **`block`**.

Workload and target names must be non-empty and must not contain `.` or start with `@` (reserved for internal rate-limiter cache keys).

### Batching and Flow Control
- **`max_batch_size`**: Limits the number of items (e.g., transactions) the generator will announce to a target in a single protocol request.
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
  "nodeConfig": "node-config.json",
  "protocol_parameters": {
    "epoch_length": 600,
    "min_fee_a": 44,
    "min_fee_b": 155381
  }
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
  "nodeConfig": "node-config.json",
  "protocol_parameters": {
    "epoch_length": 600,
    "min_fee_a": 44,
    "min_fee_b": 155381
  }
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
Uses an observer that follows the chain via ChainSync + BlockFetch to track when submitted transactions are confirmed on-chain. Outputs are only recycled back to the input queue after the transaction reaches a confirmation depth of 2 blocks, protecting against mempool eviction and short rollbacks.

**`config.json` snippet:**
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
    "chain-follower": {
      "type": "nodetonode",
      "params": {
        "addr": "127.0.0.1",
        "port": 30000,
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
    "recycle": { "type": "on_confirm", "params": "chain-follower" }
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
  "nodeConfig": "node-config.json",
  "protocol_parameters": {
    "epoch_length": 600,
    "min_fee_a": 44,
    "min_fee_b": 155381
  }
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
│   │       │   └── Runtime.hs       # STM resources, rate limiters, builder spawning
│   │       ├── Clock.hs             # Monotonic time source
│   │       ├── WorkloadRunner.hs    # Rate-limited workload execution
│   │       └── Internal/
│   │           └── RateLimiter.hs   # GCRA token bucket
│   │
│   └── tx-centrifuge/           # Cardano-specific library
│       └── Cardano/Benchmarking/TxCentrifuge/
│           ├── Fund.hs              # UTxO/fund loading from JSON
│           ├── NodeToNode.hs        # Multiplexed N2N connection (4 mini-protocols)
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
