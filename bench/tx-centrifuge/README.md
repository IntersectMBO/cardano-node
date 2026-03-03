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
  "builder": { "type": "value", "params": { "fee": 1000000 } },
  "rate_limit": { "type": "token_bucket", "params": { "tps": 10 } },
  "workloads": {
    "group-A": {
      "targets": {
        "node-1": { "addr": "127.0.0.1", "port": 30000 },
        "node-2": { "addr": "127.0.0.1", "port": 30001 }
      }
    }
  },
  "nodeConfig": "config.json",
  "protocolParametersFile": "pp.json"
}
```

## Core Concepts: The Pull-Fiction Engine

The underlying `pull-fiction` library implements a reactive rate-limiting strategy. It only produces data when a consumer asks for it, and only as fast as the rate limiter allows.

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
1.  **Initial Inputs**: Starting resources (of type `input`) are partitioned across workloads.
2.  **Input Queue (Unbounded)**: Holds available `input` items.
3.  **Builder (One per Workload)**: A dedicated thread that pulls `input`s, produces a `payload`, and pairs it with any `[input]`s to be recycled. It pushes the `(payload, [input])` pair to the payload queue.
4.  **Payload Queue (Bounded)**: The sole source of **backpressure**. The builder blocks here if consumers are slower than the production rate.
5.  **Workers (One per Target)**: Threads that manage the consumer connection. They pull from the payload queue via a rate-limited fetcher.

### Resource Recycling
To enable indefinite-duration runs with finite resources, inputs must be returned to the `Input Queue`. There are two main patterns:

1.  **Optimistic Recycling (Builder-level)**: The builder immediately returns resources to the `Input Queue` as soon as the payload is constructed, before it even enters the payload queue. This is the highest-throughput mode but assumes the payload will be successfully processed downstream.
2.  **Standard Recycling (On-Fetch)**: The builder pairs the payload with the resources to be **recycled** as a `(payload, [input])` tuple in the payload queue. When a worker **fetches** this tuple from the payload queue (triggered by a downstream request), the library returns those resources to the `Input Queue` in a separate STM transaction before handing the payload to the worker. Note: recycling happens on fetch, not on downstream acknowledgement — if the worker is killed between fetch and delivery, those inputs are lost.

## Configuration

### Initial Inputs (`initial_inputs`)
The generator requires a set of initial UTxOs, configured in the `initial_inputs` section of the main configuration file.

- **`type`**: The input loader variant (e.g., `"genesis_utxo_keys"`).
- **`params`**:
- - **`network_magic`**: Required for deriving UTxO references from keys (e.g., `42` for testnet).
- - **`signing_keys_file`**: Path to a JSON file (e.g., `funds.json`) containing the actual fund data.

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
- `optimistic_recycle`:
  - `true`: Output UTxOs are recycled immediately by the builder, before the transaction enters the payload queue.
  - `false`: Output UTxOs are recycled when a worker fetches the transaction from the payload queue (before it is delivered to the node).

## Usage

```bash
tx-centrifuge config.json
```

## Detailed Examples

### 1. High-Throughput (Optimistic Recycling)
Optimized for maximum TPS using simple 1-in/1-out transactions.

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
      "fee": 1000000,
      "optimistic_recycle": true
    }
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
  "nodeConfig": "config.json",
  "protocolParametersFile": "pp.json"
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
Uses complex transactions with independent rate limits for each target connection.

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
      "fee": 2000000,
      "optimistic_recycle": false
    }
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
