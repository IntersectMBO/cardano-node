# Transaction Generator

The transaction generator stresses a cluster of Cardano nodes with a simulated workload of transactions.
It works in two phases:

* **Phase 1**: Secure a genesis fund and prepare a set of initial UTxOs via the local protocol.
* **Phase 2**: Connect to a set of nodes via the node-to-node protocol and submit transactions at a specified rate limit.

## Getting started

Create a JSON configuration file (e.g., `config.json`):

```json
{
  "debugMode": false,
  "tx_count": 100,
  "tps": 10,
  "inputs_per_tx": 2,
  "outputs_per_tx": 2,
  "tx_fee": 212345,
  "min_utxo_value": 1000000,
  "add_tx_size": 39,
  "init_cooldown": 50,
  "era": "Conway",
  "keepalive": 30,
  "localNodeSocketPath": "/path/to/node.socket",
  "nodeConfigFile": "/path/to/config.json",
  "sigKey": "/path/to/genesis-utxo.skey",
  "targetNodes": [
    {"addr": "127.0.0.1", "port": 30000, "name": "node0"},
    {"addr": "127.0.0.1", "port": 30001, "name": "node1"}
  ],
  "plutus": null
}
```

Run the generator:

```bash
cabal run tx-generator -- json_highlevel config.json
```

Optionally override node config or add cardano-tracer:

```bash
cabal run tx-generator -- json_highlevel config.json \
  --nodeConfig /path/to/node-config.yaml \
  --cardano-tracer /tmp/tracer.socket
```

## Configuration Parameters

### Connection Settings

| Parameter             | Description                                                     | Required |
|-----------------------|-----------------------------------------------------------------|----------|
| `localNodeSocketPath` | Path to local node socket for protocol queries                  | Yes      |
| `targetNodes`         | List of nodes to submit transactions to (Node-to-Node protocol) | Yes      |
| `nodeConfigFile`      | Path to node configuration file                                 | Yes      |
| `sigKey`              | Path to signing key with sufficient funds (genesis key)         | Yes      |

### Transaction Settings

| Parameter        | Description                              | Default  |
|------------------|------------------------------------------|----------|
| `tx_count`       | Total number of transactions to generate | Required |
| `tps`            | Transactions per second submission rate  | Required |
| `inputs_per_tx`  | Number of inputs per transaction         | 2        |
| `outputs_per_tx` | Number of outputs per transaction        | 2        |
| `tx_fee`         | Fee per transaction (lovelace)           | 212345   |
| `min_utxo_value` | Minimum UTxO value (lovelace)            | 1000000  |
| `add_tx_size`    | Additional transaction size (bytes)      | 39       |

### Other Settings

| Parameter       | Description                                                   | Default  |
|-----------------|---------------------------------------------------------------|----------|
| `era`           | Cardano era (Shelley, Allegra, Mary, Alonzo, Babbage, Conway) | Required |
| `keepalive`     | Keepalive timeout for mini-protocol (seconds)                 | 30       |
| `init_cooldown` | Cooldown period after initialization (seconds)                | 50       |
| `debugMode`     | Enable debug logging                                          | false    |
| `plutus`        | Plutus script configuration (null for simple transfers)       | null     |

## How It Works

The high-level JSON configuration is automatically compiled into a multi-phase script that runs sequentially:

### Phase 1: Genesis Fund Import
- Connects to local node via socket (`localNodeSocketPath`)
- Reads protocol parameters from `nodeConfigFile`
- Imports genesis funds from `sigKey` via local submission
- Waits for cooldown period (`init_cooldown`)

### Phase 2: Collateral Creation (if Plutus enabled)
- Creates collateral UTxOs for Plutus script execution
- Submits via local socket

### Phase 3: UTxO Splitting
- Splits funds into the required number of UTxOs
- Creates `tx_count * inputs_per_tx` UTxOs for benchmarking
- May involve multiple splitting rounds for large numbers
- Each splitting step waits for cooldown period

### Phase 4: Benchmark Submission
- Connects to target nodes via Node-to-Node protocol on P2P port
- Uses the ports specified in `targetNodes` (e.g., 30000)
- Spawns one submission thread per target node
- Submits transactions at the specified TPS rate
- Waits for all transactions to complete (unless `debugMode: true`)
- Reports final submission statistics

**Important**: All phases use the **local node socket** for setup (phases 1-3), and only phase 4 connects to **target nodes** via Node-to-Node protocol for actual benchmarking.

## Low-Level JSON Script (Advanced)

For fine-grained control, use low-level JSON scripts. Example in `test/script.json`:

```bash
cabal run tx-generator -- json test/script.json
```

See `bench/script/test-large.ljson` for more examples.

## Performance Tuning

### For Maximum Transaction Speed:
- Increase `tps` value (100+)
- Add multiple nodes to `targetNodes` array
- Ensure low-latency network connection to target nodes
- Monitor with cardano-tracer for real-time metrics
- Use era-appropriate transaction formats

### Target Node Requirements:
- Nodes must be listening on specified ports (Node-to-Node protocol)
- Typically the P2P port (default: 30000+)
- Not the CLI/API port (default: 3001)

## Examples

### Simple Devnet Test
```json
{
  "tx_count": 1000,
  "tps": 50,
  "inputs_per_tx": 2,
  "outputs_per_tx": 2,
  "era": "Conway",
  "localNodeSocketPath": "./devnet/node.socket",
  "nodeConfigFile": "./devnet/config.json",
  "sigKey": "./devnet/genesis-utxo.skey",
  "targetNodes": [
    {"addr": "127.0.0.1", "port": 30000, "name": "local-node"}
  ]
}
```

### Multi-Node Load Test
```json
{
  "tx_count": 100000,
  "tps": 200,
  "inputs_per_tx": 4,
  "outputs_per_tx": 4,
  "era": "Conway",
  "localNodeSocketPath": "./node-0/node.socket",
  "nodeConfigFile": "./node-0/config.json",
  "sigKey": "./genesis-keys/utxo1.skey",
  "targetNodes": [
    {"addr": "10.0.1.10", "port": 30000, "name": "node0"},
    {"addr": "10.0.1.11", "port": 30000, "name": "node1"},
    {"addr": "10.0.1.12", "port": 30000, "name": "node2"}
  ]
}
```

## Troubleshooting

### Common Issues:

1. **"AsyncBenchmarkControl uninitialized"**
   - This means the generator failed during the setup phases (1-3) before reaching the benchmark phase
   - Check that `localNodeSocketPath` points to a running node's socket
   - Verify `nodeConfigFile` path is correct and readable
   - Ensure `sigKey` has sufficient funds (at least several million ADA for genesis wallets)
   - Confirm the era in your config matches your devnet's current era
   - Check the generator output for errors in earlier phases

2. **"Cannot connect to node"** (during benchmark phase)
   - Verify `targetNodes` port is the **P2P/Node-to-Node port** (typically 30000+)
   - Do NOT use the CLI/API port (typically 3001)
   - Ensure target nodes are actually listening on specified ports
   - Check firewall rules if connecting to remote nodes

3. **"Insufficient funds"**
   - The `sigKey` must point to a genesis UTxO key with substantial ADA
   - Required funds ≈ `(tx_count * inputs_per_tx * min_utxo_value) + (tx_count * tx_fee)`
   - For 1000 txs with 2 inputs: need at least ~2B lovelace (2000 ADA)

4. **"Protocol version mismatch"**
   - Set `era` to match your devnet's current era (query with `cardano-cli query tip`)
   - Common eras: Conway, Babbage, Alonzo, Mary, Allegra, Shelley

5. **"Timeout" / "Keepalive expired"**
   - Increase `keepalive` value (try 60 or 120)
   - Increase `init_cooldown` if splits are failing (try 15.0 or 20.0)
   - Check node performance and network latency

6. **Script hangs during splitting phase**
   - Node might be slow to process transactions
   - Increase `init_cooldown` to allow more time between phases
   - Check node logs for errors or resource issues

### Debug Mode

Set `"debugMode": true` in your config to:
- Use local socket for all submissions (instead of Node-to-Node)
- Skip waiting for benchmark completion
- Get more detailed logging
- Useful for testing configuration without actual load testing
