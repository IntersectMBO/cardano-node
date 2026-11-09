// Diverse Cardano node implementations in the performance workbench: integration requirements.
// Build: typst compile workbench-integration.typ
// The design (ICAN Group palette and typefaces) lives in template/, shared with perf-observables-spec.typ.

#import "template/lib.typ": basic-report

#let doc-version = "draft 0.1"
#let doc-date = "2026-11-09"

#set outline(depth: 2)

#show: it => basic-report(
  doc-category: [Requirements · #doc-version · #doc-date],
  doc-title: "Diverse Cardano nodes: performance workbench",
  title-accent: "integration",
  author: "Michael Karg",
  affiliation: "ICAN Group",
  datetime-fmt: "Built [year]-[month]-[day]",
  it,
)

// Table helper: options and keys.
#let items(widths: (auto, 1fr), head: ([Item], [Requirement]), ..cells) = table(
  columns: widths,
  table.header(..head),
  ..cells,
)

// ----- Front matter -------------------------------------------------

#heading(numbering: none, outlined: true)[About this document]

*Status:* #doc-version (#doc-date)

*Reference:* cardano-node 11.2.0, as run by the workbench in `nix/workbench/` of the cardano-node repository.

*Companion document:* _Cardano node performance observability_ (`perf-observables-spec.typ`, in this directory). It specifies the events a node emits for the analysis. This document covers everything else a node needs to run in the workbench: delivery, invocation, input files, protocols, process behaviour and workload.

The workbench treats a node implementation as a *self-contained executable*: it builds it, starts it with a fixed command line, and talks to it only over the network protocols and through its log stream. It never passes options specific to a language runtime. This keeps the integration the same for any implementation, whatever it's written in.

Most requirements below aren't specific to the workbench: they're what any node needs to take part in a Cardano network (the network protocols, encodings and ledger rules). The workbench-specific parts are delivery (@sec-delivery), invocation (@sec-invocation) and process behaviour (@sec-process).

The key words MUST, MUST NOT, SHOULD, SHOULD NOT and MAY are to be interpreted as described in RFC 2119.

// ----- 1 -------------------------------------------------------------

= A workbench run <sec-run>

A benchmark run is defined by a _profile_: cluster size and topology, genesis parameters, and workload. A run on the local backend goes like this:

+ The workbench creates the genesis files and keys with `cardano-cli create-testnet-data`, and a configuration and topology for each node.
+ `supervisord` starts every node of the cluster on `127.0.0.1`, each in its own working directory, shortly before the genesis system start.
+ A healthcheck verifies that all nodes are connected and synced (@sec-healthcheck).
+ The transaction generator (`tx-generator`) prepares its funds through one node's local socket, then submits the workload to all nodes over the node-to-node protocol (@sec-workload).
+ The block producers stop by themselves once their chain reaches the profile's last slot or block (@sec-invocation). The workbench then stops everything else, and the analysis (`locli`) computes the report from each node's log stream.

The run directory, e.g. for a 6-node cluster:

```
run/<run-id>/
  profile.json         the profile
  genesis/             genesis.{byron,shelley,alonzo,conway,dijkstra}.json,
                       pools-keys/poolN/, utxo-keys/, ...
  node-0/ … node-5/    one working directory per node:
                       start.sh, config.json, topology.json,
                       stdout, stderr, db/, node.socket
  generator/           tx-generator
  healthcheck/         healthcheck
```

The reference for a working integration is a benchmark on the local backend; this document describes that. Runs on the workbench's cloud backend, with larger clusters on separate machines, may follow later, but aren't part of this integration.

To compare implementations, the same profile is run once per implementation, with all nodes of a cluster running the same implementation. Trace forwarding (to `cardano-tracer`) is out of scope: comparisons use profiles without it, so every node writes its events to stdout (@sec-observability).

// ----- 2 -------------------------------------------------------------

= Delivery <sec-delivery>

#items(
  [Nix flake], [The implementation MUST be provided as a Nix flake, which the workbench uses as a flake input.],
  [Package], [The flake MUST expose a package `packages.x86_64-linux.<name>`, with `<name>` agreed on, whose `bin/` holds exactly one executable: the node. If the log stream needs translating by a tool (@sec-observability), that tool SHOULD be a separate package of the same flake.],
  [Self-contained], [The executable MUST NOT depend on anything outside its Nix closure (e.g. a language runtime installed on the host), and MUST NOT need network access other than to its peers.],
  [Runtime settings], [Settings of the language runtime, if any (memory management, garbage collection, compilation), MUST be part of the executable, e.g. in a launcher script that the package's `bin/` entry provides. The workbench passes none.],
  [Cores], [The executable MUST limit its parallelism to a fixed number of cores, chosen by the implementation (e.g. 2 to 4) and documented with it. For reference, the Haskell node runs with 2 (`+RTS -N2`). The limit is part of the executable, like any other runtime setting.],
  [Memory], [Benchmark machines have 16 GiB of RAM, and the workbench sets no memory limit of its own. For reference, the Haskell node runs without a heap limit, bounded only by the machine's memory. An implementation's memory settings MUST be deliberate and documented: the bound SHOULD be the machine's memory, not a runtime default (a JVM, for example, caps its heap at a quarter of the memory it sees unless told otherwise).],
  [Revision], [The workbench identifies the implementation by the flake input's revision (`shortRev`), and records it in the run's metadata. Builds SHOULD therefore come from a git revision.],
  [Build], [The package MUST build from the flake without manual steps. It MAY use prebuilt release artifacts, fetched with fixed hashes, instead of building from source. The workbench builds it from the implementation's repository, so it needs read access to it.],
)

For example, the `bin/` entry can be a launcher script that sets the runtime's options and `exec`s the node: `-XX:ActiveProcessorCount` and `-XX:MaxRAMPercentage` on a JVM, `GOMAXPROCS` for Go, or the worker thread count of an asynchronous runtime in Rust.

// ----- 3 -------------------------------------------------------------

= Invocation <sec-invocation>

The workbench starts each node from a script in its working directory. The command line follows the Haskell node's. A block producer, here node `node-1`:

```
<executable> run --config config.json --database-path db --topology topology.json
  --host-addr 127.0.0.1 --port 30001 --socket-path node.socket
  --shelley-vrf-key ../genesis/pools-keys/pool2/vrf.skey
  --shelley-kes-key ../genesis/pools-keys/pool2/kes.skey
  --shelley-operational-certificate ../genesis/pools-keys/pool2/opcert.cert
```

Paths are relative to the working directory, `node-<i>/`. The node MUST accept these options:

#items(
  head: ([Option], [Meaning]),
  [`run`], [Subcommand: run the node.],
  [`--config`], [The node configuration (@sec-config).],
  [`--database-path`], [The directory for the node's chain and ledger state. The node MUST create it if it doesn't exist. Its layout is up to the implementation.],
  [`--topology`], [The peers to connect to (@sec-topology).],
  [`--host-addr` \ `--port`], [Address and port to listen on for node-to-node connections.],
  [`--socket-path`], [The Unix domain socket to listen on for node-to-client connections (@sec-protocols). The workbench takes its creation as the sign that the node has started.],
  [`--shutdown-on-slot-synced` \ `--shutdown-on-block-synced`], [Exit with success as soon as the tip of the node's current chain has reached the given slot number, or block number (≥). Most profiles pass one of the two; they're how a run normally ends (@sec-process).],
  [`--shelley-vrf-key` \ `--shelley-kes-key` \ `--shelley-operational-certificate`], [The block producer's credentials (@sec-keys). Only given to block producers; a node without them doesn't forge.],
)

The workbench passes no other options. The start script also sets the environment variable `TRACE_DISPATCHER_LOGGING_HOSTNAME` to the node's name; the node MAY ignore it, as the name is also in the configuration.

// ----- 4 -------------------------------------------------------------

= Input files <sec-inputs>

== Node configuration <sec-config>

`config.json` is in the Haskell node's format. The transaction generator reads it too, so the workbench doesn't provide it in any other format. The keys that matter to the node:

#items(
  head: ([Key], [Meaning]),
  [`Protocol`], [Always `"Cardano"`.],
  [`ByronGenesisFile` \ `ShelleyGenesisFile` \ `AlonzoGenesisFile` \ `ConwayGenesisFile` \ `DijkstraGenesisFile`], [Paths of the genesis files, relative to the working directory (e.g. `../genesis/genesis.shelley.json`).],
  [`TestShelleyHardForkAtEpoch` \ … \ `TestConwayHardForkAtEpoch`], [All `0`: the chain is in Conway from its first block (@sec-era).],
  [`RequiresNetworkMagic`], [`"RequiresMagic"`: the network uses the magic from genesis (`networkMagic` in the Shelley genesis; `42` in all profiles with a generated genesis).],
  [`TraceOptionNodeName`], [The node's name (e.g. `"node-1"`): the value of `host` in every event (@sec-observability).],
  [`LedgerDB.Snapshots`], [Only for nodes that write ledger snapshots: a snapshot is due every `SnapshotInterval` slots, the first at slot `SlotOffset`. Such a node SHOULD follow this schedule.],
)

Ledger snapshots cost CPU and I/O, so for a fair comparison both implementations either write them or don't. If the node writes snapshots, they're part of the benchmark: following the schedule above puts the snapshot load into the same slots for both implementations. If it doesn't, the workbench runs a profile that disables snapshots for the Haskell node as well.

The genesis hashes (`ByronGenesisHash` etc.) are deliberately left out: the node MUST NOT require them, and computes them itself if it needs them. All other keys, such as the trace configuration (`TraceOptions`) and peer selection targets, MAY be ignored. Profiles can add keys of their own; a node SHOULD ignore keys it doesn't know.

== Genesis <sec-genesis>

The genesis files are created by `cardano-cli create-testnet-data`, in the format of the reference version. In particular:

- The Shelley genesis holds initial funds, stake credentials and stake pools in its `extraConfig` section (`initialFunds`, `stakeCredentials`, `stakePools`, each as inline `data`). The classic `initialFunds` and `staking` fields are empty.
- The Byron genesis holds the genesis funds the transaction generator spends first (`nonAvvmBalances`).
- `genesis.dijkstra.json` is always referenced, and is a placeholder when the profile doesn't use that era. A node that doesn't support Dijkstra MUST still accept the file.
- The system start lies a few tens of seconds in the future when the nodes start (34 s in the default local profiles). The node MUST wait for it.

Genesis parameters vary with the profile, e.g. slot length 1 s, epoch length 600 slots, active slot coefficient 0.05 and security parameter 3 for small local clusters. The protocol version is also set by the profile; it can be beyond the one current on mainnet.

So does the genesis's size. The largest benchmark profiles put 4 million UTxO entries and 1 million stake delegators into it, where small profiles put almost none. Loading it is part of the node's startup, and these profiles' startup allowance grows with it, to 2000 s (@sec-process). Implementations SHOULD test with a genesis of that size early.

== Topology <sec-topology>

`topology.json` is in the Haskell node's peer-to-peer format, with fixed local roots only. Node `node-0` of a 6-node ring:

```
{ "bootstrapPeers": null,
  "localRoots": [ { "accessPoints": [ { "address": "127.0.0.1", "port": 30001 } ],
                    "advertise": false, "trustable": true, "valency": 1 } ],
  "publicRoots": [],
  "useLedgerAfterSlot": -1 }
```

The node MUST connect to all access points of its local roots and keep these connections active. There are no other peers: no public roots, no peers from the ledger (`useLedgerAfterSlot: -1`), and peer sharing is disabled. The shape of the topology (ring, torus, number of local roots per node) depends on the profile.

== Keys <sec-keys>

Block producer credentials are in the text envelope format of `cardano-cli` (JSON with `type`, `description` and `cborHex`):

#items(
  head: ([File], [Envelope `type`]),
  [`vrf.skey`], [`VrfSigningKey_PraosVRF`],
  [`kes.skey`], [`KesSigningKey_ed25519_kes_2^6`],
  [`opcert.cert`], [`NodeOperationalCertificate`],
)

// ----- 5 -------------------------------------------------------------

= Era <sec-era>

Every profile described here runs Conway from the first block: the configuration schedules all hard forks at epoch 0 (@sec-config). An implementation MAY support only Conway, provided that from the outside it behaves exactly like the Haskell node with this configuration:

- Blocks, headers, transactions and query results use the hard-fork combinator's era-tagged encodings, with Conway's era tag.
- Queries are answered in their era-wrapped form (e.g. the query for the current era, or for the protocol parameters).
- The era history reports the eras before Conway with zero length.
- The initial ledger state includes the funds of every genesis file, the Byron genesis's among them (@sec-genesis): the transaction generator spends those first.

// ----- 6 -------------------------------------------------------------

= Network protocols <sec-protocols>

The node serves two separate protocol bundles, each with its own handshake and versions: node-to-node over TCP, and node-to-client over the Unix domain socket. Apart from the handshake, chain sync is the only mini-protocol in both, carrying headers in node-to-node and full blocks in node-to-client.

== Node-to-node

The node MUST implement the node-to-node protocol over TCP, in at least one of the versions `NodeToNodeV_14`, `NodeToNodeV_15` and `NodeToNodeV_16`:

- The Haskell clients in the workbench, the transaction generator and `cardano-cli ping` (healthcheck), propose all three. The node picks the highest version it supports.
- `NodeToNodeV_16` carries features of the Dijkstra era. It's only required for profiles that benchmark that era; for the Conway profiles described here, V14 or V15 suffice.
- The handshake data: the network magic from genesis, initiator-only mode, peer sharing disabled, no query, and from V16 on, Peras unsupported. The node MUST accept initiator-only connections, over which it never acts as initiator.
- Mini-protocols: handshake, chain sync, block fetch, transaction submission (`TxSubmission2`) and keep-alive.
- The transaction generator only uses two of them: `TxSubmission2`, in which the node is the receiving side, and keep-alive. Its chain sync and block fetch clients never send a message.
- How the node decides which transactions to request from which peer is up to the implementation. The Haskell node's choice between its inbound logic variants (`TxSubmissionLogicVersion` in its configuration, unset in the workbench) doesn't change the protocol, and MAY be ignored.

Between the nodes themselves, the node MAY negotiate any version the implementation supports, as all nodes of a cluster run the same implementation.

== Node-to-client <sec-n2c>

The node MUST implement the node-to-client protocol on the socket given by `--socket-path`, in a version that the `cardano-cli` and `cardano-api` of the reference version propose. Its clients are the transaction generator (on one node, by default `node-0`, a block producer) and the healthcheck (on every node). They use:

#items(
  head: ([Mini-protocol], [Used for]),
  [Local state query], [The current era; the current protocol parameters, including the Plutus cost models; and what `cardano-cli conway query tip` needs (chain tip and block number, system start, era history).],
  [Local transaction submission], [The generator's setup transactions (@sec-workload).],
  [Local chain sync], [The current tip, which the generator reads before its queries.],
)

All results MUST be in the reference encodings, which `cardano-api` decodes: e.g. protocol parameters in the ledger's CBOR encoding.

// ----- 7 -------------------------------------------------------------

= Process behaviour <sec-process>

#items(
  head: ([Aspect], [Requirement]),
  [Starting], [The start script `exec`s the executable, under `supervisord`. The executable SHOULD in turn `exec` its runtime, so that signals reach the node process.],
  [Startup], [The node MUST keep running for at least 5 s, and MUST create its node-to-client socket within the profile's startup allowance (`analysis.cluster_startup_overhead_s`, 40 s in the default local profiles). Otherwise the workbench considers the start failed.],
  [Ending a run], [With `--shutdown-on-slot-synced` or `--shutdown-on-block-synced` (@sec-invocation), the block producers MUST exit by themselves, having flushed their log stream. The workbench waits for all of them to exit. If they haven't 40 s after the end the profile plans for, it terminates the run.],
  [Stopping], [Otherwise, and for all other processes, the workbench stops the node with SIGTERM. The node MUST exit within 10 s, having flushed its log stream; otherwise it's killed with SIGKILL.],
  [Exit code], [0 when exiting because of a shutdown option. The workbench records it, but doesn't evaluate it at present.],
  [stdout], [The event stream (@sec-observability).],
  [stderr], [Free; collected, but not analysed.],
)

// ----- 8 -------------------------------------------------------------

= Observability <sec-observability>

The node MUST write its events to stdout. The analysis consumes the log stream specified in _Cardano node performance observability_: JSON lines, at least of conformance tier 1. The node can provide it in one of two ways:

- *Directly:* its stdout is that stream.
- *Translated:* its stdout is in the implementation's own format, and a translation into that stream is available: either defined (a documented mapping), or implemented as a tool that's part of the flake (@sec-delivery). The specification allows this, and sets the rules for it: the translation MUST NOT change when an event happened (`at` stays the time taken at the emission point), and MUST NOT drop or merge events.

The workbench collects `node-<i>/stdout` after the run, and translates it where needed before analysing. The start script itself writes the command line to stdout before starting the node, so the stream begins with lines that aren't events; the specification allows this, and a translation MUST skip them.

- `host` in every event is the value of `TraceOptionNodeName` (@sec-config).
- The node MUST NOT forward events elsewhere in a way that affects its performance during a run (e.g. to a remote collector).
- Tier R fields of `Resources` (garbage collector metrics) are optional. Where an implementation with a garbage-collected runtime provides them, they're only compared with runs on the same runtime, never across runtimes.

`tools/validate.py` in this directory checks a stream against the specification's schemata, directly or after translation.

// ----- 9 -------------------------------------------------------------

= Workload <sec-workload>

The transaction generator runs in two phases:

+ *Setup*, through the local socket of one node (@sec-n2c): it queries the current era and the protocol parameters, then submits transactions that spend the genesis funds, create collateral, and split the funds into the outputs the benchmark spends. Between these steps, it pauses for a few seconds.
+ *Benchmark*: it submits transactions at a fixed rate, over node-to-node transaction submission, to all nodes of the cluster.

There are two workloads, never compared with each other:

- *value*: plain transfers.
- *plutus*: transactions that spend from a Plutus script. The script is a loop whose iteration count is chosen so that the transaction uses as much of the per-transaction execution budget as possible.

== Plutus

For the plutus workload, the node needs a Plutus evaluator, and its accounting of execution units MUST match the Plutus Core specification exactly, for the cost models in the protocol parameters.

The reason: the generator runs each script itself, with the Haskell evaluator and the cost models it queried from the node, and declares the resulting execution units in the transaction, close to the maximum allowed. If the node's evaluator counted more, the script would fail phase-2 validation, and the transaction would still be included but consume its collateral. The workload would then silently differ from the reference run.

How fast the evaluator runs is up to the implementation: the execution units are a deterministic function of the script, its arguments and the cost model, not of the time it takes. The plutus workload measures exactly that difference in speed, under an identical budget.

== Comparability <sec-comparability>

The analysis compares only blocks that pass its chain filter, which requires a minimum fill level. Under the workload's submission pressure, a node is expected to fill every block it forges: by size under the value workload, by execution budget under the plutus workload. How it selects transactions from its mempool is up to the implementation.

An implementation whose blocks pass the filter less often yields fewer samples. Its metrics then carry less statistical confidence, and comparisons with the reference are less conclusive, though they stay fair: they compare only blocks of matching fill.

// ----- 10 ------------------------------------------------------------

= Healthcheck <sec-healthcheck>

After the nodes have started, the workbench checks the cluster once:

+ *Connectivity:* for every node, `cardano-cli ping` to each of its local roots (node-to-node handshake and keep-alive).
+ *Sync:* for every node, `cardano-cli conway query tip --socket-path node-<i>/node.socket` until it reports a block number ≥ 0 and a slot > 0, within 3 minutes. It also checks that the directory given by `--database-path` exists.

A conforming node passes both checks, but they're treated differently:

- *Connectivity is strict.* A node MUST answer the initial ping; if it doesn't, the run is aborted.
- *Sync is lenient.* A failed sync check is recorded as the node not conforming at that point, but it doesn't abort the run.

Apart from the directory check, the healthcheck only inspects the node through these protocols, never its database.

// ----- 11 ------------------------------------------------------------

= Changes on the workbench side <sec-wb-changes>

For transparency, what the workbench changes to run a node other than the Haskell one:

- *Selecting the implementation:* the node's executable comes from the flake input instead of the cardano-node build.
- *Transaction generator:* it proposes all node-to-node versions from `NodeToNodeV_14` to `NodeToNodeV_16`, instead of only V16 (@sec-protocols).
- *Runtime options:* options for the Haskell runtime (`+RTS … -RTS`: parallelism, statistics files, profiling, heap limit) are no longer passed.
- *Run metadata:* the run's manifest records the flake input's revision. Fields that describe the Haskell node's components are set to "unknown".
- *Healthcheck:* it no longer records the Haskell runtime's settings, matches events by namespace (`ns`) only, and doesn't abort the run when the sync check fails (@sec-healthcheck).
- *Analysis:* `locli` parses `Resources` events without the optional fields (`FsRd`, `FsWr`, `NetRd`, `NetWr`, `CentiBlkIO` and tier R), as the specification allows.

// ----- 12 ------------------------------------------------------------

= Open points <sec-open>

+ *Node-to-client version:* the exact versions the reference `cardano-cli` and `cardano-api` propose, to be pinned down.
+ *Warm-up:* a JIT-compiled runtime reaches its steady state only after a while. To be agreed whether the analysis should skip an initial period of the run, for both implementations.
+ *Mixed clusters:* comparisons assume each cluster runs one implementation. Clusters that mix implementations would require interoperating with the Haskell node in every respect.
+ *Phasing:* a first milestone with the value workload only, followed by the plutus workload.
+ *On-disk ledger state:* this document assumes the ledger state is kept in memory. Some profiles benchmark the Haskell node's on-disk ledger backend instead. In principle, an implementation would take part the same way: the configuration selects the backend (`LedgerDB.Backend`) and places its data on a dedicated SSD volume (`LSMDatabasePath`), and a cgroup memory limit makes the backend's reads go to disk, not the page cache. An implementation would map these settings to its own on-disk store.
