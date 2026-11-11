// The internals of locli: how the analysis works, for contributors.
// Maintained by hand, next to the code it describes; update it with the code.
//
// Build:  typst compile locli-internals.typ
// (the PDF isn't tracked; diagrams use `fletcher` from Typst Universe)

#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge

#set document(title: "The internals of locli", author: "Cardano Performance & Tracing")
#set page(paper: "a4", margin: 2cm, numbering: "1")
#set text(size: 10pt)
#set par(justify: true, spacing: 0.9em)
#set heading(numbering: "1.1")
#show heading.where(level: 1): set block(above: 1.6em, below: 1em)
#show raw.where(block: false): set text(size: 0.95em)
#show link: underline

// Diagram vocabulary: commands / stages, data, external inputs and outputs.
#let stage(..args) = node(..args, fill: rgb("#dbe8f7"), stroke: 0.6pt + rgb("#5577aa"), corner-radius: 3pt)
#let datum(..args) = node(..args, fill: rgb("#e3f2d9"), stroke: 0.6pt + rgb("#6a9a4a"))
#let ext(..args) = node(..args, fill: rgb("#fdf1cf"), stroke: 0.6pt + rgb("#b8952c"), corner-radius: 6pt)
#let fig(d, caption) = figure(d, caption: caption, gap: 0.8em)

#align(center)[
  #text(size: 20pt, weight: "bold")[The internals of locli]
  #v(0.2em)
  #text(size: 11pt)[locli 3.0 · Cardano Performance & Tracing]
]

#v(1em)
#outline(depth: 2)

= About this document

`locli` analyses the log output of a benchmarking cluster: it reconstructs what each node did, slot by slot and block by block, condenses that into statistical distributions, and renders them as data (JSON, gnuplot CDFs, text tables) and as Typst reports. The workbench (`nix/workbench/analyse/analyse.sh`) drives it.

This document is for contributors to `locli`. Sections 1 and 2 give the overall picture, which is also useful to readers of the reports; the rest goes stage by stage, naming modules, key functions, and the invariants that keep the results correct. Where a design decision isn't obvious from the code, its reason is given.

Conventions in the diagrams: #box(fill: rgb("#dbe8f7"), inset: 2pt)[stages] (locli commands or functions), #box(fill: rgb("#e3f2d9"), inset: 2pt)[data], #box(fill: rgb("#fdf1cf"), inset: 2pt)[external inputs and outputs].

= The pipeline at a glance

A `locli` invocation runs a _script_: a sequence of commands, each reading and extending a shared state (`Cardano.Command`, `runChainCommand` over `State`). For example, `unlog-db` fills the state with every node's log objects, `rebuild-chain` adds the chain, `compute-propagation` the block propagation results. The workbench composes these scripts (@sec-workbench); a command whose input isn't in the state yet fails with a list of what's missing.

#fig(diagram(
  spacing: (10mm, 7mm),
  node-inset: 5pt,
  ext((0, 0), [node logs \ (JSON lines)]),
  stage((0, 1), [`prepare-db` \ per node]),
  datum((0, 2), [SQLite DB \ per node]),
  stage((0, 3), [`unlog-db`]),
  datum((0, 4), [log objects \ per node]),
  ext((1.3, 4), [`meta.json`, genesis, \ chain filters]),

  stage((-1.4, 5), [`collect-slots`, \ `filter-slots`]),
  datum((-1.4, 6), [slot stats \ per node]),
  stage((-1.4, 7), [`compute-machperf`, \ `compute-clusterperf`]),
  datum((-1.4, 8), [cluster performance]),

  stage((1.4, 5), [`build-mach-views`, \ `rebuild-chain`]),
  datum((1.4, 6), [chain \ (block events)]),
  stage((1.4, 7), [`compute-propagation`]),
  datum((1.4, 8), [block propagation]),

  stage((0, 9), [`compute-summary`]),
  datum((0, 10), [summary]),
  stage((0, 11), [`render-*`, `report`, `compare`]),
  ext((0, 12), [JSON, gnuplot CDFs, text tables, \ Typst tables and reports]),

  edge((0, 0), (0, 1), "-|>"),
  edge((0, 1), (0, 2), "-|>"),
  edge((0, 2), (0, 3), "-|>"),
  edge((0, 3), (0, 4), "-|>"),
  edge((0, 4), (-1.4, 5), "-|>"),
  edge((0, 4), (1.4, 5), "-|>"),
  edge((1.3, 4), (1.4, 5), "-|>", stroke: (dash: "dashed")),
  edge((1.3, 4), (-1.4, 5), "-|>", stroke: (dash: "dashed")),
  edge((-1.4, 5), (-1.4, 6), "-|>"),
  edge((-1.4, 6), (-1.4, 7), "-|>"),
  edge((-1.4, 7), (-1.4, 8), "-|>"),
  edge((1.4, 5), (1.4, 6), "-|>"),
  edge((1.4, 6), (1.4, 7), "-|>"),
  edge((1.4, 7), (1.4, 8), "-|>"),
  edge((-1.4, 8), (0, 9), "-|>"),
  edge((1.4, 8), (0, 9), "-|>"),
  edge((0, 4), (0, 9), "-|>", stroke: (dash: "dotted")),
  edge((0, 9), (0, 10), "-|>"),
  edge((0, 10), (0, 11), "-|>"),
  edge((-1.4, 8), (0, 11), "-|>", bend: -25deg),
  edge((1.4, 8), (0, 11), "-|>", bend: 25deg),
  edge((0, 11), (0, 12), "-|>"),
), [The single-run pipeline. Multi-run analyses (variance, @sec-cdf) start from the stored results instead.])

Two analyses run side by side on the same log objects:
- the *machine timeline*: per node, one record per slot (`SlotStats`), from which resource usage, forge loop timings, ledger snapshot and transaction processing metrics are derived;
- *block propagation*: per block, the events of its forger and every observer, from which forging and propagation delays are derived.

The summary ties both together with run metadata. Every result is a set of cumulative distributions (CDFs, @sec-cdf), stored as JSON in the run's `analysis/` directory; reports are rendered from those files, so they can be re-rendered without parsing logs again (@sec-reports).

= Ingesting logs <sec-ingest>

== From log lines to log objects

`Cardano.Unlog.LogObject` turns a JSON log line into a `LogObject`: timestamp, namespace, host, thread, and an interpreted body, `LOBody`. The `interpreters` map selects the parser by namespace; lines of other namespaces become `LOAny` and are dropped before analysis. Required payload fields use `.:`, optional ones `.:?`; a line that doesn't parse becomes `LODecodeError`.

Node namespaces and payload shapes change between node versions, often without notice. Every interpreter is therefore backed by a real log line in the test fixture (`prop_interpreters_covered_by_fixture`), and an interpreter written from example traces counts as unverified until checked against a real log. Compatibility covers log formats of about the last year; older formats are dropped with a major version.

The `Resources` sample follows the performance observability specification: only CPU time, RSS and the thread count are required. Other fields (e.g. GC fields, which a runtime without GC doesn't report) read as 0 when absent; the reports tell an unreported 0 from a measured one by plausibility (@sec-reports).

== The database backend

`prepare-db` (`Cardano.Unlog.BackendDB.prepareDB`) parses a node's log files once and stores the persistable log objects in a per-node SQLite database (`Cardano.Unlog.LogObjectDB`): tables for events, transactions, resource samples (the full sample as a JSON blob, a few fields as columns for ad-hoc queries), leadership checks and ledger metrics, plus a summary table (log line count, first and last timestamp) and the trace frequencies. `unlog-db` loads every node's database concurrently (`runLiftLogObjectsDB`, `loadHostLogsDB`) and converts the rows back into log objects.

Databases written by older versions must stay readable. Rows are read through per-constructor converters (`toLOBodyConverters`); an unknown row shape is an error, while missing optional content stays lenient.

It is the only backend: the file backend, which parsed the JSON logs on every analysis, was removed in 3.0.

Incremental analysis (consuming the logs of running nodes) isn't built, and would need a redesign of the analysis passes, which recompute from complete log object lists. Its ingestion side would build on this backend: `prepareFile` already decodes and stores one line at a time; the databases would be opened in append mode and fed in chunks, each chunk committed on its own; and a reader can fetch only new rows by each table's implicit `rowid`, which grows with log order, without an index (indexes are avoided on purpose, as every run's databases are archived).

== Values taken at load time

On a 52-node cluster run, the analysis holds about 94 million log objects, more than 64 GiB, partly in swap. Every extra pass over them pages them back in. So values that need the whole stream per node come from the database, while the session is open, and travel in `HostLogs` as `Maybe` fields:

#table(
  columns: (auto, 1fr),
  table.header[Field][Source],
  [`hlRawFirstAt`, `hlRawLastAt`, `hlRawLines`], [the database's summary table],
  [`hlRawLastUTxOSize`], [the latest `LedgerMetrics` row (`getLastUTxOSize`)],
  [`hlLoadedCount`], [the number of rows loaded, counted (and forced) right after the query],
)

`computeSummary` falls back to a pass over the log objects only when a field is `Nothing`, defensively. Values computed at load time must be forced there: an unevaluated `length rows` would keep the node's entire row list alive until the summary is computed.

= The machine timeline <sec-machperf>

#fig(diagram(
  spacing: (9mm, 6mm),
  node-inset: 5pt,
  datum((0, 0), [log objects \ (one node)]),
  stage((0, 1), [`timelineStep` \ (fold)]),
  datum((0, 2), [`SlotStats` \ per slot]),
  stage((0, 3), [`runSlotFilters`]),
  ext((1.5, 3), [slot conditions \ of the chain filters]),
  stage((0, 4), [`slotStatsMachPerf`]),
  datum((0, 5), [`MachPerfOne` \ (per-node CDFs)]),
  stage((0, 6), [`summariseClusterPerf`]),
  datum((0, 7), [`ClusterPerf`]),
  edge((0, 0), (0, 1), "-|>"),
  edge((0, 1), (0, 2), "-|>"),
  edge((0, 2), (0, 3), "-|>"),
  edge((1.5, 3), (0, 3), "-|>", stroke: (dash: "dashed")),
  edge((0, 3), (0, 4), "-|>"),
  edge((0, 4), (0, 5), "-|>"),
  edge((0, 5), (0, 6), "-|>"),
  edge((0, 6), (0, 7), "-|>"),
), [From one node's log objects to the cluster's performance (`Cardano.Analysis.MachPerf`).])

*Timeline* (`collect-slots`). `timelineFromLogObjects` folds a node's log objects (`timelineStep`) into one `SlotStats` record per slot: forge loop timestamps, leadership and forge counts, the chain's block number, mempool and transaction counts, ledger snapshots, UTxO size, and the resource sample of that slot. Slots without events are patched in (`patchSlotGap`), carrying over the state that persists (e.g. the mempool fill level, the block number). Resource samples are cumulative counters; accumulators (`Data.Accum`, `Cardano.Unlog.Resources`) turn them into per-slot values: differences, CPU shares in percent of a core, or the latest value.

*Filtering* (`filter-slots`). `runSlotFilters` keeps the slots that satisfy the slot conditions of the chain filters, e.g. `epoch3+`. Block conditions (block size, adoption) don't apply to slots; they act on the chain (@sec-blockprop). Filtering leaves gaps in a node's slot list where slots were removed. `timeline-slots` writes the filtered timeline per node as text (`logs-node-*.mach.txt`), for inspection.

*Per-node results* (`compute-machperf`). `deltifySlotStats` turns the forge loop timestamps into durations; `slotStatsMachPerf` then condenses a node's slots into `MachPerfOne`, a record of CDFs, one per metric. Notable derivations:

- *Ledger snapshots.* Snapshots taken and their write durations come from `TookSnapshot` completion events. Each snapshot defines a write window, from its start until 5 s after completion, to also cover the major GC that usually follows. "Live data after major GC" is split by whether the sample falls into such a window.
- *Transaction processing.* Run totals per node: processed (collected), accepted, rejected (processed minus accepted, since the node's own per-batch rejected count isn't reliable), the reject ratio, and CPU, GC and mutator time over the analysed slots divided by the processed transactions. The mempool fill level is the last mempool count of each slot, carried forward, so every slot weighs the same.
- *CPU 85% peaks.* Maximal runs of consecutive slots (a filter gap ends a run) at 85% CPU or more, their length in slots, each attributed to one cause: the *epoch transition* (the peak ends at most 3 slots before the epoch's first block is adopted, where the ledger computes the transition; counted from the epoch's first slot), a *ledger snapshot* (it overlaps a write window), or *other*.

Some CDFs are kept in the JSON on purpose without being reported (slots analysed per node, leadership wins per slot); the metric definitions test lists them.

*Cluster results* (`compute-clusterperf`). `summariseClusterPerf` combines the nodes' CDFs into one `ClusterPerf` (@sec-cdf).

= Block propagation <sec-blockprop>

#fig(diagram(
  spacing: (9mm, 6mm),
  node-inset: 5pt,
  datum((0, 0), [log objects \ (every node)]),
  stage((0, 1), [`buildMachViews`]),
  datum((0, 2), [per-node block events \ (`MachView`)]),
  stage((0, 3), [`rebuildChain`]),
  ext((1.5, 3), [block conditions \ of the chain filters]),
  datum((0, 4), [chain (`BlockEvents`), \ rejected blocks]),
  stage((0, 5), [`blockProp`]),
  datum((0, 6), [`BlockPropOne`]),
  edge((0, 0), (0, 1), "-|>"),
  edge((0, 1), (0, 2), "-|>"),
  edge((0, 2), (0, 3), "-|>"),
  edge((1.5, 3), (0, 3), "-|>", stroke: (dash: "dashed")),
  edge((0, 3), (0, 4), "-|>"),
  edge((0, 4), (0, 5), "-|>"),
  edge((0, 5), (0, 6), "-|>"),
), [Block propagation (`Cardano.Analysis.BlockProp`).])

`buildMachViews` folds each node's log objects (`blockPropMachEventsStep`) into a map from block to that node's events for it: as the forger (leadership, ticked ledger state, mempool snapshot, forged, announced, sent, adopted) or as an observer (header seen, fetch requested, fetched, announced, sent, adopted). `rebuildChain` merges these views into the chain: one `BlockEvents` per block, with the forger's events, every observer's events with their delays, and the result of each chain filter for the block. Blocks failing a filter are kept aside as rejected, and counted in the summary.

`blockProp` derives the CDFs over the chain's accepted blocks: the forger's loop phases, each observer's notice, fetch and adoption delays, and the end-to-end propagation, i.e. when a given fraction of the cluster has adopted the block. A forged block that lacks one of the forger's events is an analysis error that names block, forger and the missing event.

*Battles.* The chain holds only the winning blocks; `rebuildChain` finds each chain block's rivals among all blocks any node has seen or forged: blocks forged for the same slot, at any height (a slot battle: several leaders in one slot), and blocks at the same height forged for another slot (a height battle: a forger that hadn't yet seen its predecessor). Their counts are per block (`beSlotBattle`, `beHeightBattle`). Their CDFs (Anomaly control) cover the chain blocks that pass the slot conditions only: block conditions would bias them, as a contested block is often adopted later, by fewer nodes, or through a chain switch (failing `unitary`).

= The summary

`computeSummary` (`Cardano.Analysis.Summary`) collects what describes the run as a whole: metadata and manifest (`meta.json`), genesis parameters, workload, the filters applied, the slot and block domains before and after filtering, the nodes' start and stop spread, log object counts, the chain's rejected blocks, the final UTxO size (each node's last `LedgerMetrics`, averaged over nodes), and profiling data if the nodes were profiled. It reads the per-node values loaded with the logs (@sec-ingest), never the log objects themselves.

= Distributions <sec-cdf>

`Data.CDF` defines `CDF p a`: a distribution of values of type `a`, given by its size, average, median, standard deviation, range, and samples at a fixed list of centiles. The parameter `p` says what each sample is:

- `CDF I a`: plain values. Per-node results are of this type, sampled at `stdCentiles`.
- `CDF (CDF I) a`: a distribution of distributions. Cluster results combine one CDF per node; multi-run results combine one cluster result per run.

#fig(diagram(
  spacing: (12mm, 6mm),
  node-inset: 5pt,
  datum((0, 0), [per-node CDFs \ `CDF I a`]),
  stage((1, 0), [`cdf2OfCDFs`]),
  datum((2, 0), [cluster CDF \ `CDF (CDF I) a`]),
  stage((3, 0), [`cdf2OfCDFs`]),
  datum((4, 0), [multi-run CDF]),
  edge((0, 0), (1, 0), "-|>"),
  edge((1, 0), (2, 0), "-|>"),
  edge((2, 0), (3, 0), "-|>"),
  edge((3, 0), (4, 0), "-|>"),
), [Combining distributions: nodes into a cluster, clusters of several runs into a multi-run result.])

`cdf2OfCDFs` combines CDFs centile by centile; the combined average is a distribution of the inputs' averages, so `cdfAverageVal` of a combined CDF is the mean of its inputs' averages. The combined standard deviation is approximated from the inputs' (the `Combine` in use, `stdCombine1` or `stdCombine2`); only when every input holds a single sample (e.g. per-node run totals) is it the exact standard deviation of those samples. A deliberate limit: the exact pooled value would change the statistics of every metric, and with them the comparability of reports.

Some metrics have no samples on a node, e.g. "Live after major GC outside snapshot writes" when no such GC happened. Combining an empty CDF would pull the cluster's average of averages towards 0, so combination uses only the non-empty inputs (`nonEmptyCDFs`); if all are empty, one of them stands for all.

Reports read combined results in one of two aspects (`CDF2Aspect`): `--overall` treats all samples as one population, `--inter-cdf` (the workbench's default for multi-run results) describes the spread between the inputs.

*Reading older results.* A `clusterperf.json`, `blockprop.json` or `summary.json` written by an older `locli` lacks the CDFs added since. Decoding is lenient towards missing CDFs (`withEmptyCDFs`): they read as empty, and reports show them as not available (`—`), so older runs can still be compared against and combined with newer ones. Anything else missing still fails.

= Metric definitions

Every reported metric is a `Field` (`Cardano.Analysis.API.Field`), defined in `Cardano.Analysis.API.Metrics`: an identifier, column headers for the text tables, width, unit, precision, scale, a selector into the result record, a short description and a long one. The fields drive every renderer and the data dictionary in the reports' appendix. Which fields a report section shows is a list of identifiers per section (`sumFieldsReport`, `mtFieldsReport`, `mtFieldsSnapshots`, `bpFieldsForger`, …); a test checks that every listed identifier is defined, and that every CDF of the per-node results has a definition, apart from the deliberately unreported ones.

Two lists change how values render:
- `fieldsWarnIfZero`: a 0 most likely means a problem (e.g. trace parsing), and is highlighted (snapshots taken).
- `fieldsZeroIsUnreported`: a value that can't be zero throughout if measured, e.g. network and storage I/O, or the GC fields of a managed runtime. Zero throughout means the node didn't report it, and it renders as not available.

Plain-text outputs stay ASCII, since they're read over SSH on the benchmarking machines; Unicode is fine in the Typst reports.

= Rendering and reports <sec-reports>

#fig(diagram(
  spacing: (10mm, 6mm),
  node-inset: 5pt,
  datum((0, 0), [results (JSON)]),
  stage((-1.3, 1), [`render-*` \ `--typst`]),
  datum((-1.3, 2), [single tables \ `*.typ`]),
  stage((-1.3, 3), [`report`]),
  ext((-1.3, 4), [single-run or \ variance report]),
  stage((1.3, 1), [`compare`]),
  ext((1.3, 4), [comparison report]),
  edge((0, 0), (-1.3, 1), "-|>"),
  edge((-1.3, 1), (-1.3, 2), "-|>"),
  edge((-1.3, 2), (-1.3, 3), "-|>", label: [`#import`], label-side: left),
  edge((-1.3, 3), (-1.3, 4), "-|>"),
  edge((0, 0), (1.3, 1), "-|>"),
  edge((1.3, 1), (1.3, 4), "-|>", label: [values injected], label-side: right),
), [Typst output: importing table files vs injecting values.])

*Text formats* (`Cardano.Render`): JSON dumps, gnuplot CDF files (one per metric, used by the reports' chart appendix), and plain-text tables. `Render` handles text formats only (`TextFormat`); Typst output comes from `Cardano.Report`.

*Typst* (`Cardano.Report`, `Cardano.Typst`, templates in `report-templates/typst/`). Templates are EDE files, rendered with values prepared in Haskell; the document structure and styling live in the templates, while Haskell formats numbers (`renderNumField`) and escapes data (`typstEscape`). A report section (`Section`) pairs a table file name and title with a `SectionTable`, which keeps a result and its field selection together, so a section can produce its own values.

- `render-* --typst` writes one standalone document per table (`single_table.ede`), binding the table as `locli-table`.
- `report` writes the single-run report (`main_single.ede`), which `#import`s the table files next to it; a missing file is noted in the report instead of failing its compilation. For a variance analysis, the same command and template bundle the multi-run results; the input runs are listed in a comment of the report source.
- `compare` writes the comparison report (`main_comparison.ede`): values of all runs side by side, with absolute and relative differences to the first run, injected through the template environment, since no table file holds runs side by side.
- Plutus runs get a workload calibration addendum, from the generator's budget summary.

*Re-render vs re-analysis.* Everything in this section works from the stored results, so a rendering change only needs a re-render (`wb analyse call RUN read-summaries read-clusterperfs read-propagations …`, seconds). Changing how a metric is computed (the per-node folds, the combination) needs a re-analysis (`wb a std`, reusing the databases).

= The workbench <sec-workbench>

`nix/workbench/analyse/analyse.sh` composes `locli` scripts from short op names, substituted into commands with their file arguments. The standard single-run analysis (`wb a std RUN`) runs these ops in this order, plus a few dump helpers:

#table(
  columns: (auto, 1fr),
  table.header[Workbench op][locli command(s)],
  [`logs`], [`unlog-db`],
  [`read-context`], [`read-meta-genesis`],
  [`build-mach-views`], [`build-mach-views`],
  [`rebuild-chain`, `dump-chain`, `chain-timeline`], [`rebuild-chain` (with the chain filters), `dump-chain`, `timeline-chain`],
  [`collect-slots`, `filter-slots`, `timeline-slots`], [the same commands],
  [`compute-propagation`, `propagation-*`], [`compute-propagation`, `render-propagation` (`--json`, `--typst` per section, `--gnuplot`, `--pretty`)],
  [`compute-machperf`, `render-machperf`], [the same commands],
  [`compute-clusterperf`, `clusterperf-*`], [`compute-clusterperf`, `render-clusterperf` (`--json`, `--typst` per section, `--gnuplot`, `--pretty`)],
  [`compute-summary`, `summary-*`], [`compute-summary`, `render-summary` (`--json`, `--typst`)],
  [`report`], [`report --typst analysis/report-RUN.typ`],
)

Further entry points: `wb a prep RUN` (`prepare-db` per node), `wb a variance RUN…` (reads the runs' results, computes and renders the multi-run results into a new run directory, and ends with `report`), `wb a compare RUN…` (`compare` into a new run directory), and `wb a pdf RUN` (compiles the run's `report-*.typ`).

= Testing

`cabal test locli:test:test-locli`, run from `bench/locli`. The suite pins what's easy to break silently:

- *Ingestion*: a round-trip of real log lines through the database, every interpreter backed by a real line, the optional `Resources` fields, the values loaded with the logs.
- *Computation*: CDF combination, the per-node derivations (snapshots and write windows, transaction totals, CPU peaks by cause) on hand-made slots, combining nodes without samples.
- *Compatibility*: reading and combining results as written by `locli` 2.x.
- *Rendering*: every template through the real generators on real analysis results (a 6-node run in `test/data/analysis/`), number formatting and escaping, the plausibility rules.

Tests of computations are checked to fail when the code they guard is broken, e.g. by reverting a fix.
