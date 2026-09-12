# Alarm system concept for `cardano-tracer`

Status: proposal

## Summary

`cardano-tracer` should provide a central alarm service for alarms detected by:

1. `hermod-recon`, running as a separate process;
2. rules evaluated against the timeseries store in `cardano-tracer`; and
3. trace messages received by `cardano-tracer` whose severity is at or above a
   configured threshold.

Both sources publish the same immutable `AlarmEvent` representation. The alarm
service stores accepted events, fans them out to statically configured consumers,
and makes them available to authorised clients through a REST API.

An alarm is a one-shot event. There is no acknowledgement, ownership, or
active/resolved lifecycle in this proposal.

## Goals

- Give Hermod and timeseries rules one consistent way to raise alarms.
- Decouple alarm detection from alarm delivery.
- Make alarm consumers and their filters configurable.
- Preserve enough evidence to understand why an alarm was raised.
- Prevent a continuously true timeseries rule or a flood of repeated trace
  messages from producing an event on every occurrence.
- Keep the set of delivery mechanisms open until the initial consumers are
  selected.

## Non-goals

- Incident management, acknowledgement, assignment, or resolution.
- Running `hermod-recon` inside the `cardano-tracer` process.
- Defining a new rule language. Hermod continues to use its LTL language and
  timeseries rules use the existing `cardano-timeseries-io` query language.
- Letting remote clients create arbitrary timeseries rules through the API.

## Terminology

- **Producer**: a component that detects a condition and submits an alarm.
- **Rule**: configured detection logic, identified by a stable `ruleId`.
- **Alarm event**: the immutable record produced by a rule occurrence.
- **Consumer**: a statically configured destination to which the service pushes
  matching alarm events.

## Architecture

```text
 node trace files ---> hermod-recon ---- HTTP POST ----+
                                                     |
 node metrics ---> timeseries store ---> rule engine +--> alarm service
                                                     |      |
 node trace messages ---> trace severity rules ------+      +--> alarm store
                                                     |      +--> configured consumers
                                                     |      `--> REST history
                                                     |
                                                     `--> common AlarmEvent
```

The alarm service is a component of `cardano-tracer`, but should be kept separate
from the existing legacy email-notification code. The latter groups trace messages
by severity and has a different data model. Its email implementation may later be
reused by an email alarm consumer.

### Internal components

1. **Ingress API** validates, authenticates, normalises, and deduplicates alarms
   submitted by external producers such as Hermod.
2. **Timeseries rule evaluator** periodically executes configured queries using
   the existing in-process `TimeseriesHandle`.
3. **Trace severity matcher** checks every trace message received from a node
   against the configured trace severity rules.
4. **Alarm store** assigns an event ID and retains accepted events for history.
5. **Dispatcher** matches accepted events against configured consumers and hands
   them to consumer-specific workers.
6. **History API** serves filtered history to authorised clients.

An event is visible to consumers and readers only after it has been accepted
by the alarm store.

## Alarm event

All producers are normalised to a versioned envelope. A representative JSON
event is:

```json
{
  "schemaVersion": 1,
  "eventId": "0198b80a-cad7-7b2d-95cb-37ee0dd3ee81",
  "sourceEventId": "hermod-mainnet-42-1731529958123",
  "raisedAt": "2026-07-13T12:34:56.123Z",
  "receivedAt": "2026-07-13T12:34:56.184Z",
  "source": "hermod-recon",
  "ruleId": "chain-growth-42",
  "severity": "critical",
  "summary": "Chain did not grow within the expected interval",
  "scope": {
    "network": "mainnet",
    "nodeId": "relay-1"
  },
  "labels": {
    "team": "node-operations",
    "site": "eu-central"
  },
  "details": {
    "formulaIndex": 42,
    "formula": "...",
    "relevance": []
  }
}
```

Required producer fields are `sourceEventId`, `raisedAt`, `ruleId`, `severity`,
and `summary`. `cardano-tracer` supplies `eventId`, `receivedAt`, and the trusted
`source` associated with the producer credential. It must not trust a caller to
choose its own source identity.

`severity` should use the existing Cardano severity vocabulary:
`debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, and
`emergency`. Alarm configurations should normally use `warning` or higher.

`scope` contains well-known routing dimensions. `labels` is an extensible map for
consumer filtering. `details` is source-specific JSON and must have configured
size and nesting limits.

The pair `(source, sourceEventId)` is the idempotency key. Re-submitting the same
event returns the already assigned `eventId` and does not dispatch it twice.

## Producers

### Hermod ReCon

`hermod-recon` remains a separate process. On a `FormulaNegativeOutcome`, it
submits one alarm to the ingress API. The mapping is:

| Hermod value | Alarm field |
| --- | --- |
| configured rule name, or formula index as fallback | `ruleId` |
| formula index | `details.formulaIndex` |
| formula | `details.formula` |
| relevant trace events | `details.relevance` |
| configured rule severity | `severity` |
| configured rule summary | `summary` |

Hermod should use a structured alarm output/sink. Human-readable output and the
current `--grep` output are not a stable integration contract. As an incremental
migration, a small adapter process may translate Hermod's machine-formatted
`FormulaNegativeOutcome` trace into the ingress request, but the preferred result
is a native Hermod HTTP alarm sink.

The Hermod worker owns retry with exponential backoff for transient failures.
It uses the same `sourceEventId` on every retry so retries are safe. A permanent
validation or authorisation error is logged locally and is not retried forever.

### Timeseries rules

Timeseries rules are configured in `cardano-tracer` and evaluated in process.
A rule consists of:

- a stable identifier, summary, severity, labels, and optional node scope;
- an existing timeseries query that evaluates to a boolean vector;
- an evaluation interval;
- an optional `for` duration for which the expression must remain true; and
- an optional repeat interval.

For every output series, the evaluator maintains a small state machine:

```text
false/missing -- true --> pending -- true for configured duration --> publish
     ^                       |                                      |
     `------ false/missing --'---- false/missing <------------------'
```

The default is edge-triggered: one event is published when the rule first meets
its `for` duration. It does not publish a resolution event. After the expression
becomes false or missing, a later false-to-true transition may publish a new
alarm. When `repeatEvery` is configured, a still-true rule may publish reminders
at that interval.

Each output series must generate a distinct deterministic series key from its
labels. The series key is included in `sourceEventId` and in the alarm labels.
Evaluation errors and missing data are traced as health information, not silently
interpreted as alarms. A future rule option may explicitly define missing data as
alarming.

The evaluator must put limits on query execution time, result cardinality, and
concurrent evaluations so alarm rules cannot starve normal timeseries ingestion
and queries.

### Trace severity rules

`cardano-tracer` already receives every trace message forwarded by the connected
nodes. A trace severity rule raises an alarm when a received message's severity
is at or above a configured threshold. A rule consists of:

- a stable `ruleId` and an optional summary;
- a `threshold` severity (using the same lowercase severity vocabulary); and
- an optional `suppressForSecs` window (default 300 seconds) and optional labels.

The mapping from a matching trace message to the alarm event is:

| Trace message value | Alarm field |
| --- | --- |
| message severity | `severity` |
| message timestamp | `raisedAt` |
| node name | `scope.nodeId` |
| namespace (dot-joined) | `labels.namespace` |
| machine-readable message, hostname, thread ID | `details` |

The trusted `source` is the constant `trace`: like the future timeseries rules,
this is an internal producer that never passes HTTP authentication, so its
identity is fixed rather than derived from a credential.

Flood prevention reuses the idempotency key instead of a separate rate limiter:
the `sourceEventId` is `ruleId:node:namespace:windowIndex`, where the window
index is the message timestamp divided by `suppressForSecs`. All matches from
the same node and namespace within one window therefore share the
`(source, sourceEventId)` key and collapse into a single alarm; a match in the
next window raises a fresh one.

## Consumer model

A consumer implementation has the following conceptual interface:

```text
initialise(config) -> worker
matches(filter, AlarmEvent) -> Bool
deliver(worker, AlarmEvent) -> delivered | retryable-error | permanent-error
shutdown(worker)
```

Each configured consumer has:

- a unique name and consumer type;
- type-specific destination settings and credentials;
- a filter;
- queue capacity, retry policy, and timeout; and
- an enabled flag.

Filters can select `source`, `ruleId`, minimum severity, scope fields, and labels.
The first implementation should support conjunction only. More complex boolean
filter expressions can be added later without changing `AlarmEvent`.

Consumer types are intentionally not fixed by this concept. Likely initial
implementations are `webhook`, `email`, and `log`. A webhook is a useful generic
first delivery mechanism because downstream systems can adapt it to chat or
incident-management products.

Consumer workers use bounded, independent queues. A slow or broken destination
must not block alarm ingestion or other consumers. Delivery is at least once for
consumers that enable retries; a destination should therefore deduplicate using
`eventId`. Exhausted delivery attempts are retained as operational failures and
traced by `cardano-tracer`. They do not create alarm events recursively.

## API

The alarm API should use a separately configurable endpoint, even if its server
implementation later shares code with the timeseries server.

### Producer ingress

```text
POST /alarms/v1/events
Authorization: Bearer <producer token>
Content-Type: application/json
```

- `201 Created` for a newly accepted event;
- `200 OK` for an idempotent replay;
- `400 Bad Request` for invalid input;
- `401 Unauthorized` or `403 Forbidden` for rejected credentials; and
- `429 Too Many Requests` when producer limits are exceeded.

The response contains `eventId`, `receivedAt`, and whether the event was newly
created.

### History

```text
GET /alarms/v1/events?after=<cursor>&limit=100&source=hermod-recon&minSeverity=warning
Authorization: Bearer <reader token>
```

Results are ordered by the store cursor and use cursor pagination. Supported
filters mirror the simple consumer filters. Retention is configurable.

## Configuration sketch

The precise Haskell representation can be refined during implementation. This
YAML illustrates the intended operator-facing configuration:

```yaml
alarms:
  endpoint:
    epHost: "127.0.0.1"
    epPort: 3210
    epForceSSL: true
  retention:
    maxAge: 7d
    maxEvents: 100000
  limits:
    maxEventBytes: 262144
    ingressQueue: 1000
  authentication:
    producers:
      - name: hermod-mainnet
        tokenFile: "/run/secrets/hermod-alarm-token"
        source: hermod-recon
    readers:
      - name: operations-dashboard
        tokenFile: "/run/secrets/dashboard-alarm-token"
        allowHistory: true
        filter:
          minSeverity: warning
          labels:
            team: node-operations
  timeseriesRules:
    - ruleId: high-ping-latency
      summary: "Average node ping latency is above 500 ms"
      severity: warning
      query: "avg_over_time (netdata_statsd_cardano_node_ping_latency_ms_gauge_value_average[now - 5m; now]) > 500"
      evaluateEvery: 30s
      for: 2m
      labels:
        team: node-operations
  traceRules:
    - ruleId: error-traces
      summary: "A trace message with severity error or above was received"
      threshold: error
      suppressForSecs: 300
      labels:
        team: node-operations
  consumers:
    - name: alarm-audit-log
      type: log
      enabled: true
      filter:
        minSeverity: warning
```

Secrets should be read from protected files or an equivalent secret provider,
not written directly into the main configuration. TLS uses the existing
`tlsCertificate` configuration initially. Deployments that terminate TLS at a
trusted reverse proxy must restrict direct access to the alarm listener.

## Storage and delivery guarantees

The store is append-only from the alarm domain's perspective because alarm events
are immutable. It must support:

- atomic insert with idempotency-key uniqueness;
- cursor-ordered reads and filtered history;
- retention by age and maximum count; and
- recovery across `cardano-tracer` restarts.

The concrete backend should sit behind a small `AlarmStore` interface. An embedded
transactional database is preferable to ad-hoc JSON files once retry state and
cursor pagination are implemented. Storage failure rejects ingress rather than
publishing an event that cannot subsequently be replayed.

The core guarantee is: after the service reports an event as accepted, the event
is durable until retention removes it. Configured consumers receive at-least-once
delivery when retries are enabled.

## Security

- Deny producer and history access by default.
- Give producers, readers, and administrators distinct credentials and roles.
- Derive the producer source from credentials.
- Apply per-credential request and connection limits.
- Bound event size, label count, label lengths, JSON depth, and query result
  cardinality.
- Do not include credentials in traces or alarm details.
- Treat Hermod relevance evidence as potentially sensitive operational data;
  consumer and reader filters are an authorisation boundary.
- Refuse to start an externally reachable clear-text endpoint unless an explicit
  insecure setting is enabled.

The first version does not require a remote administration API. Rules, consumers,
and credentials are managed through configuration and take effect on restart.
Hot reload can be considered separately.

## Observability

`cardano-tracer` should expose metrics and structured traces for:

- accepted, rejected, duplicate, and rate-limited ingress events;
- timeseries rule evaluations, evaluation failures, and evaluation duration;
- alarms published by source, rule, and severity, with bounded label cardinality;
- per-consumer queue depth, attempts, successes, retries, permanent failures,
  and drops; and
- alarm-store size, retention removals, and failures.

Alarm infrastructure failures must not create alarms through the same pipeline,
which could cause a feedback loop. They are emitted as normal internal traces and
metrics.

## Failure behaviour

- If Hermod cannot reach `cardano-tracer`, Hermod retries with its stable
  idempotency key and bounded local buffering.
- If the store is unavailable, external ingress fails and timeseries alarm
  publication is retried without advancing the rule's published state.
- If one consumer is unavailable, its worker retries independently while other
  consumers continue.
- If a timeseries query fails, the rule reports an evaluation failure and keeps
  its previous state until a configured stale-state timeout; it does not publish
  a false alarm.
- If `cardano-tracer` restarts, persisted rule state avoids duplicate alarm edges.
  When state is unavailable, the evaluator observes one complete `for` duration
  before publishing.

## Suggested implementation phases

### Phase 1: alarm core and Hermod ingress

- Introduce `AlarmEvent`, validation, store, and idempotent ingress.
- Add the authenticated history endpoint.
- Add a `log` consumer to exercise filtering and dispatch.
- Add either the native Hermod HTTP sink or the temporary machine-trace adapter.

### Phase 2: timeseries evaluation

- Add configuration and validation for timeseries rules.
- Implement edge/`for` state and persistence.
- Add evaluator safety limits and metrics.

### Phase 3: production consumers

- Select and implement the required consumer types.
- Add independent retry queues and persisted delivery attempts where required.
- Add operational dashboards and retention tooling.

## Possible future features

The features in this section are not part of the current design. They are
recorded here so a later revision can pick them up without redesigning the
core.

### Live subscription (SSE)

A **subscriber** is an authorised API client that receives a live stream of
alarm events. Subscribers are deliberately different from consumers: consumers
are controlled by the operator's configuration and can implement reliable
delivery, while subscribers are dynamic clients and receive a best-effort live
stream. Guaranteeing delivery to a live client while it is disconnected is
explicitly out of scope.

```text
GET /alarms/v1/stream?source=timeseries&minSeverity=warning
Authorization: Bearer <subscriber token>
Accept: text/event-stream
Last-Event-ID: <event ID or cursor>
```

Server-Sent Events (SSE) is recommended over WebSockets because the data flow
is one-way, it works with ordinary HTTP infrastructure, and it has a standard
reconnection model. Events use `event: alarm`, an event ID/cursor, and the JSON
`AlarmEvent` as data. Periodic comments act as heartbeats.

Subscription is disabled unless `allowSubscriptions` is true. A reader
credential gains an `allowSubscription` flag; its allowed filter acts as a
ceiling that a requested filter can only narrow, never broaden. If retained
history contains the `Last-Event-ID`, the server replays later events before
switching to live delivery. Otherwise it reports that the cursor is outside
retention and the client must resume from the oldest available cursor. The
alarm store's cursor doubles as the SSE resumption cursor, so history and
stream resumption share the same pagination mechanism.

Each subscriber gets a bounded queue (a `subscriberQueue` size under `limits`
in the configuration). SSE delivery is best effort: if a client is slow and
its queue fills, only that client is disconnected, and it reconnects using its
last processed event ID. Subscription access is denied by default, and the SSE
queue size joins the list of bounded resources.

Observability for this feature adds connected SSE subscribers and slow-client
disconnects to the exposed metrics and traces.

## Decisions still open

The following choices do not block the architecture:

1. Which push consumers are required first (`webhook`, `email`, a message broker,
   or another integration).
2. Which embedded storage backend should implement `AlarmStore`.
3. Whether Hermod Recon gains a native HTTP alarm sink immediately or initially uses an
   adapter.
4. Whether configuration hot reload is needed.
5. Whether consumer retry queues must survive process restart in the first
   release.

