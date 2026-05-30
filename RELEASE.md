# Team

The release process is owned by **Intersect** (the Cardano MBO), which runs a
weekly Release Office Hours meeting attended by all team leads to give status
updates and plan upcoming releases.

Code contributions and maintenance come from **IOG**, **Tweag**, and
**Ensurable Systems**, which also provide rotating release engineers.

## Sign-offs

| Role | Person |
|------|--------|
| Cardano Product Committee (CPC) | Samuel Leathers |
| Cardano Technical Steering Committee (TSC) | Kevin Hammond |
| Rotating Release Engineer | (varies per release) |
| QA Lead | Martin Kourim |
| SRE Lead | John Lotoski |
| Performance Lead | Michael Karg |

A release may be published as a **GitHub pre-release** with sign-offs from
CPC, TSC, RE, and SRE. At that point QA kicks off the final integration test
suite. The remaining sign-offs (QA, Performance) are required before the
release is promoted to a full release.

## Component Teams

| Component | Responsible Team |
|-----------|-----------------|
| Ledger | Ledger team (IOG) |
| Consensus | Consensus team (IOG) |
| Network | Network team (IOG) |
| API & CLI | Node/API/CLI team (IOG/Tweag/Ensurable Systems) |
| Node | Rotating Release Engineer (no permanent owner) |

# Release Cadence

Releases target a **4–8 week** cycle. Although earlier planning aimed for
2–4 weeks, the active development of new eras (currently Dijkstra) has meant
nearly every release contains substantial changes, pushing the realistic
cadence to 4–8 weeks.

# Release Process

This is the release process for node releases.

1. The weekly Intersect release meeting determines which commits or branches
   to integrate, including any in-flight PRs that need to be included.
2. The Release Engineer works with component dev leads to integrate
   dependencies bottom-up through the stack:
   Ledger → Consensus → Network → API → CLI → Node.
3. Components pass their own property tests, golden tests, unit tests, and
   integration tests before integration proceeds up the stack. Integration
   tests using the `cardano-testnet` package (in this repository) provide the
   primary integration signal at the node level.
4. When the full stack is stable, components are released to CHaP bottom-up
   and the Release Engineer replaces source-repository-package (SRP) stanzas
   with the CHaP releases, working up the stack until the node is ready.
5. SRE deploys the release candidate to preview/preprod networks.
6. The community is notified of the release candidate and given time to
   provide feedback.
7. The Release Engineer creates a draft GitHub Release and asks dev leads to
   fill in their changelog sections.
8. CPC, TSC, Release Engineer, and SRE Lead sign off → the release is
   published as a **GitHub pre-release**.
9. QA runs the final integration test suite (`cardano-node-tests`).
10. Performance testing is run against the final tag
    (see [Performance Testing](#performance-testing)).
11. SRE escalates deployments through all remaining environments up to
    mainnet and monitors for issues. No new SRE sign-off is required —
    their pre-release approval carries over — but any issues observed
    during this window are reported back and can block promotion.
12. QA Lead and Performance Lead sign off → the GitHub release is
    **promoted from pre-release to full release**.

This is the release process for node release hot fixes:

1. **Create a hotfix branch**
   - **Branch naming format**: `release/X.Y.Z` (e.g., for a hotfix on
     `10.5.0` already released, name it `release/10.5.1`).
   - **Important**: CI will only run if the branch follows this exact naming
     convention.

2. **Update the version**
   - Bump the Node version in the `cabal` file to reflect the hotfix changes.

3. **Follow the standard release process**
   Resume from **Step 5** of the [normal release process](#release-process).

# Testing

## Continuous Testing (throughout integration)

The bulk of testing happens during integration, at each layer of the stack:

- **Property tests** — extensive property-based test suites in each component
- **Unit tests** — per-package unit tests
- **Golden tests** — encoding and serialisation stability tests
- **Integration tests** — the `cardano-testnet` package in this repository
  provides the primary integration test suite

## Final Integration Tests (post pre-release tag)

Once CPC, TSC, and the Release Engineer have signed off and the pre-release
tag is cut, the QA team runs the `cardano-node-tests` suite — Python wrappers
that exercise end-to-end scenarios against the tagged binaries.

## Performance Testing

Performance tests are run against the final tagged version only. The typical
workload takes around 48 hours, though this is not a guaranteed SLA. Two
workloads are used:

- **Value workload** — raw payment transactions (throughput and latency
  baseline)
- **Plutus workload** — intensive Plutus scripts designed to stress the system

Results are reviewed and signed off by the Performance Lead before the release
is promoted to full release.

# Rotating Release Engineer Role

The Release Engineer is drawn from IOG, Tweag, or Ensurable Systems
development teams on a rotating basis, with a new engineer assigned from a
*different* team each release. They serve until the release is fully signed
off.

Their primary responsibility is integrating component releases bottom-up
through the stack into a releasable node. They coordinate with dev leads,
triage build failures, cherry-pick required in-flight PRs to release branches,
and drive the release to completion.

The Release Engineer works with the dev leads to decide which changes to
include in each component, including in-flight PRs not yet merged to `master`.

## Sub-Teams

### SRE (Site Reliability & Engineering)

The SRE team provides tooling for monitoring, logging, and measurement of live
environments. They initiate deployments to developer testnets, public
testnets, and production systems, and update dashboards and alerts to align
with new node features.

The [Cardano World book site](https://book.play.dev.cardano.org/) hosts
configuration files for every release. Full releases appear in the main
release section; pre-releases have a dedicated pre-release section on the
site.

### DevX (Developer Experience)

DevX is responsible for CI/CD, the Nix build system, OCI images (Docker
containers), systemd services, and helper scripts for running the node locally
and remotely.

### Test Engineers

Test engineers write and run integration tests from the `cardano-node-tests`
repository, and measure node synchronisation times between releases.

### Performance Engineers

Performance engineers run benchmarks of the node and report improvements and
regressions between releases.

# Versioning

The node uses "pseudo semantic versioning":

- **Major** (`X`) — references the maximum protocol version active on stable
  networks. This increments when a new era is fully activated.
- **Minor** (`Y`) — incremented for each regular release cut from `master`.
- **Patch** (`Z`) — normally `0`; incremented only for emergency hotfix
  releases branched from a prior tag.

## Pre-release vs Full Release

All releases are tagged with the plain `X.Y.Z` format — there is no `-pre`
suffix in tags. Whether a release is a pre-release or a full release is
indicated solely by its **GitHub release type**. A pre-release may be promoted
to a full release in GitHub without any code changes or new tags.

Example: `10.2.1` was published as a GitHub pre-release for approximately one
month before being promoted to a full release. The tag was always `10.2.1`;
only the GitHub release type changed.

# Collaboration

Intersect runs a weekly **Release Office Hours** meeting that all team leads
attend. Async coordination uses the `#cardano-release-tech` Slack channel.

# Release Notes

Release notes are drafted and published via the GitHub releases UI. The
standard template contains:

- (no header) A high-level summary of the release.
  For a larger release it may be best for CPC to draft this summary, since
  they have more context.
- Known Issues
- Technical Specification (usually unchanged between releases)
- Links to `cardano-node` documentation
- Changelogs
  + Summaries of each major dependency's changes, written for an interested
    user or developer. These are best drafted by the individual component
    teams rather than the Release Engineer.
  + Links to the individual changelog of each upstream package that IOG
    maintains. See the script explained below.

The previous release's notes are typically used as a starting template.

## Detailed Changelog Table

The script `scripts/generate-release-changelog-links.hs` generates a table of
changelogs for every package version in a given `cardano-node` build plan. It
takes a cabal-generated `plan.json` and a GitHub API access token.

Nix users can run it directly from the devshell:

```shellsession
$ nix build .#project.x86_64-linux.plan-nix
$ generate-release-changelog-links -o links.md result-json $GITHUB_API_TOKEN
```

`result-json` is the extra output produced by the `plan-nix` build.

Non-nix users can still run the cabal script directly, though it will build
its dependencies from scratch (requires `zlib` via your OS package manager):

```shellsession
$ scripts/generate-release-changelog-links.hs -- -o links.md result-json $GITHUB_API_TOKEN
```

For full usage, including how to generate a GitHub API token:

```shellsession
$ generate-release-changelog-links --help
# or, without nix:
$ scripts/generate-release-changelog-links.hs -- --help
```
