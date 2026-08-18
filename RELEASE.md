# Team

The release process is owned by **Intersect** (the Cardano MBO), which runs a
weekly Release Office Hours meeting attended by all team leads to give status
updates and plan upcoming releases.

Code contributions and maintenance come from **BlockPQR**, **ICAN**, **IOG**,
**Midgard Labs** and **Serokell**, which also provide rotating release engineers.

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
| Plutus | Plutus team (Midgard Labs) |
| Ledger | Ledger team (BlockPQR) |
| Consensus | Consensus team (Serokell) |
| Network | Network team (IOG) |
| API & CLI | Node/API/CLI team (BlockPQR) |
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
   Plutus → Ledger → Consensus → Network → API → CLI → Node.
3. Components pass their own property tests, golden tests, unit tests, and
   integration tests before integration proceeds up the stack. Integration
   tests using the `cardano-testnet` package (in this repository) provide the
   primary integration signal at the node level.
4. When the full stack is stable, upstream components are released to CHaP
   bottom-up, and the Release Engineer replaces source-repository-package
   (SRP) stanzas with the CHaP releases, working up the stack until the node
   is ready. With those dependency updates in place, prepare the
   [Herald-managed packages](#herald-managed-packages) in this repository.
   For each new release, including a pre-release, the Release Engineer bumps
   the `cardano-node` Cabal version and, in the same change, increments the
   minor block-header protocol version to the next unused integer for that
   major protocol version. Coordinate across `master` and release branches
   as described in [Block-header Protocol Version](#block-header-protocol-version).
5. SRE deploys the release candidate to preview/preprod networks, validates
   its operation, and advises the Release Engineer when it is ready for
   tagging.
6. The Release Engineer ensures the release changes are merged into the
   target branch (`master` or the maintenance branch) and the required
   checks pass on the commit being released. They create and push the
   signed release tag, triggering the artifact upload workflow, which
   creates a draft GitHub Release if one does not already exist. The
   Release Engineer asks dev leads to fill in their changelog sections.
7. The community is notified of the release candidate and given time to
   provide feedback.
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
   - Base the work on the appropriate maintenance branch, named
     `release/X.Y.x`, such as `release/11.2.x`. If no maintenance branch
     exists, create one from the prior release tag. The `release/` prefix
     is required for Hydra to create its jobset automatically.
   - Open a PR against the maintenance branch and confirm that the required
     GitHub Actions and Hydra checks run.

2. **Update the version**
   - Bump the Node version in the `cabal` file to reflect the hotfix changes.
   - In the same change, increment the minor block-header protocol version
     to the next unused integer for that major protocol version, coordinating
     across `master` and release branches as described in
     [Block-header Protocol Version](#block-header-protocol-version).

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

Once CPC, TSC, the Release Engineer, and SRE have signed off and the GitHub
pre-release is published, the QA team runs the `cardano-node-tests` suite — Python wrappers
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

The Release Engineer is drawn from BlockPQR, ICAN, IOG, Midgard Labs or
Serokell development teams on a rotating basis, with a new engineer assigned
from a *different* team each release. They serve until the release is fully
signed off.

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

The [Cardano Operations book site](https://book.play.dev.cardano.org/) hosts
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
- **Patch** (`Z`) — normally `0`; incremented for fixes and maintenance within
  an existing release series. Patch releases may be cut from a maintenance
  branch or from `master` after reviewing the included changes.

## Block-header Protocol Version

Whenever the `cardano-node` Cabal version is bumped for a new release, including
a pre-release, also update the minor block-header protocol version in
`Consensus.cardanoProtocolVersion` in
[`Cardano.Node.Protocol.Cardano`](cardano-node/src/Cardano/Node/Protocol/Cardano.hs).
This is separate from the minor component of the Cabal package version.

For each major protocol version, start the minor protocol version at `0` for
its first release and increment it by one for each subsequent release or
pre-release. Allocate these values across both `master` and all release
branches: check previously allocated versions before choosing the next value.
Do not reset the counter for a new branch or Cabal release series, or reuse a
major/minor protocol version pair for a different release. This rule applies
to the normal protocol path; the experimental hard-fork path is exempt.

## Pre-release vs Full Release

All releases are tagged with the plain `X.Y.Z` format — there is no `-pre`
suffix in tags. Whether a release is a pre-release or a full release is
indicated solely by its **GitHub release type**. A pre-release may be promoted
to a full release in GitHub without any code changes or new tags.

Example: `10.2.1` was published as a GitHub pre-release for approximately one
month before being promoted to a full release. The tag was always `10.2.1`;
only the GitHub release type changed.

## Herald-managed packages

`cardano-node-chairman`, `cardano-submit-api` and `cardano-testnet` manage changelogs with herald (see `.herald.yml`): each PR adds a fragment under the package's `.changes/` directory.
Before merging and tagging the release, run `herald batch <package>` (available in the nix dev shell) to compile the changelog and bump the version in the cabal file.
It bumps relative to the current cabal version, so pass the version explicitly (`-v A.B.C.D`) if that was already bumped by hand.

# Collaboration

Intersect runs a weekly **Release Office Hours** meeting that all team leads
attend. Asynchronous coordination uses a Slack channel shared between all
participating organizations.

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
  + Links to the individual changelog of each upstream package included in
    the release. See the script explained below.

The previous release's notes are typically used as a starting template.

## Detailed Changelog Table

`generate-release-changelog-links` generates a table of changelogs for
CHaP-hosted package versions in a given `cardano-node` build plan. It reads
`GITHUB_TOKEN` from the environment (generate one at https://github.com/settings/tokens or
run `gh auth token`).

Run from the release checkout so the plan describes the version being
released. Nix devshell users have the binary available directly:

```shellsession
$ nix build .#project.x86_64-linux.plan-nix
$ export GITHUB_TOKEN="$(gh auth token)"
$ generate-release-changelog-links -o links.md
```

`result/plan.json` is the default plan path. Pass an explicit path as the
first argument to override it.

Without entering the devshell, use `nix run .#generate-release-changelog-links
-- -o links.md` after generating the plan and setting the token.

Non-Nix users can generate a plan and run via Cabal:

```shellsession
$ cabal build all --dry-run
$ cp dist-newstyle/cache/plan.json release-plan.json
$ export GITHUB_TOKEN="$(gh auth token)"
$ cabal run generate-release-changelog-links -- -o links.md release-plan.json
```

Keep that copy of the node plan: invoking Cabal for the helper can update
`dist-newstyle/cache/plan.json` for the helper's own build.

For full usage:

```shellsession
$ generate-release-changelog-links --help
```

# Tagged Artifacts and Publication

After the release changes have been merged into the target branch (`master`
or the appropriate maintenance branch) and the required checks have passed
on the commit being released, create and push the signed `X.Y.Z` tag pointing
to that commit. The `Post Release Upload` workflow waits for Hydra's
`ci/hydra-build:required` check, retrieves the five platform archives from the
IOG binary cache, and uploads them with a SHA-256 checksum file to a draft
GitHub release. It preserves an existing release's metadata if one already
exists for the tag. The separate `Upload to ghcr.io` workflow publishes the
container images.

Verify that all archives, checksums, release notes, and required sign-offs are
present before publishing the draft as a GitHub pre-release. Publishing also
triggers the release workflows; the asset upload replaces same-name assets,
so wait for the workflows to finish before treating the downloads as complete.

After publishing a release or pre-release, the Release Engineer notifies SRE
so they can update the [Cardano Operations book](https://book.play.dev.cardano.org/)
with the corresponding release information and configuration files. Notify
SRE again when a pre-release is promoted to a full release so its placement
in the book can be updated.
