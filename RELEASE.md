# Team

The release team is made up of the following:

* *Release Squad Lead* (usually Cardano Head of Product)
* *Rotating Release Engineer* from one of the development teams
* *SRE Team* supporting deployment and CI/CD
* *Test Engineers* that focus on integration tests
* *Performance Engineers* running benchmarking tests
* *Dev Leads* for each of the components of the node

# Release Process

This is the release process for node releases

1. Release Squad Lead will work with Release Engineer and Dev Leads to determine where to cut the releases from ledger,
   network and node. These will be published via CHaP and will follow the defined process of the team for versioning, etc...
2. These will be integrated up the stack to the node to produce a release branch
3. Release Engineer will work with Test/Performance sub-teams to initiate a testing round.
   This may use a tagged release, a commit taken from master,
   or a hotfix branch based on a previously released version.
4. A release candidate will be deployed to preview/preprod by SRE
5. Community will be notified of release candidate and given a few days to provide feedback
6. Release Engineer will create a draft GitHub Release containing an empty template for the notes
7. Release Engineer will ask Dev Leads to fill out their section of the high-level changelog in the release notes
8. Release Squad Lead will finalize the Release notes
9. Head of Engineering will meet with release team and identify if release should be published as stable or beta (pre-release)
   in GitHub.
   Stable releases can go all the way to mainnet, beta should only go to preprod and wait for that release to be relabeled as stable or a new stable release to be cut.
10. GitHub release is published

# Rotating Release Engineer Role

All sprints are aligned across the node and it's components. At the end of a sprint cycle the new rotating Release Engineer is decided on by the leadership team.
This person's primary duties are integration of new releases of dependencies up the stack to the node. They serve this role until the release is finalized
according to the above release process (ideally 1 sprint cycle).
The Release Engineer works with the Dev Leads to decide which changes should be included in their component, including any in-flight PRs that haven't been merged yet.

## Sub-Teams

### SRE (Site Reliability & Engineering)

The SRE team provides the tooling for monitoring, logging and measurement of live environments. The team initiates deployments of new versions to developer
testnets, public testnets and production systems. They are responsible for updating dashboards/alerts to align with new node features/refactoring.

### DevX (Developer Experience)

The DevX team is responsible for CI/CD, the building process (using nix and compiling manually), OCI images (e.g. docker containers), systemd services,
and helper scripts associated in running the node for local development and remote deployment purposes.

### Test Engineers

The test engineers are responsible for writing and running integration tests from `cardano-node-tests` repository. They execute integration tests as well as
tests that measure node synchronization times between releases.

### Performance Engineers

Performance engineers run benchmarks of the node and report any improvements/regressions between node versions.

# Versioning

The node uses a "pseudo semantic versioning" that takes into account breaking change (e.g. new eras) in the versioning logic. This new versioning
standard within the node is supported as of `8.0.0` release. The first part of the version always references the max protocol version allowed
on a stable network with no additional experimental override flags. This first part will remain `8` referencing Babbage era until the next era (Conway)
is finalized. The second digit is a new release incremented counter. Every release based off the master trunk will increment this number by one. The final
part should always be `0` *unless* an emergency bug fix is necessary that cannot wait until the next major release. This should be a rare occurrence going
forward and is used for forking a new version off a previous release and backporting fixes.

Not all releases are declared stable. Releases that aren't stable will be released as *pre-releases* and will append a `-pre` tag indicating it is not ready
for running on production networks. The same version can be re-released without the pre tag without making any code changes by submitting a new tag without the
`pre` suffix. This means stable could jump from `8.0.0 -> 8.3.0` without ever releasing `8.1.0`, `8.1.1`, `8.2.0`, etc...

Note that the version always has three parts, so a major release has a trailing `.0`.

# Collaboration

The release team meets for a quick touch-point weekly where all team leads are invited. Currently these calls are closed to the public, but in the future we expect
to open them up to the larger community. The release team also meets ad-hoc as needed and collaborates asynchronously throughout the week.

# Release notes

The release notes are drafted and published via the GitHub releases UI.
Our current template contains the following sections.

- (no header) A very-high level summary of the release.
  For a larger release, it may be best for the Cardano Head of Product to draft this summary instead of the Release Engineer, since they have more context.
- Known Issues
- Technical Specification (usually unchanged)
- Links to `cardano-node` documentation
  It seems to be a judgement call whether each of these should specify the upstream version.
- Changelogs
  + Summaries of the major dependencies' changelogs.
    These are written as a few sentences that an interested user and/or dev would find helpful.
    It may be best for the individual teams to draft these summaries instead of the Release Engineer, since they have more context.
  + Links to the individual changelog of each upstream package that IOG maintains.
    See the script explained below.

Usually the release notes from the previous release are copied and used as a template.

## Detailed changelog table

There's a script (`scripts/generate-release-changelog-links.hs`) that generates a table of changelogs for each of the package versions included in a given `cardano-node` release. The script takes a cabal-generated `plan.json` and a GitHub API access token, and outputs a large table which contains links to the `CHANGELOG.md` file (if one exists) for each of the package versions contained in the build plan.

Example usage:

```shellsession
$ nix build .#project.x86_64-linux.plan-nix.json
...
$ scripts/generate-release-changelog-links.hs -- -o links.md result-json $GITHUB_API_TOKEN
...
```

For more information, including how to generate / retrieve a GitHub API token, use

```
scripts/generate-release-changelog-links.hs -- --help
```

Note that this is a cabal script and may take a while to build all the dependencies. You will need to have the `zlib` native library available, either installed via your OS package manager or by using `nix-shell -p zlib`.
