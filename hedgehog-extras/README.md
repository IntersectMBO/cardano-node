# hedgehog-extras

![Dependency Status](https://github.com/input-output-hk/hedgehog-extras/actions/workflows/cabal-outdated.yml/badge.svg)
[![CI](https://github.com/input-output-hk/hedgehog-extras/actions/workflows/haskell.yml/badge.svg)](https://github.com/input-output-hk/hedgehog-extras/actions/workflows/haskell.yml)

Supplemental library for hedgehog.

## About

This library provides additional utilities and helpers for working with the [Hedgehog](https://hackage.haskell.org/package/hedgehog) property-based testing framework.

## Installation

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - hedgehog-extras
```

## Releasing

This project has a hybrid automated/manual release flow:

### Automatic Tagging (on main branch)

When you push a version bump to main, CI automatically creates a git tag:

1. Update [CHANGELOG.md] to include the *Unreleased* changes in the changelog for the new version.
1. Update `hedgehog-extras.cabal` and set `version:` to the new version
1. Commit and push to main: `git commit -m "Bump version to X.Y.Z.W" && git push origin main`
1. CI automatically creates and pushes tag `vX.Y.Z.W`
1. The release workflow then runs automatically

### Manual Tagging

Alternatively, you can manually create and push tags:

1. Update `hedgehog-extras.cabal` with the new version
2. Commit changes: `git commit -m "Release X.Y.Z.W"`
3. Create tag: `git tag -a vX.Y.Z.W -m "Release version X.Y.Z.W"`
4. Push both: `git push origin main && git push origin vX.Y.Z.W`

### What Happens Automatically

When a tag is pushed (either automatically or manually), GitHub Actions:
- Runs the full test suite across all platforms and GHC versions
- Validates the cabal project with `cabal check`
- Creates source distributions (`cabal v2-sdist`)
- **Uploads to Hackage as a candidate** (requires `HACKAGE_USER` and `HACKAGE_PASS` secrets)
- Generates release notes from git commit history
- Creates a GitHub Release

### Publishing the Hackage Candidate

After the workflow completes:
1. Check the candidate at https://hackage.haskell.org/package/hedgehog-extras/candidates
2. Test the candidate package
3. When ready, publish it from the Hackage candidate page

### Notes

- The workflow is defined in `.github/workflows/haskell.yml`
- Packages are uploaded as **candidates**, not published releases
- Release notes are auto-generated from git commits since the last tag
- The workflow requires `HACKAGE_USER` and `HACKAGE_PASS` repository secrets
- Workflow requires `contents: write` permission (already configured)

## License

Copyright 2025 Input Output Global, Inc.

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for details.
