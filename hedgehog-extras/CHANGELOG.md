# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the [Package Versioning Policy (PVP)](https://pvp.haskell.org/).

## *Unreleased*

## [0.10.1.0] - 2025-11-03

### Changed
- Updated dependency on tar ^>= 0.7

## [0.10.0.0] - 2025-09-09

### Added
- Comprehensive CHANGELOG.md with complete project history
- Descriptive error messages for workspace test failures
- Sum type `WorkspacePolicy` to replace boolean blindness in `workspaceWithConfig`

### Changed
- `workspaceWithConfig` now uses `WorkspacePolicy` instead of `Bool` parameter
- Improved error messages in workspace tests with specific failure contexts
- Updated documentation to reference Package Versioning Policy (PVP) instead of SemVer

## [0.9.0.0] - 2025-06-24

### Changed
- Fix hanging problem with moduleWorkspace

### Added
- Workspace behavior tests and improved workspace API
- Directory dependency to test suite
- GHC 9.8 to GHA build matrix
- Claude IDE command templates for git operations

### Fixed  
- Preserve workspace directories on test failure for debugging

## [0.8.0.0] - 2025-06-14

### Added
- New `testUnitIO` function

### Changed
- Make binDist search components recursively

### Removed
- Delete unused 'h' file

## [0.7.2.0] - 2025-06-07

### Added
- MonadAssertion instance to UnitIO

## [0.7.1.0] - 2025-06-06

### Added
- New assertion functions: `tryAssertion`, `assertFailure` and `assertFailure_`
- Re-export MonadBaseControl, bracket_ and bracket

## [0.7.0.0] - 2024-12-24

### Added
- Add `expectFailure` combinator
- Add `expectFailureWith` combinator and tests
- New UnitIO monad
- Thread safe golden test support
- Add diffVsGoldenFileExcludeTrace function
- Re-export Golden module from Test module
- Move orphans to own file

### Changed
- Generalize resulting monad in `expectFailure`
- Define `expectFailure` in terms of `expectFailureWith`
- Haskell CI: reduce number of shellcheck warnings
- Haskell CI: simplify build workflow, using cardano-cli's CI as guidance
- Loosen base version, to allow building with GHC 9.12
- Align haddock pipeline with Haskell pipeline
- Make the GitHub page workflow manually triggerable (workflow_dispatch)

### Fixed
- Fix haddock for `threadDelay`
- Fix haddock for `byDeadline` and `byDuration`
- Process: more informative error message when plan.json does not exist
- Detail: fix a warning in the Haskell pipeline

## [0.6.5.1] - 2024-11-20

### Changed
- Enforce UTF-8 encoding on reading and writing files

## [0.6.5.0] - 2024-07-27

### Added
- Make it build with ghc-9.10
- CI: Add ghc-9.10 to the build matrix

## [0.6.4.0] - 2024-05-16

### Added
- Add missing `HasCallStack` to writeGoldenFile, reportGoldenFileMissing, checkAgainstGoldenFile

### Changed
- Use removePathForcibly to remove directories

## [0.6.3.0] - 2024-05-03

### Added
- Add TestWatchdog and Tripwire with their tests
- Add test for asyncRegister_

### Changed
- Switch to using haskell-actions/setup
- Allow the port to be reused immediately after it is closed

## [0.6.2.0] - 2024-04-23

### Added
- New randomPort, reserveRandomPort and portInUse functions

### Changed
- Apply suggestions from code review
- Remove double space and add quotes

## [0.6.1.0] - 2024-02-13

### Added
- Add test to ensure a directory doesn't exist (`assertDirectoryMissing`)
- Add test to ensure a directory exists (`assertDirectoryExists`)

### Changed
- Allow reading files into any FromJSON
- Lower aeson bound
- #39 Lower bound on aeson
- Remove double printing of a command with its arguments

## [0.6.0.2] - 2024-01-29

### Fixed
- Fix missing call sites for short-circuiting functions

## [0.6.0.1] - 2024-01-23

### Fixed
- Fix missing stderr in failed commands

## [0.6.0.0] - 2024-01-16

### Added
- Add exec variant that allows a negative call (#55)
- Add concurrency abstractions from lifted-async and lifted-base

## [0.5.1.0] - 2024-01-04

### Added
- New RECREATE_GOLDEN_FILES which will causes golden tests to always create golden files replacing any existing golden files

### Changed
- Use MultiwayIf to simplify code

## [0.5.0.0] - 2023-11-23

### Added
- Add upper bound to tar

### Changed
- Make it build with ghc-9.8
- CI: Add ghc-9.8.1 to build matrix

### Removed
- Remove unused import

## [0.4.8.0] - 2023-11-21

### Changed
- Make it build with ghc-9.8
- CI: Add ghc-9.8.1 to build matrix

### Removed
- Remove unused import

## [0.4.7.1] - 2023-10-05

### Removed
- Remove dependency on hw-aeson

## [0.4.7.0] - 2023-06-27

### Added
- New indexM function

## [0.4.6.0] - 2023-06-19

### Added
- Add a way to log golden files
- Create new process groups for newly spawned processes (#42)

## [0.4.5.1] - 2023-05-10

### Fixed
- Fix empty case for diffVsGoldenFile

### Changed
- Put the golden-file filename in the failure message of diffVsGoldenFile

## [0.4.5.0] - 2023-05-09

### Added
- New downloadToFile function
- New downloadAndExtractGithubCommitToTemp

### Fixed
- Fix annotation for renameFile

## [0.4.4.1] - 2023-05-05

### Added
- Make it build with ghc-9.6
- CI: Add ghc-9.6.1 to build matrix

## [0.4.4.0] - 2023-05-04

### Added
- Add extra assertion functions, fix readM to show correct line (#33)

### Changed
- Tidy up imports

## [0.4.3.0] - 2023-05-03

### Added
- Add assertWithinTolerance and readM

## [0.4.2.0] - 2023-04-28

### Added
- New functions assertFileExists assertFileMissing assertFilesMissing
- New Hedgehog.Extras.Test.Golden module

### Removed
- Delete assertFileExists from Network module because it doesn't belong here

### Changed
- Use filepath operator (</>) over (<>) with "/"

## [0.4.1.0] - 2023-04-12

### Added
- New Ok versions of functions that require the operation to succeed
- New createSubdirectoryIfMissing function

### Changed
- Modify createDirectoryIfMissing to return its argument. createDirectoryIfMissing_ will be the version that returns ()

### Fixed
- Fix compile errors

## [0.4.0.1] - 2023-03-06

### Fixed
- Fix retry function

## [0.4.0.0] - 2023-03-06

### Changed
- Pass retry count to retry function

## [0.3.0.3] - 2023-01-09

### Fixed
- Fix moduleWorkspace

## [0.3.0.2] - 2023-01-07

### Added
- Generic instance

## [0.3.0.1] - 2023-01-07

### Changed
- Don't create an intermediate workspace directory
- Find the nearest plan.json
- Restructure cabal file
- Update copyright
- Copy over waitNamedPipe from Win32-network

### Removed
- Remove unused dependency on Win32-network

### Fixed
- Use setup-haskell action

## [0.3.0.0] - 2022-12-13

### Added
- Add errorMessage to argument of deadline functions
- getProjectBase: explore up the dir hierarchy to find cabal.project
- New fromJustM function
- Add .gitignore file

### Changed
- Update GH actions versions
- Relax bounds on aeson

## Early Versions (0.1.0.0 - 0.2.x)

### 2022
- New rewriteArrayElements function
- Update to support aeson >= 2.0.0 (#8)
- Replace waitByDeadlineX functions to byDeadlineX functions that trigger retries by assertion failure (#7)
- Add retry support (#6)

### 2021
- Module re-exports (#5)
- Generic error handling functions (#4)
- New exec, binFlex functions. New execConfigCwd field (#3)
- Add support for YAML (#2)
- Force evaluation of string for cat (#1)
- Setup Github Actions CI
- Downgrade cabal file version to 2.4 because 3.0 is incompatible with stack
- Drop support for ghc-8.6.5
- Harmonise component versions and remove redundant conditionals
- Import cardano-submit-api from cardano-rest repo
- Test support for running CLI commands with environment variables
- Run golden tests at the value level rather than the text level
- Update copyright to 2021
- Cabal 3.4 in Github Actions

### 2020
- New copyRewriteJsonFile function to work around permissioning issues on Hydra
- Remove redundant imports
- General chairman test code quality changes
- Chairman test running on Windows and Linux
- Report all log files on any test failure
- Bump versions to 1.21.0 and update the change logs
- Run byron-shelley testnet
- Run a chairman process per node
- Increase test code re-use by moving test support code into separate common hedgehog-extras library

[Unreleased]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.10.0.0...HEAD
[0.10.0.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.9.0.0...v0.10.0.0
[0.9.0.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.8.0.0...v0.9.0.0
[0.8.0.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.7.2.0...v0.8.0.0
[0.7.2.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.7.1.0...v0.7.2.0
[0.7.1.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.7.0.0...v0.7.1.0
[0.7.0.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.5.1...v0.7.0.0
[0.6.5.1]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.5.0...v0.6.5.1
[0.6.5.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.4.0...v0.6.5.0
[0.6.4.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.3.0...v0.6.4.0
[0.6.3.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.2.0...v0.6.3.0
[0.6.2.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.1.0...v0.6.2.0
[0.6.1.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.0.2...v0.6.1.0
[0.6.0.2]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.0.1...v0.6.0.2
[0.6.0.1]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.6.0.0...v0.6.0.1
[0.6.0.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.5.1.0...v0.6.0.0
[0.5.1.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.5.0.0...v0.5.1.0
[0.5.0.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.8.0...v0.5.0.0
[0.4.8.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.7.1...v0.4.8.0
[0.4.7.1]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.7.0...v0.4.7.1
[0.4.7.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.6.0...v0.4.7.0
[0.4.6.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.5.1...v0.4.6.0
[0.4.5.1]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.5.0...v0.4.5.1
[0.4.5.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.4.1...v0.4.5.0
[0.4.4.1]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.4.0...v0.4.4.1
[0.4.4.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.3.0...v0.4.4.0
[0.4.3.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.4.2.0...v0.4.3.0
[0.4.2.0]: https://github.com/input-output-hk/hedgehog-extras/compare/v0.3.0.2...v0.4.2.0
