# Description

Add your description here, if it fixes a particular issue please provide a
[link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=)
to the issue.

# Checklist

- [ ] Commit sequence broadly makes sense and commits have useful messages
- [ ] New tests are added if needed and existing tests are updated.  These may include:
  - golden tests
  - property tests
  - roundtrip tests
  - integration tests
  See [Runnings tests](https://github.com/input-output-hk/cardano-node-wiki/wiki/Running-tests) for more details
- [ ] Any changes are noted in the `CHANGELOG.md` for affected package
- [ ] The version bounds in `.cabal` files are updated
- [ ] CI passes. See note on CI.  The following CI checks are required:
  - [ ] Code is linted with `hlint`.  See `.github/workflows/check-hlint.yml` to get the `hlint` version
  - [ ] Code is formatted with `stylish-haskell`.  See `.github/workflows/stylish-haskell.yml` to get the `stylish-haskell` version
  - [ ] Code builds on Linux, MacOS and Windows for `ghc-8.10.7` and `ghc-9.2.7`
- [ ] Self-reviewed the diff

# Note on CI
If your PR is from a fork, the necessary CI jobs won't trigger automatically for security reasons.
You will need to get someone with write privileges.  Please contact IOG node developers to do this
for you.
