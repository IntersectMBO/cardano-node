# ChangeLog

## 2.14.1 -- June 2024
* A new NixSvcOptions field is introduced: `_nix_keepalive`
  and it's propagated down to the `kaClient` that does keepalives.
  This makes keepalive timeouts configurable.
* The fast-solo profile is introduced for quick test runs.
* A `CHANGELOG.md` is created for the tx-generator.
