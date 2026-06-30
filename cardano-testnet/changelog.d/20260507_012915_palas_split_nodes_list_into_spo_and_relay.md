### Changed

- Refactored `NodeOption` from a sum type into a record with a `TestnetNodeOptions` container
  that enforces at the type level that SPO nodes come first and at least one is present.
- `readNodeOptionsFromEnv` now validates that node directories are consecutively numbered
  and that SPOs come before relays.
