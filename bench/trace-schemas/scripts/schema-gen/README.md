# GhciSchemaGen overview

`make trace-schemas-regenerate`

or the step-by-step commands:

`nix develop -c cabal run cardano-node -- trace-documentation --config configuration/cardano/mainnet-config.yaml --output-namespace-list bench/trace-schemas/newNamespaces.txt --output-file bench/trace-schemas/trace-documentation.md`

`nix develop -c bash -lc 'GHC_LIBDIR=$(ghc --print-libdir) runghc bench/trace-schemas/scripts/schema-gen/GhciSchemaGen.hs'`

`nix develop -c bash -lc "runghc -package-env - bench/trace-schemas/scripts/schema-gen/ApplySchemaOverrides.hs --verbose"`

`nix develop -c bash -lc "runghc -package-env - bench/trace-schemas/scripts/schema-gen/ValidateTraceSchemas.hs"`

`nix develop -c bash -lc "runghc -package-env - bench/trace-schemas/scripts/schema-gen/ValidateTraceLog.hs --log-file run/.../stdout"`

## What it does

- Generates JSON schemas for trace messages in `bench/trace-schemas/messages` and type schemas in `bench/trace-schemas/types`.
- Uses the trace-documentation command to generate `bench/trace-schemas/newNamespaces.txt` from `MetaTrace` namespaces.
- Parses `namespaceFor` and `forMachine` clauses in source files, then asks `cabal repl` (GHCi) for types of variables used in those `forMachine` patterns.

## High-level flow

1. **Find relevant Haskell files**
   - Scans `cardano-node/src`, `trace-dispatcher/src`, `trace-forward/src`, `trace-resources/src`.
   - Keeps only `.hs` files that contain `forMachine` or `namespaceFor`.

2. **Build namespace mapping**
   - Parses `namespaceFor` clauses to map a constructor (e.g. `ReplayBlockStats`) to namespace parts (e.g. `["LedgerReplay"]`).
   - This tells it which constructor corresponds to each namespace in `bench/trace-schemas/newNamespaces.txt`.

3. **Parse `forMachine`**
   - Extracts:
     - The constructor pattern in each clause.
     - The JSON keys from lines like `"foo" .= ...`.
     - Which pattern variable supplies each key.
   - Literal string RHS like `"kind" .= String "X"` are treated as string fields.

4. **Ask GHCi for types**
   - For each file, it runs `cabal repl cardano-node` and issues `:t` queries for patterns.
   - Extracts variable types from the returned type signatures or error output.
   - This becomes a map `constructor -> variable -> type`.

5. **Build/update schemas**
   - For each namespace in `newNamespaces.txt`:
     - Finds the matching constructor via the namespace map.
     - Builds or updates the `data.properties` schema using:
       - key -> variable mapping from `forMachine`
       - variable types from GHCi
   - If `data.properties` is still empty, it optionally pulls `data` from `bench/trace-schemas/messages-hist`.

## Key heuristics

- Namespace matching uses suffix match so `Namespace [] ["LedgerReplay"]` can match `ChainDB.ReplayBlock.LedgerReplay`.
- If a field uses a literal string (`String "..."`), it is treated as `type: string`.
- If `data.properties` does not exist, it synthesizes an object schema and fills properties from `forMachine`.

## Where to look in code

- Entry point and flow: `main`
- Namespace mapping: `parseNamespaceMap`, `findCtor`
- forMachine parsing: `parseForMachineClauses`, `parseFieldVarMap`, `parseFieldLine`
- GHCi type extraction: `ghciTypesForFile`, `runGhci`
- Schema update: `updateSchemaForNamespace`, `updateData`, `buildDataFromFieldMap`

## Validation

- `ValidateTraceSchemas.hs` checks `meta.schema.json` with `check-jsonschema`, then validates every file in `bench/trace-schemas/messages` against that meta-schema.
- The Haskell script controls discovery and execution; the actual JSON Schema validation is delegated to `check-jsonschema` via `nix run nixpkgs#check-jsonschema`.
- Run it with `runghc -package-env - ...` so the standalone script does not inherit the repo's package environment.
- `ValidateTraceLog.hs` validates a real cardano-node log file: it skips the non-JSON preamble, validates the common envelope against `TraceMessage.schema.json`, validates known namespaces against the matching schema in `bench/trace-schemas/messages`, and reports namespaces that do not have a corresponding message schema.

## Human changes that survive regeneration

- Treat `bench/trace-schemas/messages` and `bench/trace-schemas/types` as generated outputs.
- Put manual edits in sidecar override patches under `bench/trace-schemas/overrides`.
- Apply overrides with `ApplySchemaOverrides.hs` after every generation.
- Enforce in CI with:
  - `make trace-schemas-regenerate`
  - `make trace-schemas-overrides-check`
  - `make trace-schemas-overrides-coverage RANGE=origin/master...HEAD`

See `bench/trace-schemas/overrides/README.md` for override format and file layout.
