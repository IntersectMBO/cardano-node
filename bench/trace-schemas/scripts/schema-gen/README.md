# GhciSchemaGen overview

## What it does
- Generates JSON schemas for trace messages in `bench/trace-schemas/messages` and type schemas in `bench/trace-schemas/types`.
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
