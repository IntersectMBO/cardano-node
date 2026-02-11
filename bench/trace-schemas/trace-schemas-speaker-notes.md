# Speaker Notes: JSON Schemas for Cardano Traces

## Slide 1: JSON Schemas for Cardano Traces

This presentation is a short overview of `bench/trace-schemas`.
The main idea is that this part of the repository gives us JSON Schemas for Cardano node trace messages.
That means tracing is not only readable by humans, but also structured and verifiable by tools.
The flow to keep in mind for the whole talk is simple: discover namespaces, generate schemas, apply human patches, and validate the result.

## Slide 2: 1. General Idea And Construction Script

The main entry point is `scripts/schema-gen/RegenerateTraceSchemas.sh`.
It starts by running `cardano-node trace-documentation`, which generates `newNamespaces.txt` and `trace-documentation.md`.
That gives us the discovered trace namespaces and a human-readable documentation file.
Then it runs `GhciSchemaGen.hs`, which scans Haskell source, reads `namespaceFor` and `forMachine`, and uses GHCi to infer the field types used in the trace payloads.
After generation, it runs `ApplySchemaOverrides.hs` to re-apply human-maintained changes.
Finally, it runs `ValidateTraceSchemas.hs` to make sure the produced schemas are still valid.
So the pipeline is automated end to end: discover, generate, override, validate.

## Slide 3: 2. Folders

The generated output is mainly split into `messages/` and `types/`.
`messages/` contains the actual trace message schemas, grouped by namespace areas such as `Consensus`, `Net`, `ChainDB`, and `Startup`.
`types/` contains reusable type schemas that those message schemas can reference.
The `overrides/` folder is where human-maintained patches live, and it mirrors the generated tree.
At the root level, there are also a few support files: `meta.schema.json`, `TraceMessage.schema.json` and the generated `newNamespaces.txt`.
So this folder structure separates generated artifacts, reusable definitions, support metadata, and manual adjustments.

## Slide 4: 2. Scripts

The key script is `GhciSchemaGen.hs`, because that is what actually builds the schemas.
`ApplySchemaOverrides.hs` is responsible for patching the generated files with manual corrections.
`ValidateTraceSchemas.hs` checks that the schema files themselves conform to the local meta-schema.
`ValidateTraceLog.hs` goes one step further and validates a real cardano-node log file against the common envelope and then the namespace-specific schemas.
`CheckOverrideCoverage.hs` is more of a maintenance and CI helper: it checks that if generated schema files changed, the matching override files were updated too.
So the scripts are not only about generation. They also enforce correctness and workflow discipline.

## Slide 5: 3. Overrides

Overrides are necessary because `messages/` and `types/` are generated outputs in a heuristic way.
If we edit those files directly, our changes can disappear the next time the regeneration script runs.
Instead, manual edits go into `overrides/messages/...` or `overrides/types/...`, using the `.schema.override.json` suffix.
These files use JSON Merge Patch semantics.
That means object fields merge recursively, arrays replace whole values, and setting a key to `null` deletes it.
The practical value is that generated output stays reproducible, while human decisions stay explicit, reviewable, and persistent across regeneration.
The main takeaway is that `bench/trace-schemas` is not just a directory of schema files. It is a managed pipeline with a safe manual patch layer on top.
