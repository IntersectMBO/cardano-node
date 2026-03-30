# Trace Schema Overrides

Use this directory for human-maintained patches that must survive schema regeneration.

## Why

Files under `bench/trace-schemas/messages` and `bench/trace-schemas/types` are generated.
Direct edits in those generated files are hard to track and can be overwritten.

Store manual changes as sidecar override files here instead.

## Layout

Mirror the generated tree, with `.override.json` suffix:

- `bench/trace-schemas/overrides/messages/.../Foo.schema.override.json`
- `bench/trace-schemas/overrides/types/.../Bar.schema.override.json`

Each override maps to a target by:

- removing the `overrides/` path segment
- replacing `.schema.override.json` with `.schema.json`

Example:

- Override: `bench/trace-schemas/overrides/messages/Startup/Info.schema.override.json`
- Target: `bench/trace-schemas/messages/Startup/Info.schema.json`

## Override format

Override files use JSON Merge Patch semantics (RFC 7396).

You can use either:

1. A raw patch object.
2. An envelope with metadata:

```json
{
  "_meta": {
    "owner": "tracing-team",
    "reason": "Keep strict enum until upstream type metadata is available",
    "ticket": "DEVOPS-1234"
  },
  "patch": {
    "data": {
      "properties": {
        "kind": {
          "enum": ["ExpectedKindA", "ExpectedKindB"]
        }
      }
    }
  }
}
```

Notes:

- Set a key to `null` in a patch to delete it.
- Arrays are replaced as whole values (standard merge-patch behavior).

### RFC 7396 behavior summary

Merge Patch is value-oriented and simple:

- If the patch is an object, it is merged key-by-key into the target object.
- If a patch key value is `null`, that key is removed from the target object.
- If a patch key value is an object, merge continues recursively.
- If a patch key value is a scalar (`string`, `number`, `boolean`) or an array, that value fully replaces the target value.
- If the patch itself is not an object (for example an array or string), it replaces the whole target document.

Practical implications for schema overrides:

- Prefer object patches to avoid replacing entire schema files.
- Be explicit when patching `required`: it is an array, so your patch replaces the full `required` list.
- Deleting one item from an array is not supported directly by merge patch; replace the array with the exact desired final array.
- Keep patches focused to the minimal affected subtree to reduce conflicts during regeneration.

### Mini before/after example

Target fragment:

```json
{
  "data": {
    "properties": {
      "kind": { "type": "string" },
      "error": { "type": "string" }
    },
    "required": ["kind"]
  }
}
```

Patch:

```json
{
  "data": {
    "properties": {
      "error": null,
      "kind": {
        "enum": ["A", "B"]
      }
    },
    "required": ["kind", "code"]
  }
}
```

Result:

- `data.properties.error` is removed.
- `data.properties.kind` is updated (merged/replaced under that key).
- `data.required` becomes exactly `["kind", "code"]`.

## Examples

Concrete example files are also available in:

`bench/trace-schemas/overrides/examples/`

### 1) Raw patch: tighten an enum in a message schema

`bench/trace-schemas/overrides/messages/Startup/Info.schema.override.json`

```json
{
  "data": {
    "properties": {
      "kind": {
        "enum": ["StartupInfo", "StartupInfoLegacy"]
      }
    }
  }
}
```

### 2) Envelope patch with metadata: add description and required field

`bench/trace-schemas/overrides/messages/Net/Server/Local/Error.schema.override.json`

```json
{
  "_meta": {
    "owner": "tracing-team",
    "reason": "Document operator-facing semantics",
    "ticket": "TRACING-271"
  },
  "patch": {
    "data": {
      "description": "Local server accept loop error payload",
      "required": ["kind", "error"]
    }
  }
}
```

### 3) Delete generated key: remove an unwanted property

`bench/trace-schemas/overrides/types/ConnectionId.schema.override.json`

```json
{
  "properties": {
    "legacyDebugField": null
  }
}
```

## Apply / Check

Apply overrides:

`nix develop -c bash -lc "runghc -package-env - bench/trace-schemas/scripts/schema-gen/ApplySchemaOverrides.hs --verbose"`

Check that overrides are already applied:

`nix develop -c bash -lc "runghc -package-env - bench/trace-schemas/scripts/schema-gen/ApplySchemaOverrides.hs --check --verbose"`

Fail when generated schema files changed without matching override file updates:

`nix develop -c bash -lc "runghc -package-env - bench/trace-schemas/scripts/schema-gen/CheckOverrideCoverage.hs --range origin/master...HEAD"`
