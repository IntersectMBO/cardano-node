"""Validate a JSON-lines log stream against the observability schema.

Usage:  python3 validate.py SCHEMA_DIR LOG.jsonl [MAX_EXAMPLES]

Non-JSON lines (e.g. a start-up preamble) are skipped. Prints, per namespace in
the catalogue, the number of valid and invalid events, and the distinct
validation errors with an example each. Exit code 1 if any event is invalid.

Requires the Python packages jsonschema and referencing.
"""

import json
import pathlib
import sys
from collections import Counter, defaultdict

from jsonschema import Draft202012Validator
from referencing import Registry, Resource


def load_validator(schema_dir: pathlib.Path) -> Draft202012Validator:
    resources = []
    for name in ("envelope", "types", "observables"):
        schema = json.loads((schema_dir / f"{name}.schema.json").read_text())
        Draft202012Validator.check_schema(schema)
        resources.append((schema["$id"], Resource.from_contents(schema)))
    registry = Registry().with_resources(resources)
    root = registry.contents("observables.schema.json")
    return Draft202012Validator(root, registry=registry)


def main() -> int:
    schema_dir, log = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
    max_examples = int(sys.argv[3]) if len(sys.argv) > 3 else 1
    validator = load_validator(schema_dir)
    catalogue = set(validator.schema["$defs"])

    valid, invalid = Counter(), Counter()
    # (ns, data path with array indices dropped, failed keyword) -> (count, first message, examples)
    errors = defaultdict(lambda: [0, "", []])
    skipped = 0
    with log.open() as f:
        for line in f:
            try:
                event = json.loads(line)
            except json.JSONDecodeError:
                skipped += 1
                continue
            if not isinstance(event, dict):
                skipped += 1
                continue
            ns = event.get("ns", "<no ns>")
            errs = sorted(validator.iter_errors(event), key=lambda e: list(e.absolute_path))
            if errs:
                invalid[ns] += 1
                for e in errs:
                    path = "/".join("*" if isinstance(p, int) else str(p) for p in e.absolute_path)
                    entry = errors[(ns, path, e.validator)]
                    entry[0] += 1
                    entry[1] = entry[1] or e.message[:160]
                    if len(entry[2]) < max_examples:
                        entry[2].append(line.strip()[:300])
            elif ns in catalogue:
                valid[ns] += 1

    for ns in sorted(catalogue | set(invalid)):
        if valid[ns] or invalid[ns]:
            mark = "FAIL" if invalid[ns] else "ok  "
            print(f"{mark} {ns}: {valid[ns]} valid, {invalid[ns]} invalid")
    missing = sorted(ns for ns in catalogue if not valid[ns] and not invalid[ns])
    if missing:
        print(f"\nnot present in log: {', '.join(missing)}")
    if errors:
        print("\nerrors:")
        for (ns, path, keyword), (count, msg, examples) in sorted(errors.items()):
            print(f"- {ns}  '{path}' ({keyword}, {count}x): {msg}")
            for ex in examples:
                print(f"    e.g. {ex}")
    print(f"\n{skipped} non-JSON lines skipped")
    return 1 if invalid else 0


if __name__ == "__main__":
    sys.exit(main())
