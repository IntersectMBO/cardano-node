# `genesis-tool`

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

The general synopsis is as follows:
 ```
   genesis-tool SYSTEMVER COMMAND
```

..where `SYSTEMVER` is one of the supported system generations: `byron-legacy`, `byron-pbft` etc.

The supported commands are (as per excerpt from the tool's `--help`):

```
Available commands:
  genesis                  Perform genesis.
  signing-key-public       Pretty-print a signing key's verification key (not a
                           secret).
  migrate-delegate-key-from
                           Migrate a delegate key from an older version.
  dump-hardcoded-genesis   Write out a hard-coded genesis.
  print-genesis-hash       Compute hash of a genesis file.
  signing-key-address      Print address of a signing key.
.
```

The canned `scripts/genesis.sh` example provides a nice set of defaults.
