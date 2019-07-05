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
  genesis                         Perform genesis.
  pretty-secret-key-public        Pretty-print a secret key's public key and its hash (not a secret).
  migrate-delegate-key-from       Migrate a delegate key from an older version.
```
