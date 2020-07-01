# cardano-cli


A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

The general synopsis is as follows:
 ```
   Usage: cardano-cli (Genesis related CMDs | Key related CMDs | Delegation related CMDs | Transaction related CMDs | Local node related CMDs)
```

The top-level commands are as shown below.

```
$ cardano-cli --help
cardano-cli - utility to support a variety of key operations (genesis
generation, migration, pretty-printing..) for different system generations.

Usage: cardano-cli (Byron specific commands | Shelley specific commands | 
                     Miscellaneous commands)

Available options:
  --version                Show the cardano-cli version
  -h,--help                Show this help text

Byron specific commands
  byron                    Byron node operation commands
  genesis                  Create genesis.
  print-genesis-hash       Compute hash of a genesis file.
  keygen                   Generate a signing key.
  to-verification          Extract a verification key in its base64 form.
  signing-key-public       Pretty-print a signing key's verification key (not a
                           secret).
  signing-key-address      Print address of a signing key.
  migrate-delegate-key-from
                           Migrate a delegate key from an older version.
  issue-delegation-certificate
                           Create a delegation certificate allowing the
                           delegator to sign blocks on behalf of the issuer
  check-delegation         Verify that a given certificate constitutes a valid
                           delegation relationship between keys.
  submit-tx                Submit a raw, signed transaction, in its on-wire
                           representation.
  issue-genesis-utxo-expenditure
                           Write a file with a signed transaction, spending
                           genesis UTxO.
  issue-utxo-expenditure   Write a file with a signed transaction, spending
                           normal UTxO.
  get-tip                  Get the tip of your local node's blockchain
  validate-cbor            Validate a CBOR blockchain object.
  pretty-print-cbor        Pretty print a CBOR file.

Shelley specific commands
  shelley                  Shelley specific commands

Miscellaneous commands
  version                  Show the cardano-cli version
```

See [`../README.md`](../README.md) for full usage instructions and examples of use.

## How to build

### Cabal

Use [Cabal - Version 3.0](https://www.haskell.org/cabal/) to build and/or install this project:

```
$ cd cardano-cli
$ cabal build
$ cabal install
```

It may be necessary to specify the installation directory when installing the command using the `--installdir` option.
