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
  keygen                   Generate a signing key.
  to-verification          Extract a verification key in its base64 form.
  redelegate               Redelegate genesis authority to a different
                           verification key.
  check-delegation         Verify that a given certificate constitutes a valid
                           delegation relationship betwen keys.
```

All commands have help available:

```
$ cabal new-run -- genesis-tool byron-pbft migrate-delegate-key-from --help
Usage: genesis-tool migrate-delegate-key-from SYSTEMVER --to FILEPATH
                                              --from FILEPATH
  Migrate a delegate key from an older version.

Available options:
  --to FILEPATH            Output secret key file.
  --from FILEPATH          Secret key file to migrate.
  -h,--help                Show this help text

System version
  byron-legacy             Byron Legacy mode
  byron-pbft               Byron PBFT mode

```

## Genesis operations

The canned `scripts/genesis.sh` example provides a nice set of defaults and
illustrates available options.  Running it will produce a `./genesis.${systemStart}` directory,
where `${systemStart}` will be chosen 15 minutes in the future.

The directory will have the following content:

                            ## Affected by

 - `avvm-seed.*.seed`         - --avvm-entry-count and --avvm-entry-balance
 - `delegate-keys.*.key`      - --n-delegate-addresses
 - `delegation-cert.*.json`   - --n-delegate-addresses
 - `genesis-keys.*.key`       - --n-delegate-addresses, --total-balance
 - `poor-keys.*.key`          - --n-poor-addresses, --total-balance

Dummy genesis, that corresponds to `Test.Cardano.Chain.Genesis.Dummy.dummyConfig.configGenesisData`
can be dumped by the `dump-hardcoded-genesis` subcommand.

The `print-genesis-hash` subcommand will compute the genesis hash of a given genesis JSON file.
That value will be suitable for the `--genesis-hash` option of `cardano-node`.

## Key operations

Note that no key operation currently supports accepting password-protected keys.
The `keygen` subcommand, though, can generate such keys.

### Signing key generation & verification key extraction

Signing keys can be generated using the `keygen` subcommand, password protection being
controlled by the `--no-password` flag.

Extracting a verification key out of the signing key is performed by the `to-verification` subcommand.

## Delegate key migration

In order to continue using a delegate key from the Byron Legacy era in the new implementation,
it needs to be migrated over, which is done by the `migrate-delegate-key-from` subcommand:

```
$ cabal new-run -- genesis-tool byron-pbft migrate-delegate-key-from byron-legacy \
                                           --from key0.sk --to key0.pbft
```

## Delegation

The `redelegate` subcommand enables generation of delegation certificates,
given the following inuputs:

   - protocol magic
   - starting epoch of delegation
   - delegator signing key
   - delegate verification key

To check the generated delegation certificate, you can use the `check-delegation` subcommand,
which would verify:

   - certificatesignature validity
   - correspondence of the expected issuer/delegate with those on the certificate.

The expected issuer and delegate are supplied through the `--issuer-key` and `--delegate-key`
options.

## Signing key queries

One can gather information about a signing key's properties through the `signing-key-public`
and `signing-key-address` subcommands (the latter requires the network magic):

```
$ cabal new-run -- genesis-tool byron-pbft signing-key-public \
                                           --secret key0.pbft
public key hash: a2b1af0df8ca764876a45608fae36cf04400ed9f413de2e37d92ce04
     public key: sc4pa1pAriXO7IzMpByKo4cG90HCFD465Iad284uDYz06dHCqBwMHRukReQ90+TA/vQpj4L1YNaLHI7DS0Z2Vg==

$ cabal new-run -- genesis-tool byron-pbft signing-key-address \
                                           --secret key0.pbft  \
                                           --testnet-magic 459045235
2cWKMJemoBakxhXgZSsMteLP9TUvz7owHyEYbUDwKRLsw2UGDrG93gPqmpv1D9ohWNddx
VerKey address with root e5a3807d99a1807c3f161a1558bcbc45de8392e049682df01809c488, attributes: AddrAttributes { derivation path: {} }
```
