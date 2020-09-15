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
  byron                    Byron specific commands

Shelley specific commands
  shelley                  Shelley specific commands

Miscellaneous commands
  version                  Show the cardano-cli version
```

Shelley-specific commands

```
cardano-cli shelley --help
Up to date
Usage: cardano-cli shelley COMMAND
  Shelley specific commands

Available options:
  -h,--help                Show this help text

Available commands:
  address                  Shelley payment address commands
  stake-address            Shelley stake address commands
  key                      Shelley key utility commands
  transaction              Shelley transaction commands
  node                     Shelley node operaton commands
  stake-pool               Shelley stake pool commands
  query                    Shelley node query commands. Will query the local
                           node whose Unix domain socket is obtained from the
                           CARDANO_NODE_SOCKET_PATH enviromnent variable.
  genesis                  Shelley genesis block commands
  governance               Shelley governance commands
  text-view                Commands for dealing with Shelley TextView files.
                           Transactions, addresses etc are stored on disk as
                           TextView files.


```

Byron-specific commands

```
cardano-cli byron --help
Up to date
Usage: cardano-cli byron (COMMAND | COMMAND | COMMAND | COMMAND | COMMAND | 
                           COMMAND | COMMAND)
  Byron specific commands

Available options:
  -h,--help                Show this help text

Available commands:
  key                      Byron key utility commands
  transaction              Byron transaction commands
  query                    Byron node query commands.
  genesis                  Byron genesis block commands
  governance               Byron governance commands
  delegation               Byron delegation commands
  miscellaneous            Byron miscellaneous commands
```

## How to build

### Cabal

Use [Cabal - Version 3.0](https://www.haskell.org/cabal/) to build and/or install this project:

```
$ cd cardano-cli
$ cabal build
$ cabal install
```

It may be necessary to specify the installation directory when installing the command using the `--installdir` option.
