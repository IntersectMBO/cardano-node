# Changelog for cardano-cli

## 1.19.0 -- August 2020

- Support for converting ITN extended keys to Shelley stake keys (#1579)
- Support for converting password-protected Byron signing keys (#1633)
- Support for building script addresses (#1641)
- Improve the output of the stake-address-info query (#1546, #1636, #1671)
- Support for Bech32-encoded stake pool IDs (#1528, #1638, #1730)
- Reorganise the Byron CLI commands similarly to the Shelley ones (#1609, #1628)
- Code organisation refactoring (#1457, #1594)
- Extra tests and refactoring of tests (#1565, #1566, #1602, #1668)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

## 1.18.0 -- July 2020

- Properly display the tx hash in the UTxO query command output (#1526, #1535)
- Refactoring and minor improvements in tests (#1538, #1541)

## 1.17.0 -- July 2020

- Allow genesis keys as tx witnesses (#1483)
- Allow extended genesis delegate keys to sign operational certs (#1497)
- New cli "key" command with key utilities (#1487, #1493)
- More helpful flag defaults in cli command for fee calculation (#1516)
- Default to the Cardano protocol for talking to a node (#1515)

## 1.16.0 -- July 2020

- Accept either a pool id or verification key in delegation cli command (#1460)
- Improved bash completion for flags that accept files (#1459)
- More and improved integration tests (#1429, #1450, #1453)

## 1.15.1 -- July 2020

- Support for interacting with nodes running in Byron-only, Shelley-only or
  the composite Cardano mode (Byron;Shelley) (#1435)
- Add support for byron keys and extended ed25519 keys (#1411)
- Port the CLI command implementations to the new API (#1416)
- Fix the output of the calculate-min-fee command (#1408)
- New stake and VRF key hashing commands (#1407)
- Use JSON output format for the address info command (#1426)

## 1.15.0 -- July 2020

- Fix the ledger state dump query (#1333, #1334)
- Fix the format of Byron addresses used in Byron CLI commands (#1326)
- Port CLI commands to use the new API (#1341, #1375, #1396, #1397)
- Change to JSON output for the "query tip" command (#1340, #1365)
- Moving code around to eliminate the cardano-config package (#1289, #1316)

## 1.14.2 -- June 2020

- Fix the hashing of stake pool metadata
- Fix the query that dumps the ledger state as JSON (#1333)

## 1.14.1 -- June 2020

No changes in the cardano-cli. There were changes in the cardano-node.

## 1.14.0 -- June 2020

- New flags for transaction metadata in tx construction (#1233)
- New flags for reward account withdrawals in tx construction (#1237)
- New command for pool metadata JSON validation and hashing (#1234, #1299)
- New flags for pool metadata in pool registration cert command (#1234)
- New flags for pool relays in pool registration cert command (#1282, #1296)
- New command to convert ITN keys (#1070, #1136)
- New command to get the txid of a tx body (#1231)
- Return appropriate exit code for tx submission failures (#1226)
- Fix the query stake-address-info to accept stake addresses (#1194, #1197)
- More regression tests (pioneer exercises 2, 3, 4) (#1209, #1247, #1279, #1287)
- Start to migrate to using the new typed API from cardano-api lib (#1284, #1298)
- Fix reporting of git revision via version command (#1283)

## 1.13.0 -- June 2020

- Fix the parsing of the pool margin in pool registration certs (#1063, #1110)
- Change the Shelley cli command and flag names to be more consistent (#1068)
- Add a command to query stake addresses, balance and delegation (#1053, #1129)
- Add a command to get the stake pool id (#1069)
- Add a command to create MIR certificates (#1075)
- Improved human readable error messages for Shelley commands (#1021)
- Improve error message for tx-in parser errors (#1066)
- Use a better default value of eMax in generated example genesis files (#1145)
- Regression tests covering the "pioneer" exercises 1 (#1073)
- Prerequisites for Tx metadata support (but not full support yet) (#1080)
- Updated Shelley from scratch documentation (#1062)

## 1.12.0 -- May 2020

- Reorganise the `shelley` subcommands (#840, #845)
- New `shelley genesis create` command (#852, #864, #908, #926, #929)
- New key-gen commands for various Shelley  keys (#846, #870)
- New commands for Shelley  address construction (#870, #872, #887)
- New Shelley transaction sign command (#894, #900)
- New Shelley transaction submission command (#904)
- New node query commands (#880, #884, #903, #918, #920, #933, #994, #1008, #1016)
- New commands to create stake address certificates (#890, #919, #967)
- New commands to create stake pool certificates (#922)
- New system commands to update genesis delgations and create MIR certs (#895)
- New command to calculate the minimum fee for a transaction (#931)
- New command to view the content of the various binary files (#915)
- New command to create Shelley protocol param updates (#950, #1004)
- Byron update proposal vote creation and submission (#804)
- Various refactoring (#874, #875, #949, #958, #966, #972)
- Commands that talk to the node no longer require the node config file (#901,
  #907, #917, #913, #928)
- Improved human readable error messages for Byron commands (#1003)
- Documentation on constructing a Shelley chain from scratch (#893, #932, #1000)
- Add `version` command and `--version` flag, with git revision (#959)
- Additional tests (#898, #935, #941, #952)


## 1.11.0 -- April 2020

- First version of the CLI as a separate package. The package provides a CLI
  (command line interface) to various low level node-related functionality.

  The CLI is not yet stable in this release.

- Split the `cardano-cli` package out of `cardano-node` (#819)
- Initial structure of Shelley CLI commands with a top-level "shelley" command
- Group Byron commands under a top-level "byron" command
- Commands to generate Shelley KES and VRF keys (#816)
- Command to generate Shelley address keys (#824)

