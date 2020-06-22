# Changelog for cardano-cli

## 1.13.0 -- June 2020

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

