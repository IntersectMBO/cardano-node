# Changelog for cardano-cli

## 1.35.1 -- July 2022

None

## 1.35.0 -- June 2022
- Add Vasil hardfork to cardano-api and cardano-cli (#3765)
- Reference script integration (#3953)
- Wire up remaining Plutusv2 reference script types (#4034)
- Add friendly printing of transactions (envelopes) with signatures (#3617)
- cardano-cli transaction view: Add friendly certificate printing (#3377)
- cardano-cli query kes-period-info: Always display metrics (#3683)
- JSON format for leadership schedule (#3687)
- Vasil cardano-cli update (#3810)
- Prevent return collateral from including reference scripts and datums (#3850)
- kes-period-info property test (#3718)
- Extend deserialiseFromRawBytesHex to produce error description (#3304)
- add genesis create-cardano command (#3832)
- Propagate protocol in block type (#3818)
- Fix kes period info command (#3945)
- Create VRF signing key file with correct permissions (#1948)
- Set local encoding to UTF-8 in cardano-cli (#4018)
- Update example-reference-script-usage.sh to also use inline datums (#4006)
- Wire up simple reference scripts in cardano-cli (#4014)
- Add read-only-tx-in-reference option to cardano-cli #(4042)

## 1.34.0 -- February 2022

- Fix some spelling errors in the CLI help text.  (#3499)
- Add a prettier rendering of update proposals. (#3208)
- Add support for CBOR-encoded blobs in the `transaction build` and `transaction
  build-raw` commands. (#3483)
- Implement a `leadership-schedule` command. This can calculate a stake pool's
  leadership schedule for the current and following epoch. It requires access to
  the VRF signing key for that stake pool.

  ```
  > cardano-cli query leadership-schedule \
     --testnet-magic 42 \
     --genesis example/shelley/genesis.json \
     --stake-pool-id  pool12t0y7agkqct89pf00eeytkvfjlquv76tjy27duannan9w63ckxv \
     --vrf-signing-key-file example/node-pool1/shelley/vrf.skey
     --current
     SlotNo                          UTC Time
     --------------------------------------------------------
     4073                   2021-12-29 17:26:54.998001755 UTC
     4126                   2021-12-29 17:27:00.298001755 UTC
     4206                   2021-12-29 17:27:08.298001755 UTC
     4256                   2021-12-29 17:27:13.298001755 UTC
     4309                   2021-12-29 17:27:18.598001755 UTC
     4376                   2021-12-29 17:27:25.298001755 UTC
     4423                   2021-12-29 17:27:29.998001755 UTC
     4433                   2021-12-29 17:27:30.998001755 UTC
  ``` (#3464, #3494)
- The CLI now supports outputting transaction bodies in ledger-compliant CDDL in
  the `transaction build` and `transaction build-raw` commands. This is
  specified by using the `--cddl-format` flag. (#3505)
- Implement a `kes-period-info` command in the CLI. This checks that your
  operational certificate is correct. It checks:
  - The counters match what is in the node's protocol state
  - The KES period in the operational certificate is correct (based on the
    current slot).
  ```
  > cardano-cli query kes-period-info --testnet-magic 42  \
    --op-cert-file example/node-pool1/shelley/node.cert
  ✓ The operational certificate counter agrees with the node protocol state counter
  ✓ Operational certificate's kes period is within the correct KES period interval
  {
      "qKesNodeStateOperationalCertificateNumber": 6,
      "qKesCurrentKesPeriod": 404,
      "qKesOnDiskOperationalCertificateNumber": 6,
      "qKesRemainingSlotsInKesPeriod": 3760228,
      "qKesMaxKESEvolutions": 62,
      "qKesKesKeyExpiry": "2022-03-20T21:44:51Z",
      "qKesEndKesInterval": 434,
      "qKesStartKesInterval": 372,
      "qKesSlotsPerKesPeriod": 129600
  }
  ``` (#3459, #3572, #3599)
- The CLI now displays collateral inputs in a nicer fashion. (#3463)
- The `transaction sign` command now allows for incremental signing by providing
  an already signed transaction via `--tx-file`. This allows more easily adding
  multiple signatures to a transaction. (#3549)
- The `transaction build` command now supports an option
  (`--calculate-plutus-script-cost`) to compute the cost for included scripts.
  ```
  cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$plutusutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+10000000" \
  --tx-in-script-file "$plutusscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$redeemerfilepath" \
  --calculate-plutus-script-cost "$WORK/create-datum-output.scriptcost"
  > cat $WORK/create-datum-output.scriptcost
  [
    {
        "executionUnits": {
            "memory": 1700,
            "steps": 476468
        },
        "lovelaceCost": 133,
        "scriptHash": "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
    }
  ]
  ``` (#3589)

## 1.33.0 -- December 2021
## 1.32.1 -- November 2021

- Default CLI commands to the Alonzo era. (#3339)
- Add defaults for building the Alonzo genesis. (#3346)

## 1.31.0 -- October 2021

- Restore support for deserialising transactions built by pre-1.27.0.0 node
  versions. (#3226)
- Various internal refactorings and improvements. (#3234)
- Use the new `GetChainBlockNo` and `GetChainPoint` queries in the query tip
  command. There is a fallback to the older method using the full chain sync
  query. (#3179)
- Allow provision of optional datums to a transaction using the CLI option
  `--tx-out-datum-embed-value`. This mechanism can for example be used to
  provide the actual script locking an output, for use when spending it. (#3171)
- Fix the use of withdrawls using the `transaction build` command. (#3317)
- Allow extended payment keys to be specified as a Plutus required signer.
  (#3319)

## 1.30.0 -- September 2021

- Allow the user to specify a signature as required when spending from a
  multisig/timelock script using the `build` or `build-raw` commands. Required
  signers *must* be present in the witnesses, and only required signers are
  visible to Plutus scripts. (#3123)
- Use a separate connection for the `query tip` command. This fixes an
  occasional bug where the `query tip` command would fail. (#3130)
- Print the Tx fee when using the `tx build` command. (#3032)
- The `tx build` command now validates its inputs (ensuring they are in the UTxO
  and that only basic VKey-locked inputs are used as collateral.) (#3151)
- Add a new comment to query the stake pools. (#3152)
- `tx build` now uses the set of existing stake pools to determing if a pool is
  already registered (and hence whether it must pay a deposit). (#3152)
- `calculate-min-req-utxo` now requires a transaction output, not just a value
  as before. This is required in the Alonzo era, and the change is made
  everywhere for consistency. (#3181)
- Allow the `tx build` command to spend the entirety of a UTxO and create no
  change output. (#3188)
- Add withdrawls to the `tx view` command. (#2613)
## 1.29.0 -- August 2021

- Add a "tx build" command to the CLI. This command takes care of calculating
  the appropriate fee for a transaction, and balancing the transaction
  appropriately. It does not do input selection for fees, so sufficient balance
  must be available in the inputs to pay the computed fee, and sufficient
  collateral must also be present when phase-2 validating scripts are used. The
  tx build command is capable of computing both the fees required from
  transaction size and the fees incurred by script execution. (#2921, #2953,
  #2995, #3025)
- Improve the output format for rational fields in protocol parameters and
  genesis. When these are simple, we now convert them to decimal format. (#2992)
- Various internal improvements. (#2932)
- Make the CLI help text more nicely formatted. (#2945)
- Introduce the `--script-valid` and `--script-invalid` flags. The latter can be
  used to mark a script as being known invalid, such that the node will allow it
  to be submitted anyway (whereas under normal operation it would reject such a
  transaction in order to avoid loss of collateral). This flag is only likely to
  be of use in testing. The `--script-valid` flag is set as a default. (#3050,
  #3091, #3093)
- Add colours to the CLI output. (#3023)
## 1.28.0 -- July 2021

- The query tip command is now tidier and shows various additional pieces of
  information:
  - The epoch number is now shown during the Byron era.  Previously this worked
    only in the Shelley and subsequent eras. (#2688)
  - The sync progress of the node. This will only be available with new network
    protocols (not yet in this release.) (#2842, #2899)
  (#2885)
- Attempting to use an IPv6/IPv4 address where the other is expected will now
  give a more helpful error message. (#2691)
- Queries should now work during the Alonzo era. (#2727, #2755)
- Support for submitting transactions during the Alonzo era. (#2774, #2798,
  #2806, #2811, #2823, #2863, #2848)
- `cardano-cli genesis create` now also creates the new Alonzo genesis file.
  (#2743)
- The UTxO CLI query now allows an additional `--tx-in` flag which allows
  filtering the UTxO by TxIn, and requires the addition of the `--whole-utxo`
  flag to return the complete UTxO set (which was previously the default).
  Returning the whole UTxO set is an expensive operation only useful in small
  testnets, so we don't want it as the default option. (#2843, #2854)
- The parser for rational units (as used in for example execution unit prices)
  now supports rational syntax (e.g. 1/2). (#2922)

## 1.27.0 -- April 2021

- The query tip now also returns the era (e.g. Shelley, Allegra, Alonzo).
  (#2561, #2562, #2598)
- The `address build` command now incorporates the functionality of the script
  address build command, which is now deprecated. (#2486, #2587)
- Add additional commands for creating MIR certificates to the CLI. This
  supports the ability to transfer funds to the treasury for Catalyst projects.
  (#2503)
- As a result of refactoring in preparation for the upcoming Alonzo release,
  there are a couple of breaking changes in CLI commands referring to scripts:
  - Auxiliary scripts (i.e. those included in the Tx auxiliary data, which are
    not required as transaction signers) must now be included with
    `--auxiliary-script-file` rather than with `--script-file`.
  - Scripts witnessing txins, certificates, withdrawals and minting must now be
    paired with the thing they are witnessing. E.g.
    ```
    --certificate-file  $certfile --certificate-script-file $scriptfile
    --tx-out $txout --mint-script-file $scriptfile
    --withdrawal $withdrawal --withdrawal-script-file $scriptfile
    --tx-in $txin --txin-script-file $scriptfile
    ```
  - Scripts should now be specified when creating the txbody, rather than when
    signing the transaction. (#2547)
- The transaction view command now additionally shows detailed of minted
  non-native tokens. (#2550)
- Removed support for Byron addresses using the Bech32 encoding. The only
  supported way to use Byron-era addresses is through a file, using the text
  envelope format. (#2605)
- Add a new command which computes the minimum ADA value/deposit for a
  multi-asset value. (#2612)
- Add two new query commands:
  - `query stake-snapshot` allows querying the three stake snapshots for a given
    stake pool.
  - `query pool-params` returns the current and future parameters, as well as
    the retiring information.
  (#2560)
- Updated the CLI reference documentation. (#2665)

## 1.26.1 -- March 2021
- It's no longer necessary to specify the era when making a CLI query. When not
  specified, the current era will be used as a default. (#2470)

## 1.26.0 -- March 2021
- Add three new queries to the CLI, exposing functionality already present in
  the API:
  - Protocol parameters
  - Stake distribution
  - Individual stake addresses
  (#2275, #2290)
- Fix the rendering of Byron-era `TxOut`s to be consistent with the rendering for
  Shelley-era addresses. (#2472)
- Add `cardano-cli transaction view`, which allows for pretty-printing details
  about a serialised transaction. (#2348)
- When constructing MIR certificates, the CLI now takes stake addresses rather
  than stake certificates. These are strictly more general and can be deduced
  from the certificates.
- Make the Mary era the default era in the CLI (#2415)
- Migrate the `cardano-submit-api` tool from `cardano-rest`. (#2370)
- The 'tip' query now additionally returns the epoch at the tip (#2440)
- Various internal improvements and refactoring (#2458)

## 1.25.0 -- January 2021
- Allow creating transactions with no outputs (#2223, #2226)
- Improved error messages for syntax errors in out-of-range lovelace quantities
  in transaction outputs (#2063, #2079)
- Improved reference documentation for simple scripts and their use (#2165)
- Refactoring in the Byron part of the CLI to make more extensive use of the
  Cardano API and reduce the maintenance burden (#2103, #2228)
- Remove support for changing the delegation from Genesis keys to operational
  keys in the Byron era. This feature was never used on the mainnet during the
  Byron era. (#2219)
- Clearer usage information in the CLI `--help` output (#2203)

## 1.24.2 -- December 2020

- Rename the flags `--lower-bound` and `--upper-bound` to be `--invalid-before`
  and `--invalid-hereafter` respectively, for naming consistency (#2186, #2190)
- Hide the deprecated `--ttl` flag in the `--help` output (#2189, #2190)

## 1.24.1 -- December 2020

- New command `transaction policyid` for making multi-asset policy ids (#2176)
- New command `byron transaction txid` to help scripts with getting the
  transaction id for Byron transactions made using the cli (#2169)
- New `--tx-file` flag for the command `transaction txid` to accept complete
  txs, not just tx bodies (#2169)
- Add a regression test for the "0" case of multi-asset tx out values (#2155)

## 1.24.0 -- December 2020

- CLI support for the Allegra and Mary eras, including creating transactions
  for the new eras, and support for the special new features in the new eras:
  script extensions, tx validity intervals, auxiliary scripts, multi-asset tx
  outputs and asset minting. (#2072, #2129, #2136)
- New flags for the `build-raw` command:
  + `--invalid-before` and `--invalid-hereafter` for the new Allegra-era feature
    of transaction validity intervals. The existing flag `--ttl` is equivalent to
    the new `--invalid-hereafter`, but it is now optional in the Allegra era.
  + `--script-file` for the new Allegra-era feature of being able to include
     auxiliary scripts in a transaction.
  + `--mint` for the Mary-era token minting feature.
- It is now necessary to specify the target era (e.g. `--allegra-era`) when
  creating a transaction (with `build-raw`) so that the right format and
  feature-set is used. The `--shelley-era` remains the default.
- It is necessary for now to specify the target era when using the CLI query
  commands. This may become automatic in future. The default is `--shelley-era`.
- Move all the Shelley sub-commands to the top level of the command line.
  For example `cardano-cli shelley transaction build-raw` becomes simply
  `cardano-cli transaction build-raw`. The existing names are also kept for
  compatibility. (#2076, #2145)
- Updated help text for the ledger/protocol state queries to clarify that they
  are primarily for debugging and are not stable interfaces (#2125, #2126, #2133)
- New command `genesis create-staked` to make it easier to set up Shelley-based
  testnets with stake pools and delegation set up from the genesis. (#2052)

## 1.23.0 -- November 2020

- Create VRF keys with the correct file permissions (#1948)
- New command to query the Shelley protocol (not just ledger) state (#2057)
- Skeletons of the new commands and flags for the multi-asset extensions (#2081)

## 1.22.1 -- October 2020

None

## 1.22.0 -- October 2020

- Adjust the ledger state dump to return the "extended" ledger state (#2019)
- Preliminary support for the upcoming Allegra and Mary eras (#1958, #2019)

## 1.21.2 -- October 2020

- Support bech32 and hex formats for reading verification keys (#1852)
- Minor help text improvements (#1661, #1956)
- Fix typo in KES docs (#1917, #1953)
- Improved documentation for CLI multi-signature support (#1976)

## 1.21.1 -- September 2020

None

## 1.21.0 -- September 2020
- Support for multi-signature scripts (#1788, #1880)

## 1.20.0 -- September 2020

- New command for creating genesis key delegation certificates (#1784)
- New command for converting more legacy signing key formats (#1756, #1822)
- Improved support for JSON to Tx metadata conversions, with two supported
  JSON schemas, suitable for different use cases (#1797)
- Support bech32 and hex formats for reading signing keys (#1790)
- Improved error messages for cli errors (#1801, #1839)

## 1.19.1 -- September 2020

- Fix the testnet vs mainnet argument for the genesis create command (#1761)
- Fix the --treasury flag for MIR cert creation (#1780)
- Fix the output rendering in the command to hash genesis files (#1713, #1767)
- Validate CBOR tx metadata when building tx bodies (#1432, #1677)

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
