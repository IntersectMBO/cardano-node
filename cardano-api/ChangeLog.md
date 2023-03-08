# Changelog for cardano-api

## vNext

### Features

- Append, not prepend change output when balancing a transaction ([PR 4343](https://github.com/input-output-hk/cardano-node/pull/4343))

- Expose convenience functions `executeQueryCardanoMode`, `determineEra`, `constructBalancedTx` and `queryStateForBalancedTx` ([PR 4446](https://github.com/input-output-hk/cardano-node/pull/4446))

- Expand `BalancedTxBody` to include `TxBodyContent` ([PR 4491](https://github.com/input-output-hk/cardano-node/pull/4491))

- Change `calculateMinimumUTxO` to return `Lovelace` instead of a `Value` ([PR 4482](https://github.com/input-output-hk/cardano-node/pull/4482))

- **Breaking change** - Reduce exposed modules in cardano-api ([PR4546](https://github.com/input-output-hk/cardano-node/pull/4546))

- **Breaking change** - `deserialiseFromRawBytes` method of the `SerialiseAsRawBytes` type class to return `Either` instead of `Maybe`.  Deprecate `eitherDeserialiseFromRawBytes`.  Use `deserialiseFromRawBytes` instead.

- The `cardano-cli governance create-update-proposal` command to reject empty cost model ([PR4885](https://github.com/input-output-hk/cardano-node/pull/4885))

- **Breaking change** - Preserve ScriptData bytes with HashableScriptData ([PR4886](https://github.com/input-output-hk/cardano-node/pull/4886))


- **Breaking change** - `determineEraExpr` to return `IO (Either UnsupportedNtcVersionError AnyCardanoEra)` instead of `IO AnyCardanoEra`.
  ([PR4788](https://github.com/input-output-hk/cardano-node/pull/4788))

- **Breaking change** - `queryExpr` to return `IO (Either UnsupportedNtcVersionError a)` instead of `IO a`.
  ([PR4788](https://github.com/input-output-hk/cardano-node/pull/4788))
  
- **Breaking change** - Remove distinction between multisig and timelock scripts([PR4763](https://github.com/input-output-hk/cardano-node/pull/4763))

### Bugs

- Allow reading text envelopes from pipes ([PR 4384](https://github.com/input-output-hk/cardano-node/pull/4384))

## 1.35.3 -- August 2022

- Fix leadership schedule for current on babbage (#4106)
- Update build to allow all invalid scripts (again) (#4088)
- Fix building of Alonzo transaction in Babbage era. (#4166)
- Add `utxoCostPerByte` protocol parameter (#4141)

## 1.35.2 -- July 2022 (not released)

None

## 1.35.1 -- July 2022 (not released)

None

## 1.35.0 -- June 2022
- Add Vasil hardfork to cardano-api and cardano-cli (#3765)
- Reference script integration (#3953)
- Wire up remaining Plutusv2 reference script types (#4034)
- Add `IsString` (Hash BlockHeader) (#3619)
- Make `LedgerStateEvents` a type alias (#3692)
- Propagate protocol epoch state decode error (#3696)
- Expose the tx mempool monitoring mini protocol in cardano-api (#3706)
- Babbage functionality integration in cardano api Part 1 (#3803)
- Remove unused package (#3816)
- Add `IsCardanoEra` constraint to BlockInMode (#3665)
- Update cardano-api's TxOut with inline datum (#3773)
- Update cardano-api txout with reference scripts (#3779)
- Implement return and total collateral in cardano-api (#3787)
- Add reference transaction inputs to cardano-api (#3804)
- Fix datum in tx and ref scripts (#3882)
- Support the babbage era in the API function `cddlTypeToEra` (#3916)
- Fix typo for TxWitness BabbageEra (#3961)
- kes-period-info property test (#3718)
- Extend deserialiseFromRawBytesHex to produce error description (#3304)
- add genesis create-cardano command (#3832)
- Propagate protocol in block type (#3818)
- Create VRF signing key file with correct permissions (#1948)
- Update example-reference-script-usage.sh to also use inline datums (#4006)
- Restore deleted comment (#4044)
- Do not require decentralization parameter in protocol parameters (#4051)

## 1.34.0 -- February 2022

- Expose `lovelaceToTxOutValue`. (#3381)
- Implement two functions: `currentEpochEligibleLeadershipSlots` and
  `nextEpochEligibleLeadershipSlots` to get the leadership slots for the
  current/next epoch respectively. (#3464, #3494)
- Various small internal fixes. (#3466)
- Add a `capi` library to support using the cardano node as a C library in other
  software. (#3501)
- `fromShelleyAddr` now takes an explicit `ShelleyBasedEra` parameter to
  determine the era. The previous behaviour (with an implicit
  `IsShelleyBasedEra` constraint) can be obtained with `fromShelleyAddrIsSbe`.
  (#2253, #3606)

## 1.33.0 -- December 2021
## 1.32.1 -- November 2021

- Asset names are now rendered in a more consistent fashion in JSON output.
  Previously names which happened to be valid ASCII were rendered as such, and
  ones which were not resulted in unprintable characters. Now all names are
  rendered as hex. Future clients may choose to additionally render ASCII names
  if plausible to do so. (#3211)
- Various testing improvements. (#3361)
- Expose ledger events via the ledger state API. (#3374)

## 1.31.0 -- October 2021

- Various internal improvements and refactorings. (#3163, #3253, #3288)

## 1.30.0 -- September 2021

- Improvements to the ledger state API. (#3143)
- Make it easier to use monadic queries. (#3151)
- Implement 'getBlockHeader' for Alonzo. This was a stray function that got
  missed when implementing Alonzo in the API. (#3158)
- A few additional exports for API consumers. (#3156)
- Expose ledger events through the API. Ledger events provide a way for
  consumers to receive details about things that are happening inside the
  ledger, and will be used by tools such as db-sync. (#3085)
- Improve the error message reported when you try to spend a non-Plutus locked
  input using a Plutus script. (#3187)


## 1.29.0 -- August 2021

- Support for automated Tx building. (#2953)
- A few additional exports for API consumers. (#3001, #3055)
- Miscellaneous internal improvements. (#2948)
- Block folding interface now derives the network ID automatically from the
  ledger config. (#2955, #2975)
- Improve the error generated when a Tx output does not meet the minimum UTxO
  value. (#3027)
- Add support for querying the Alonzo ledger state. (#2974)
- Update the API documentation.

## 1.28.0 -- July 2021

- Support for the upcoming Alonzo era, including protocol parameters, Plutus
  scripts and collateral inputs. (#2784, #2798, #2808, #2810, #2815, #2818,
  #2823, #2828)
- Add a function 'getTransactionBodyContent'. This extracts a general view of
  the TxBody from the era-specific bodies. (#2663)
- Add API support for new node queries:
  - `QuerySystemStart` gets the system start time.
  - `QueryStakePools` and `QueryStakePoolParameters` can be used to get details
    on the currently known stake pools.
  - `QueryUTxOFilter` provides various ways to query a filtered subset of the
    UTxO.
  (#2843)
- Added functions to the API to assist in automated transaction building:
  - `evaluateTransactionBalance` computes the current balance of a (partial)
    transaction, which is helpful for determining what needs to be done to
    correctly balance it (such that value produced equals value consumed).
  - `evaluateTransactionExecutionUnits` computes how many ExUnits will be needed
    by all the scripts in a (partial) transaction.
  - `evaluateTransactionFee` computes the fee for a (partial) transaction,
    assuming a given number of VKey witnesses (corresponding to inputs).
  - `estimateTransactionKeyWitnessCount` attempts to estimate the number of VKey
    witnesses needed.
  - `makeTransactionBodyAutoBalance` attempts to create and automatically
    balance a transaction body, using the above tools.
  (#2906)
- Miscellaneous internal improvements. (#2836, #2840)

## 1.27.0 -- April 2021

- Add initial support for the ledger state and folding over blocks to the API.
  (#2633)
- Scripts are now stored within the TxBody in the API, rather than in the
  witnesses. (#2547)

## 1.26.1 -- March 2021

- The cardano-submit-api now takes transactions encoded as CBOR rather than
  JSON. This reverts a change to existing behaviour for backwards compatibility.
  (#2491, #2512)
- Remove a backwards-compatibility workaround related to the optional query
  point (#2241 below) when querying the NodeLocalState. This had resulted in
  spurious notifications of disconnection in the logs. Note that as a
  consequence of this, instances of the CLI and other tools using the 1.26.1 API
  will fail to query node state from older versions of the node. (#2540)

## 1.26.0 -- March 2020
- Added a demo for the use of cardano-client. This is an API to allow writing
  programs to interact with the cardano node. (#2295, #2303)
- Removed code pertaining to the old IPC API (#2319)
- Add the ability to calculate the minimum deposit needed for a transaction to
  the API, given a value. (#2325)
- When querying the NodeLocalState, make the query point optional, and use the
  chain tip when not specified. (#2241)
- Various internal improvements and refactoring (#2349, #2458)

## 1.25.0 -- January 2020
- New IPC modules for easier interaction with the node, including support for
  all existing local state queries (#2230, #2238, #2263, #2277, #2286)
- API support for Byron era update proposals and votes (#2209, #2271)
- Make Cardano.Api the primary public module for the API.
- API support for serialising multi-asset PolicyId and AssetName (#2270)
- API for pretty-printing JSON output (#2103)
- Improved tests for Byron era legacy key formats (#2259)
- More precise error cases for tx outputs that are out of range (#2217)
- Host up-to-date generated API documentation via github
  https://input-output-hk.github.io/cardano-node/ (#2273, #2276, #2278)

## 1.24.2 -- December 2020

None

## 1.24.1 -- December 2020

- Fix the getTxId implementation for Byron-era transactions (#2169)

## 1.24.0 -- December 2020

- Full API support for the Allegra and Mary eras, including creating
  transactions for the new eras, and support for the special new features in
  the new eras: script extensions, tx validity intervals, auxiliary scripts,
  multi-asset tx outputs and asset minting (#2092, #2110, #2111, #2121, #2127,
  #2128, #2141, #2149)


## 1.23.0 -- November 2020

- Preliminary support for the Allegra script language extensions (#2069)
- Preliminary support for the Mary multi-asset extensions (#2083, #2085, #2093)
- Internal refactoring of the API code (#2040, #2055, #2094)

## 1.22.1 -- October 2020

None

## 1.22.0 -- October 2020

- Preliminary support for the upcoming Allegra and Mary eras (#1958, #2019)
- Additional test coverage (#1999)

## 1.21.2 -- September 2020

- Add a Ed25519-BIP32 instance of the new crypto classes (#1933, #1952)
- Adjust what is exposed via Cardano.Api.{Byron,Shelley} (#1932)

## 1.21.1 -- September 2020

None

## 1.21.0 -- September 2020
- Support for multi-signature scripts (#1788)
- Support for Byron witnesses for addresses that use attributes, which includes
  all addresses in legacy Daedalus Byron wallets (#1851, #1871)
- Introduce a Cardano.Api top level module exporting only the public parts
  and modules Cardano.Api.{Byron,Shelley} that expose the underlying library
  types for applications that need it (#1881)

## 1.20.0 -- September 2020

- Improved support for JSON to Tx metadata conversions, with two supported
  JSON schemas, suitable for different use cases (#1797)

## 1.19.1 -- September 2020

- Adjust the tx metadata JSON schema to be fully recursive (#1735)
- Audit compliance with CIP5 for common bech32 prefixes (#1781)
- Add functionality for validating tx metadata (#1432, #1677)

## 1.19.0 -- August 2020

- Support for scripts and specifically multi-sig scripts (#1623)
- Support for JSON syntax for multi-sig scripts (#1660)
- Support for converting tx metadata to/from JSON (#1606, #1682)
- Support for Bech32-encoded stake pool IDs (#1528, #1638)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

## 1.18.0 -- July 2020

None

## 1.17.0 -- July 2020

- Allow genesis keys as tx witnesses (#1483)
- Allow extended genesis delegate keys to sign operational certs (#1497)
- Add support for extended keys for stake, genesis and delegate keys (#1487)

## 1.16.0 -- July 2020

- Remove the old API (#1444, #1456)
- Added raw serialisation instances for all key types (#1455)
- Added bech32 serialisation following draft CIP 5 (#1455)

## 1.15.1 -- July 2020

- Include tx metadata in transactions in the new api (#1406)
- Add support for extended ed25519 keys for payment keys (#1411)
- Improve tx submission API in the new API (#1430)

## 1.15.0 -- July 2020

- Fix the ledger state dump query (#1333, #1334)
- Support for Byron witnesses in Shelley txs in the typed API (#1339)
- Support for Bech32 serialisation in the typed API (#1382)
- Support for other additional functionality in the typed API (#1337, #1375)
- More tests for the typed API (#1360, #1369, #1378)
- Moving code around to eliminate the cardano-config package (#1289, #1380)

## 1.14.2 -- June 2020

- Fix the query that dumps the ledger state as JSON (#1333)

## 1.14.1 -- June 2020

No changes in the cardano-api. There were changes in the cardano-node.

## 1.14.0 -- June 2020

- Improvements to the strongly-typed API (#1112, #1220, #1227, #1246)

  The API is not yet stable in this release.

## 1.13.0 -- June 2020

- Initial version of an improved strongly-typed API.
  Initially focusing on creating and serialising keys.

  The API is not yet stable in this release.

## 1.11.0 -- April 2020

- Initial version of the API package. The package provides client-side
  functionality for constructing and submitting transactions.

  The API is not yet stable in this release.

- Initial transaction API with Byron support and Shelley stubs (#787)
- Shelley address key pair generation (#799)
