# Changelog for cardano-api

## 8.1.0

### Features

- New error exports:
  - `TxOutInAnyEra(..)`
  - `txOutInAnyEra`
  - `StakePoolMetadataValidationError(..)`
  - `ScriptHash(..)`
  ([PR 5188](https://github.com/input-output-hk/cardano-node/pull/5188))

### Bugfix

- Fix `toEraInMode` for Conway
  ([PR 5175](https://github.com/input-output-hk/cardano-node/pull/5175))

### 8.0.0 -- May 2023

- Add `getSlotForRelativeTime` function ([PR 5130](https://github.com/input-output-hk/cardano-node/pull/5130))

- New `ToJSON ScriptWitnessIndex` instance that produces machine readable output.
  Any `JSON` output uses this instance.
  [PR 5168](https://github.com/input-output-hk/cardano-node/pull/5168)

### Features

- Delete `readEnvSocketPath` function.
  ([PR 5207](https://github.com/input-output-hk/cardano-node/pull/5207))

- Expose node config reading functionality: `NodeConfig`, `NodeConfigFile` and `readNodeConfig`

- Expose genesis file reading functionality:
  - All eras: `GenesisConfig` and `readCardanoGenesisConfig`
  - Byron: `readByronGenesisConfig`
  - Shelley: `ShelleyConfig`, `GenesisHashShelley`, `readShelleyGenesisConfig` and `shelleyPraosNonce`
  - Alonzo: `GenesisHashAlonzo` and `readAlonzoGenesisConfig`
  - Conway: `GenesisHashConway` and `readConwayGenesisConfig`

- Expose envirnment construction functionality: `mkProtocolInfoCardano` and `genesisConfigToEnv`

- New error exports:
  - `TxOutInAnyEra(..)`
  - `txOutInAnyEra`
  - `StakePoolMetadataValidationError(..)`
  - `ScriptHash(..)`
  ([PR 5188](https://github.com/input-output-hk/cardano-node/pull/5188))

- Rename `TestEnableDevelopmentHardForkEras` to `ExperimentalHardForksEnabled` and
  `TestEnableDevelopmentNetworkProtocols` to `ExperimentalProtocolsEnabled`
  ([PR 4341](https://github.com/input-output-hk/cardano-node/pull/4341))

- Changed type of `protocolParamTxFeeFixed`, `protocolParamTxFeePerByte` from `Natural` to
  `Lovelace` and `protocolUpdateTxFeeFixed` and `protocolUpdateTxFeePerByte` from `Maybe Natural`
  to `Maybe Lovelace` ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- Append, not prepend change output when balancing a transaction ([PR 4343](https://github.com/input-output-hk/cardano-node/pull/4343))

- Expose convenience functions `executeQueryCardanoMode`, `determineEra`, `constructBalancedTx` and `queryStateForBalancedTx` ([PR 4446](https://github.com/input-output-hk/cardano-node/pull/4446))

- Expand `BalancedTxBody` to include `TxBodyContent` ([PR 4491](https://github.com/input-output-hk/cardano-node/pull/4491))

- Change `calculateMinimumUTxO` to return `Lovelace` instead of a `Value` ([PR 4482](https://github.com/input-output-hk/cardano-node/pull/4482))

- **Breaking change** - Reduce exposed modules in cardano-api ([PR4546](https://github.com/input-output-hk/cardano-node/pull/4546))

- **Breaking change** - `deserialiseFromRawBytes` method of the `SerialiseAsRawBytes` type class to return `Either` instead of `Maybe`.  Deprecate `eitherDeserialiseFromRawBytes`.  Use `deserialiseFromRawBytes` instead.
  ([PR 4876](https://github.com/input-output-hk/cardano-node/pull/4876))

- The `cardano-cli governance create-update-proposal` command to reject empty cost model ([PR4885](https://github.com/input-output-hk/cardano-node/pull/4885))

- **Breaking change** - Preserve ScriptData bytes with HashableScriptData ([PR4886](https://github.com/input-output-hk/cardano-node/pull/4886))


- **Breaking change** - `determineEraExpr` to return `IO (Either UnsupportedNtcVersionError AnyCardanoEra)` instead of `IO AnyCardanoEra`.
  ([PR4788](https://github.com/input-output-hk/cardano-node/pull/4788))

- **Breaking change** - `queryExpr` to return `IO (Either UnsupportedNtcVersionError a)` instead of `IO a`.
  ([PR4788](https://github.com/input-output-hk/cardano-node/pull/4788))

- **Breaking change** - Remove distinction between multisig and timelock scripts([PR4763](https://github.com/input-output-hk/cardano-node/pull/4763))

- **Breaking change** Change return type of `queryNodeLocalState` to new `AcquiringFailure` type.

- **Breaking change** - For performance reasons, `evaluateTransactionFee` to take a
  `Ledger.PParams (ShelleyLedgerEra era)` argument instead of `ProtocolParameters`
  New type `BundledProtocolParameters` and new functions `bundleProtocolParams` and `unbundleProtocolParams`.
  ([PR4903](https://github.com/input-output-hk/cardano-node/pull/4903))

- Auto-balance multi asset transactions ([PR 4450](https://github.com/input-output-hk/cardano-node/pull/4450))

- **Breaking change** - Removed `fromShelleyPParams` in favor of
  `fromLedgerPParams ShelleyBasedEraShelley`
  ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - JSON fields have been changed: ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))
  - For `PState`:
    - Renamed `"pParams pState"` -> `"pParams"`
    - Renamed `"fPParams pState"` -> `"fPParams"`
    - Renamed `"retiring pState"` -> `"retiring"`
    - Added `"deposits"`
  - For `DState`:
    - Removed `"unifiedRewards"`
    - Added `"unified"`, which contains an object with both rewards and deposits.
  - For `InstantaneousRewards`:
    - Addition of `"deltaReserves"` and `"deltaTreasury"` fields
  - `CostModel` in `AlonzoGenesis` and `PParams` is formatted with a list of values to
    promote forward compatibility

- Fix a bug where only metadata from TxAuxData was hashed upon
  transaction body creation with `createTransactionBody` ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - Change representation of `CostModel`. It is no longer a mapping from
  param name to values, but instead a list with values, where order of value dictates the
  mapping to param names of a plutus cost model for a particular plutus version ([PR
  5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - ToJSON instance for CostModel and consequently for
  ProtocolParameters will now produce a list of values instead of a key value
  mapping. ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - `calculateMinimumUTxO` no longer fails, it is a total computation.
  ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- New generators in `gen` sublibrary: `genPositiveLovelace`, `genPositiveQuantity` and
  `genSignedNonZeroQuantity`. ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))
- New 'Governance.Poll' API implementing [CIP-0094](https://github.com/cardano-foundation/CIPs/pull/496) ([PR 5050](https://github.com/input-output-hk/cardano-node/pull/5050))

- Split serialisation from IO
  ([PR 5049](https://github.com/input-output-hk/cardano-node/pull/5049))

- Move parsers to reusable location
  ([PR 5046](https://github.com/input-output-hk/cardano-node/pull/5046))

- Remove unused error constructors
  ([PR 5041](https://github.com/input-output-hk/cardano-node/pull/5041))

- New `bounded` function
  ([PR 4919](https://github.com/input-output-hk/cardano-node/pull/4919))

- Modify `constructBalancedTx` to take `LedgerEpochInfo`
  ([PR 4858](https://github.com/input-output-hk/cardano-node/pull/4858))

- Add `ReaderT` of `NodeToClientVersion` to `LocalStateQueryExpr`
  ([PR 4809](https://github.com/input-output-hk/cardano-node/pull/4809))

- New `QueryStakeSnapshot` query
  ([PR 4754](https://github.com/input-output-hk/cardano-node/pull/4754))
  ([PR 4179](https://github.com/input-output-hk/cardano-node/pull/4179))

- Move signing key reading to cardano-api
  ([PR 4698](https://github.com/input-output-hk/cardano-node/pull/4698))

- Replace `Data.Map` with `Data.Map.Strict`
  ([PR 4675](https://github.com/input-output-hk/cardano-node/pull/4675))

- New `Cardano.Api.DeserialiseAnyOf` module
  ([PR 4639](https://github.com/input-output-hk/cardano-node/pull/4639))

- Replace `deserialiseFromRawBytes` function with `eitherDeserialiseFromRawBytes`
  ([PR 4626](https://github.com/input-output-hk/cardano-node/pull/4626))

- New `deserialiseFromTextEnvelopeCddlAnyOf` function
  ([PR 4625](https://github.com/input-output-hk/cardano-node/pull/4625))

- ** Breaking ** Replace `NotScriptLockedTxInsError` type with `ScriptLockedTxInsError`
  ([PR 4484](https://github.com/input-output-hk/cardano-node/pull/4484))

- Separate validation and creation of transaction bodies
  ([PR 4468](https://github.com/input-output-hk/cardano-node/pull/4468))

- New `QueryPoolDistribution` query
  ([PR 4250](https://github.com/input-output-hk/cardano-node/pull/4250))

- More efficient `ToJSON` instances that make use of `toEncoding` for streaming.
  ([PR 4205](https://github.com/input-output-hk/cardano-node/pull/4205))

- Expose `AcquireFailure` and `SystemStart` from `Cardano.Api.Shelley`
  ([PR 4199](https://github.com/input-output-hk/cardano-node/pull/4199))

- Update `makeTransactionBodyAutoBalance` function to automatically calculate the total and return collateral values
  ([PR 4198](https://github.com/input-output-hk/cardano-node/pull/4198))

- New `QueryPoolState` query
  ([PR 4170](https://github.com/input-output-hk/cardano-node/pull/4170))

- Add `utxoCostPerByte` protocol parameter
  ([PR 4141](https://github.com/input-output-hk/cardano-node/pull/4141))

- Expose `Key` interface in `Cardano.Api.Shelley`
  ([PR 4048](https://github.com/input-output-hk/cardano-node/pull/4048))

- New `generateInsecureSigningKey` function
  ([PR 4021](https://github.com/input-output-hk/cardano-node/pull/4021))

- SPO on-chain poll commands adjustments
  ([PR 5132](https://github.com/input-output-hk/cardano-node/pull/5132))

- UTC Time to slots conversion function
  ([PR 5130](https://github.com/input-output-hk/cardano-node/pull/5130))

- Add new interim governance commands: {create, answer, verify}-poll
  ([PR 5112](https://github.com/input-output-hk/cardano-node/pull/5112))

- CIP-1694 make space for DRep certificates
  ([PR 5108](https://github.com/input-output-hk/cardano-node/pull/5108))

- File type to track the content and direction of files
  ([PR 5105](https://github.com/input-output-hk/cardano-node/pull/5105))

- Expose UsingRawBytes et al types
  ([PR 5086](https://github.com/input-output-hk/cardano-node/pull/5086))

- Expose SerialiseAsRawBytesError in Cardano.Api
  ([PR 5085](https://github.com/input-output-hk/cardano-node/pull/5085))

- New genCardanoKeyWitness function
  ([PR 5071](https://github.com/input-output-hk/cardano-node/pull/5071))

- Replace roundtripCBOR with trippingCbor
  ([PR 5069](https://github.com/input-output-hk/cardano-node/pull/5069))

- Remove non-round-trippable value TxInsReferenceNone for babbage onwards in generator
  ([PR 5064](https://github.com/input-output-hk/cardano-node/pull/5064))

- Improve roundtrip functions to report annotations on callsite
  ([PR 5063](https://github.com/input-output-hk/cardano-node/pull/5063))

- Define Functor instance for FileError
  ([PR 5057](https://github.com/input-output-hk/cardano-node/pull/5057))

- Script data serialisation
  ([PR 5002](https://github.com/input-output-hk/cardano-node/pull/5002))

- Add LedgerStateBabbage and LedgerStateConway pattern synonyms
  ([PR 5001](https://github.com/input-output-hk/cardano-node/pull/5001))

- Conway hard forks on prot-ver 9
  ([PR 4988](https://github.com/input-output-hk/cardano-node/pull/4988))

- Guard against overflows in Shelley TxIns
  ([PR 4956](https://github.com/input-output-hk/cardano-node/pull/4956))

- Remove duplicate scripts when building transaction body for Mary, Alonzo and Babbage
  ([PR 4953](https://github.com/input-output-hk/cardano-node/pull/4953))

- Combinators for TxBodyContent and related types
  ([PR 4941](https://github.com/input-output-hk/cardano-node/pull/4941))

- Preserve ScriptData bytes fix
  ([PR 4926](https://github.com/input-output-hk/cardano-node/pull/4926))

- Detect invalid counter and certificate
  ([PR 4880](https://github.com/input-output-hk/cardano-node/pull/4880))

- Implement ADR-2: Restructure modules for generators
  ([PR 4833](https://github.com/input-output-hk/cardano-node/pull/4833))

- New NodeToClientVersionOf typeclass
  ([PR 4787](https://github.com/input-output-hk/cardano-node/pull/4787))

- Implement signArbitraryBytesKes for use in Mithril
  ([PR 4779](https://github.com/input-output-hk/cardano-node/pull/4779))

- Export SubmitResult from Cardano.Api
  ([PR 4753](https://github.com/input-output-hk/cardano-node/pull/4753))

- Add support for ghc-9.2 and partial support for CHaP
  ([PR 4701](https://github.com/input-output-hk/cardano-node/pull/4701))

- Append tx output in cli transaction build command
  ([PR 4696](https://github.com/input-output-hk/cardano-node/pull/4696))

- Add ToJSON/FromJSON instances for ChainPoint
  ([PR 4686](https://github.com/input-output-hk/cardano-node/pull/4686))

- Add an Ord ChainPoint instance
  ([PR 4685](https://github.com/input-output-hk/cardano-node/pull/4685))

- Derive Eq instance for AcquiringFailure
  ([PR 4683](https://github.com/input-output-hk/cardano-node/pull/4683))

- Export `fromShelleyBasedScript` from Cardano.Api
  ([PR 4682](https://github.com/input-output-hk/cardano-node/pull/4682))

- Expose TextEnvelopeCddl from Cardano.Api
  ([PR 4635](https://github.com/input-output-hk/cardano-node/pull/4635))

- Expose txScriptValidityToScriptValidity in Cardano.Api
  ([PR 4628](https://github.com/input-output-hk/cardano-node/pull/4628))

- Cardano Node 1.35.6 aka Single Relay P2P release
  ([PR 4612](https://github.com/input-output-hk/cardano-node/pull/4612))

- Update ouroboros-network and cardano-ledger dependencies
  ([PR 4608](https://github.com/input-output-hk/cardano-node/pull/4608))

- export RawBytesHexError
  ([PR 4599](https://github.com/input-output-hk/cardano-node/pull/4599))

- Module reshuffle
  ([PR 4593](https://github.com/input-output-hk/cardano-node/pull/4593))

- Add Ord instance for AddressInEra
  ([PR 4587](https://github.com/input-output-hk/cardano-node/pull/4587))

- Add ToJSON and FromJSON instances for Address
  ([PR 4568](https://github.com/input-output-hk/cardano-node/pull/4568))

- Export TxIns type alias
  ([PR 4565](https://github.com/input-output-hk/cardano-node/pull/4565))

- Export IsPlutusScriptLanguage
  ([PR 4554](https://github.com/input-output-hk/cardano-node/pull/4554))

- Export more generators
  ([PR 4534](https://github.com/input-output-hk/cardano-node/pull/4534))

- Condense Read and Validation modules in cardano-cli
  ([PR 4516](https://github.com/input-output-hk/cardano-node/pull/4516))

- Export TxTotalAndReturnCollateralSupportedInEra from Cardano.Api
  ([PR 4496](https://github.com/input-output-hk/cardano-node/pull/4496))

- Export `LocalTxSubmissionClient` data constructor
  ([PR 5096](https://github.com/input-output-hk/cardano-node/pull/5096))

### Bugs

- Fix: Add `AStakeExtendedVerificationKey` back into `deserialiseAnyVerificationKeyTextEnvelope`
  ([PR 4918](https://github.com/input-output-hk/cardano-node/pull/4918))

- Fix: Re-add `AGenesisExtendedVerificationKey` back into `deserialiseAnyVerificationKeyTextEnvelope`
  ([PR 4894](https://github.com/input-output-hk/cardano-node/pull/4894))

- Allow reading text envelopes from pipes ([PR 4384](https://github.com/input-output-hk/cardano-node/pull/4384))

- Fix 4493 bug - TxWitness text envelope format does not roundtrip in Shelley era
  ([PR 4501](https://github.com/input-output-hk/cardano-node/pull/4501))

- Fix minUTxO calculation in `calculateMinimumUTxO` function in `cardano-api`
  ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

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
