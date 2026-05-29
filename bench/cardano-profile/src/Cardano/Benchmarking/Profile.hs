{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile
  ( addEras
  , realize
  ) where

--------------------------------------------------------------------------------

import           Prelude
import           Control.Monad (foldM)
import           Data.Maybe (catMaybes, fromMaybe)
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Stack (HasCallStack)
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: containers.
import qualified Data.Map.Strict as Map
-- Package: text.
import qualified Data.Text            as Text
-- Package: scientific.
import qualified Data.Scientific as Scientific
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Genesis as Genesis
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Paths_cardano_profile as Paths

--------------------------------------------------------------------------------

realize :: HasCallStack => Types.Profile -> Types.Profile
realize =
    -- Compose the profile in the same order as the `jq` profile machinery!
    -- 1) `addUnusedDefaults`: Adds all properties that are the same for all
    --                         profiles. This are all candidates to be removed
    --                         when we finally switch from `jq` to this.
    -- 2) `shelleyAlonzoConway`: Given an epoch number ("pparamsEpoch" property)
    --                           creates the "genesis" property using
    --                           "epoch-timeline.json" and applying the genesis
    --                           specific overlays ("pparamsOverlays" property).
    -- 3) `overlay`: Applies an optional JSON object as an "overlay". The object
    --               is read from an envar ("WB_PROFILE_OVERLAY") in the `main`
    --               function and can override anything (some may be overridden
    --               by later steps) as long as the result is a valid `Profile`.
    -- 4) `derive`: Fills the "derive" property.
    -- 5) `finalize`: Applies fixes (porting infelicities) needed to fill the
    --                "cli_args" property that is also filled here.
    -- 6) `presets`: A special case of `overlay` above. The JSON file to apply
    --               as an overlay has its name defined in the "preset"
    --               property. This file has to be defined in the Cabal file.
    preset
  . finalize
  . derive
  . overlay
  . shelleyAlonzoConway
  . addUnusedDefaults

{-

When defining profiles try keeping the same order as in the `Profile` type:
- Name and description
- Scenario
- Composition
- Era and genesis
- Node
- Generator
- Tracer
- Cluster
- Analysis

And common modifiers `P.traceForwardingOn . P.newTracing`
next to the name.

-}

-- Step 1.
--------------------------------------------------------------------------------

addUnusedDefaults :: Types.Profile -> Types.Profile
addUnusedDefaults p =
  p { Types.genesis =
        let genesis = Types.genesis p
        in  genesis {
              Types.network_magic = 42
            -- TODO: Remove property?
            , Types.single_shot = True
            }
    , Types.generator =
        let generator = Types.generator p
        in  generator {
              -- TODO: Remove property?
              Types.add_tx_size = 100
            }
    , Types.analysis =
        let analysis = Types.analysis p
        in  analysis {
              Types.cluster_base_startup_overhead_s = 40
            , Types.start_log_spread_s = 120
            , Types.last_log_spread_s = 120
            , Types.silence_since_last_block_s = 120
            , Types.tx_loss_ratio = 0.02
            , Types.finish_patience = 21
            }
    }

-- Step 2.
--------------------------------------------------------------------------------

getEpochNumber :: HasCallStack => Types.Profile -> Integer
getEpochNumber profile =
  let number = Types.pparamsEpoch $ Types.genesis profile
  in if number <= 0
     then error $    "Profile \"" ++ Types.name profile
                  ++ "\" has epoch number = " ++ show number
     else number

-- The genesis overlay files are applied to the "genesis" property and the ones
-- available are defined as functions in `Primitives`.
getOverlay :: String -> IO Genesis.Epoch
getOverlay overlayName = do
  let dataFileName = "data/genesis/overlays/" ++ overlayName ++ ".json"
  fp <- Paths.getDataFileName dataFileName
  eitherValue <- Aeson.eitherDecodeFileStrict fp
  return $ case eitherValue of
    (Right epoch) -> epoch
    (Left e) -> error $ "\"" ++ fp ++ "\": " ++ e

-- | Fill the "genesis" object "shelley", "alonzo" and "conway" properties using
--   the profile's epoch number and overlay names. Creates the merged Epoch once
--   (timeline + overlays + profile-supplied params) and then completes two
--   sub-steps that share that Epoch:
--     2a) `genesisParams`:     protocol parameters (shelley / alonzo / conway).
--     2b) `genesisCostModels`: Plutus cost-model placements on top.
shelleyAlonzoConway :: Types.Profile -> Types.Profile
shelleyAlonzoConway profile =
  let epochNumber  = getEpochNumber profile
      -- Collects all the genesis properties from "epoch-timeline.json".
      epoch  = unsafePerformIO $ Genesis.epochTimeline epochNumber
      -- Apply the genesis overlays ("pparamsOverlays") to it.
      epoch' = unsafePerformIO $ foldM
        (\acc overlayName -> do
                 overlayEpoch <- getOverlay overlayName
                 -- Right-biased merge of both JSON objects at all depths.
                 return $ acc <> overlayEpoch
        )
        epoch
        (Types.pparamsOverlays $ Types.genesis profile)
      -- Properties set by the user in the profile take precedence.
      epoch'' =    epoch'
                <> Genesis.Epoch
                     (Genesis.EpochParams
                       (Just $ Types.shelley $ Types.genesis profile)
                       (Just $ Types.alonzo  $ Types.genesis profile)
                       (       Types.conway  $ Types.genesis profile)
                     )
                     -- Cost models don't come from the profile, only from
                     -- epochTimeline + overlays, they contribute nothing here.
                     mempty
      -- Both sub-steps consume the same merged Epoch.
      g = genesisCostModels (Genesis.cost_model epoch'')
        $ genesisParams (Genesis.epoch_params epoch'') (Types.genesis profile)
  in profile { Types.genesis = g { Types.pparamsEpoch = epochNumber } }

-- Step 2a.
--------------------------------------------------------------------------------

-- | Fill "shelley", "alonzo" and "conway" on the Genesis with the
--   protocol-parameter side of the merged Epoch from `shelleyAlonzoConway`.
--   Cost models are NOT touched here, see `genesisCostModels` (Step 2b).
genesisParams :: Genesis.EpochParams -> Types.Genesis -> Types.Genesis
genesisParams epochParams g =
  g { Types.shelley =
        case Genesis.shelley epochParams of
          (Just sheyKeyMap) -> foldl
            (\acc (k,v) -> KeyMap.insert k v acc)
            sheyKeyMap
            [
            -- We have to add "slotLength", "epochLength", "securityParam" and
            -- "activeSlotsCoeff" that are treated as first class citizens in
            -- this library, instead of JSON/KeyMap.
              ("slotLength",       Aeson.Number $ realToFrac  $ Types.slot_duration g)
            , ("epochLength",      Aeson.Number $ fromInteger $ Types.epoch_length g)
            , ("securityParam",    Aeson.Number $ fromInteger $ Types.parameter_k g)
            , ("activeSlotsCoeff", Aeson.Number $ Types.active_slots_coeff g)
            ]
          Nothing -> error "No \"shelley\" JSON object from epoch-timeline.json"
    , Types.alonzo =
        case Genesis.alonzo epochParams of
          (Just a) -> a
          Nothing  -> error "No \"alonzo\" JSON object from epoch-timeline.json"
    , Types.conway = Genesis.conway epochParams
    }

-- Step 2b.
--------------------------------------------------------------------------------

-- | Write the Plutus cost models into the Genesis's "alonzo" and "conway".
--   Cost models can land in three fields of the generated genesis JSON:
--     * `alonzo.costModels`:             mainnet alonzo-genesis shape.
--     * `conway.plutusV3CostModel`:      mainnet conway-genesis shape.
--     * `alonzo.extraConfig.costModels`: testnet/benchmark override.
--                                        Not present on mainnet.
--   The two mainnet top-level fields have strict parsers that lock the
--   STRUCTURE to what mainnet shipped with at each era's hard fork: their
--   values are free to set, but the entry COUNT (and for V3, also the FORM)
--   cannot change. Specifically:
--     * `alonzo.costModels` accepts PlutusV1 only, exactly 166 entries,
--       in either object or array form (online mainnet file uses an object)
--       (since IntersectMBO/cardano-ledger#5379 restricted to PlutusV1).
--     * `conway.plutusV3CostModel` is array-only, exactly 251 entries
--       (since IntersectMBO/cardano-ledger#5241 enforced the count there).
--   Any cost-parameter ADDITIONS subsequent hard forks made on mainnet (the 10
--   V2 entries added at Chang, the 46 V3 entries added at epoch 537) are
--   applied internally by the hard-fork combinator based on the protocol
--   version, not by extending those genesis fields.
--   `alonzo.extraConfig.costModels` was added to bypass those structural locks
--   for testnets and benchmarks (IntersectMBO/cardano-ledger#5342, landed in
--   IntersectMBO/cardano-ledger#5379; surfaced for cardano-cli in
--   IntersectMBO/cardano-cli#1352): it accepts V1/V2/V3 keys and skips the
--   exact-count check the top-level parsers enforce. Its object form is
--   still capped at each language's init names (extras are silently dropped
--   at parse time), so the ARRAY form is the only one that actually carries
--   entries past those counts, and that is what we emit there.
--   `alonzoInjectCostModels` applies the content as a per-language
--   REPLACEMENT at the Alonzo era transition.
--
--   Expects `genesisParams` (Step 2a) to have already populated the base
--   "alonzo" and "conway" KeyMaps on the Genesis.
genesisCostModels :: Genesis.CostModel -> Types.Genesis -> Types.Genesis
genesisCostModels costModel g =
  let
      -- Cardano-ledger's canonical init param counts: the number of named cost
      -- parameters each language had when first introduced in its genesis file
      -- (see libs/cardano-ledger-core/.../CostModels.hs:231-239).
      -- The strict TOP-LEVEL parsers (V1 at `alonzo.costModels`, V3 at
      -- `conway.plutusV3CostModel`) require exactly this count.
      -- The lenient `alonzo.extraConfig.costModels` parser does NOT validate
      -- the input length in either form. In OBJECT form it walks the first N
      -- canonical init names and silently ignores any extra keys, so only
      -- those N entries ever reach runtime. In ARRAY form length validation
      -- is deferred to Plutus's `mkEvaluationContext` downstream. The array
      -- form at extraConfig is therefore the only path that can actually
      -- carry entries past this count to runtime.
      v1InitCount = 166 :: Int
      v2InitCount = 175 :: Int
      v3InitCount = 251 :: Int
      -- PlutusV1.
      -- Introduced: Alonzo hard fork (Alonzo era, epoch 290 mainnet) as an
      -- OBJECT of 166 named cost parameters at `alonzo.costModels.PlutusV1` in
      -- alonzo-genesis.json.
      --
      -- Updated: Vasil (epoch 366) tweaked 100 entries; Valentine (394) tweaked
      -- 2; Chang (507) tweaked 85. V1 has never gained a parameter, total is
      -- still 166.
      --
      -- Placement: We emit the first 166 canonical entries as the OBJECT at
      -- `alonzo.costModels.PlutusV1` (mainnet shape). If the input ever carries
      -- more than 166 entries (future proofing), the FULL set also goes as an
      -- ARRAY to `alonzo.extraConfig.costModels.PlutusV1`; the lenient parser
      -- defers array length validation to Plutus's mkEvaluationContext.
      --
      -- Naming: If any of the extras have a name not in `plutusV1CostNames`,
      -- the list throws an error (on purpose), signalling that the workbench's
      -- name list must be extended.
      (v1AtTopLevel, v1AtExtra) =
        let v1Full   = fromMaybe KeyMap.empty (Genesis.plutusV1 costModel)
            n        = KeyMap.size v1Full
            val name = case KeyMap.lookup (Key.fromString name) v1Full of
              Just v  -> v
              Nothing -> error $ "PlutusV1 cost model missing canonical entry: " ++ name
        in if n == 0
           then (Nothing, Nothing)
           else if n < v1InitCount
                then error $    "PlutusV1 cost model has " ++ show n
                             ++ " entries; need at least " ++ show v1InitCount
                else let first166 = KeyMap.fromList
                           [ (Key.fromString name, val name)
                           | name <- take v1InitCount Genesis.plutusV1CostNames
                           ]
                         extra    = if n > v1InitCount
                                    then Just $ Aeson.toJSON
                                           [ val name
                                           | name <- take n Genesis.plutusV1CostNames
                                           ]
                                    else Nothing
                     in (Just (Aeson.Object first166), extra)
      -- PlutusV2.
      -- Introduced: Vasil hard fork (Babbage era, epoch 366 mainnet) with
      -- 175 named cost parameters.
      --
      -- Updated: Valentine (epoch 394) tweaked 7 entries; Chang (epoch 507)
      -- ADDED 10 new and tweaked 90 (current total 185).
      --
      -- Placement: V2 has no top-level home (`alonzo.costModels` is V1-only),
      -- so its only path to runtime is `alonzo.extraConfig.costModels.PlutusV2`.
      -- We emit the FULL V2 set as an ARRAY (canonical order); the lenient
      -- parser defers array length validation to Plutus's mkEvaluationContext.
      --
      -- Naming: If any of the entries have a name not in `plutusV2CostNames`,
      -- the list throws an error (on purpose), signalling that the workbench's
      -- name list must be extended.
      v2AtExtra =
        let v2Full   = fromMaybe KeyMap.empty (Genesis.plutusV2 costModel)
            n        = KeyMap.size v2Full
            val name = case KeyMap.lookup (Key.fromString name) v2Full of
              Just v  -> v
              Nothing -> error $ "PlutusV2 cost model missing canonical entry: " ++ name
        in if n == 0
           then Nothing
           else if n < v2InitCount
                then error $    "PlutusV2 cost model has " ++ show n
                             ++ " entries; need at least " ++ show v2InitCount
                else Just $ Aeson.toJSON
                       [ val name
                       | name <- take n Genesis.plutusV2CostNames
                       ]
      -- PlutusV3.
      -- Introduced: Chang hard fork (Conway era, epoch 507 mainnet) as an
      -- ARRAY of 251 cost parameters at `conway.plutusV3CostModel` in
      -- conway-genesis.json.
      --
      -- Updated: at epoch 537 an on-chain parameter update ADDED 46 new entries
      -- (current total 297).
      --
      -- Placement: We emit the canonical first 251 entries as the ARRAY at
      -- `conway.plutusV3CostModel` (mainnet shape). When the input carries
      -- more than 251 entries, the FULL set ALSO goes as an ARRAY to
      -- `alonzo.extraConfig.costModels.PlutusV3`; the lenient parser defers
      -- array length validation to Plutus's mkEvaluationContext.
      --
      -- Naming: If any of the entries have a name not in `plutusV3CostNames`,
      -- the list throws an error (on purpose), signalling that the workbench's
      -- name list must be extended.
      (v3AtTopLevel, v3AtExtra) =
        let v3Full   = fromMaybe KeyMap.empty (Genesis.plutusV3 costModel)
            n        = KeyMap.size v3Full
            val name = case KeyMap.lookup (Key.fromString name) v3Full of
              Just v  -> v
              Nothing -> error $ "PlutusV3 cost model missing canonical entry: " ++ name
        in if n == 0
           then (Nothing, Nothing)
           else if n < v3InitCount
                then error $    "PlutusV3 cost model has " ++ show n
                             ++ " entries; need at least " ++ show v3InitCount
                else let full     = [ val name
                                    | name <- take n Genesis.plutusV3CostNames
                                    ]
                         topLevel = Aeson.toJSON (take v3InitCount full)
                         extra    = if n > v3InitCount
                                    then Just (Aeson.toJSON full)
                                    else Nothing
                     in (Just topLevel, extra)
      -- alonzo.extraConfig.costModels content: a PlutusV* sub-key is
      -- included only when its version yielded a Just above.
      extraEntries = catMaybes
        [ (,) "PlutusV1" <$> v1AtExtra
        , (,) "PlutusV2" <$> v2AtExtra
        , (,) "PlutusV3" <$> v3AtExtra
        ]
      mExtraConfig =
        if null extraEntries
        then Nothing
        else Just $ Aeson.Object $
               KeyMap.singleton "costModels"
                                (Aeson.Object (KeyMap.fromList extraEntries))
  in g {
      Types.alonzo =
        let alzoBase = Types.alonzo g
            withCM = case v1AtTopLevel of
              Nothing -> alzoBase
              Just o  -> KeyMap.insert "costModels"
                                       (Aeson.Object (KeyMap.singleton "PlutusV1" o))
                                       alzoBase
            withExtra = case mExtraConfig of
              Nothing -> withCM
              Just ec -> KeyMap.insert "extraConfig" ec withCM
        in withExtra
    , Types.conway =
        case (Types.conway g, v3AtTopLevel) of
          (Nothing, Nothing)  -> Nothing
          (Just c,  Nothing)  -> Just c
          (Nothing, Just arr) -> Just $ KeyMap.singleton "plutusV3CostModel" arr
          (Just c,  Just arr) -> Just $ KeyMap.insert    "plutusV3CostModel" arr c
    }

-- Step 3.
--------------------------------------------------------------------------------

-- Merges the profile with a JSON object stored in the "overlay" property.
overlay :: HasCallStack => Types.Profile -> Types.Profile
overlay profile =
  let overlaykeyMap = Types.overlay profile -- An `Aeson.Object`.
  in if overlaykeyMap /= mempty
     then applyOverlay overlaykeyMap profile
     else profile

-- Step 4.
--------------------------------------------------------------------------------

-- Fills the "derive" property.
-- "derive" needs above "shelley", "alonzo" and "conway" properties.
derive :: Types.Profile -> Types.Profile
derive p@(Types.Profile _ _ _ comp _era gsis _ n gtor _ _ _ ana _ _ _ _) =
  let 
      -- Absolute/epoch durations:
      ----------------------------
      slot_duration      = Types.slot_duration gsis -- NominalDiffTime
      slots_per_epoch    = Types.epoch_length  gsis -- Integer
      -- Derived NominalDiffTime.
      epoch_duration     = fromInteger slots_per_epoch * slot_duration

      -- Block/probable durations:
      ----------------------------
      active_slots_coeff = Types.active_slots_coeff gsis -- Scientific
      -- Helper Scientific.
      block_per_epoch = fromInteger slots_per_epoch * active_slots_coeff
      -- Helper NominalDiffTime.
      _block_duration = slot_duration / realToFrac active_slots_coeff

      -- Effective durations:
      -----------------------
      -- If the nodes have a "--shutdown-on-*" parameter we use this to
      -- calculate the epochs of the run, if there is none, we use the epochs
      -- from the generator config.
      -- Force it to a Double or get an error:
      -- -- `fromRational` has been applied to a repeating decimal
      requested_epochs = -- A Double.
        case Types.shutdown_on_slot_synced n of
          (Just shutdown_slots) ->
            fromInteger shutdown_slots / fromInteger slots_per_epoch
          Nothing -> case Types.shutdown_on_block_synced n of
            (Just shutdown_blocks) ->
              fromInteger shutdown_blocks / realToFrac block_per_epoch
            -- If it does not come from the node, it comes from the generator.
            Nothing -> fromInteger $ Types.epochs gtor
      -- Derived Integer.
      -- 1.0001 epochs is a 2 epochs profile!
      requested_epochs_ceiling = ceiling (requested_epochs :: Double)

      -- Generator:
      -------------
      tps_ = Types.tps gtor -- Scientific
      generator_requested_duration = -- NominalDiffTime
        epoch_duration * fromInteger requested_epochs_ceiling
      -- Helpers NominalDiffTime.
      (generator_duration, maybe_shutdown_time) =
        case Types.shutdown_on_slot_synced n of
          (Just shutdown_slots) ->
            let shutdown_time = fromInteger shutdown_slots * slot_duration
            in (min generator_requested_duration shutdown_time, Just shutdown_time)
          Nothing -> (generator_requested_duration, Nothing)
      -- Integer.
      generator_tx_count = ceiling $ case Types.tx_count gtor of
        Just tx_count -> fromInteger tx_count
        Nothing -> realToFrac generator_duration * tps_

      -- UTxO:
      --------
      (effective_delegators, delegators_effective) =
        let d = Types.delegators gsis
        in (d, max d (Types.n_pools comp))
      utxo_generated = generator_tx_count * Types.inputs_per_tx gtor
      utxo_stuffed = max 0 (Types.utxo gsis)

      -- Dataset:
      -----------
      dataset_measure =
        if Types.utxo gsis == 0
        then 0
        else Types.utxo gsis + Types.delegators gsis
      -- NominalDiffTime.
      dataset_induced_startup_delay_optimistic =
        if dataset_measure < 10000
        then Types.cluster_base_startup_overhead_s ana
        else fromInteger dataset_measure / 50000
      -- NominalDiffTime
      dataset_induced_startup_delay_conservative =
        if dataset_measure < 10000
        then Types.cluster_base_startup_overhead_s ana
        else fromInteger dataset_measure / 2500
      -- NominalDiffTime.
      genesis_future_offset =
          dataset_induced_startup_delay_optimistic
        + Types.extra_future_offset gsis

        -- Supply:
        ----------
      supply_delegated = Types.per_pool_balance gsis * Types.n_pools comp
      supply_total = supply_delegated + Types.funds_balance gsis

      -- Size:
      --------
      -- genesis.shelley.protocolParams.maxBlockBodySize
      maxBlockBodySize =
        case KeyMap.lookup "protocolParams" (Types.shelley gsis) of
          (Just (Aeson.Object pparams)) ->
            case KeyMap.lookup "maxBlockBodySize" pparams of
              (Just (Aeson.Number scientific)) -> scientific
              _ -> error "No \"maxBlockBodySize\" Number"
          _ -> error "No \"shelley.protocolParams\" object"
      -- XXX:  this is corruption at the highest levels, pure and simple.
      default_value_tx_size_estimate = (381 :: Integer) -- Bytes?
      default_value_tx_per_block_estimate = floor $
        (realToFrac maxBlockBodySize :: Double) / fromInteger default_value_tx_size_estimate
      generator_blocks_lower_bound = ceiling $
        (1.15 :: Double) * fromInteger generator_tx_count / fromInteger default_value_tx_per_block_estimate

  in p {
       Types.derived = Types.Derived {
       -- Duration:
         Types.effective_epochs = requested_epochs_ceiling
       , Types.epoch_duration = epoch_duration
       -- Data set:
       , Types.dataset_measure = dataset_measure
       , Types.delegators_effective = delegators_effective
       , Types.genesis_future_offset = genesis_future_offset

       , Types.generator_duration = generator_duration
       , Types.generator_tx_count = generator_tx_count

       , Types.supply_total = supply_total
       , Types.supply_delegated = supply_delegated

       , Types.utxo_delegated = effective_delegators
       , Types.utxo_generated = utxo_generated
       , Types.utxo_stuffed = utxo_stuffed

       , Types.shutdown_time = maybe_shutdown_time
       -- XXX:  this is corruption at the highest levels, pure and simple.
       , Types.default_value_tx_size_estimate = default_value_tx_size_estimate
       , Types.default_value_tx_per_block_estimate = default_value_tx_per_block_estimate
       , Types.generator_blocks_lower_bound = generator_blocks_lower_bound

       , Types.dataset_induced_startup_delay_optimistic = dataset_induced_startup_delay_optimistic
       , Types.dataset_induced_startup_delay_conservative = dataset_induced_startup_delay_conservative
       }
     }

-- Step 5.
--------------------------------------------------------------------------------

-- Many fixes to be able to call `cliArgs`.
finalize :: Types.Profile -> Types.Profile
finalize profile =
  let
    -- Third, things not in "derived" that can't be a default.
    profile' =
      (\p -> p {
          -- Genesis "fixes".
          Types.genesis =
            let genesis = Types.genesis p
            in  genesis {
                   -- TODO: Remove or move to derive ?
                  Types.pool_coin =
                    if Types.n_pools (Types.composition p) == 0
                    then 0
                    else Types.per_pool_balance genesis
                  -- TODO: Remove or move to derive ?
                , Types.delegator_coin =
                    if Types.delegators genesis == 0
                    then 0
                    else Types.per_pool_balance genesis
                }
          -- Generator "fixes".
        , Types.generator = (Types.generator p) {
            -- TODO: Remove or move to derive ?
            Types.tx_count = Just $ Types.generator_tx_count $ Types.derived p
          -- Analysis "fixes".
        }
          -- Analysis "fixes".
        , Types.analysis = (Types.analysis p) {
            -- TODO: These two were set when constructing "derive".
            --       Remove from "analysis" and add to "derive" ???
            Types.minimum_chain_density =
              Types.active_slots_coeff (Types.genesis p) * 0.5
          , Types.cluster_startup_overhead_s =
              Types.dataset_induced_startup_delay_conservative (Types.derived p)
        }
      })
      profile
    -- The "cli_args" property need the "derived" property and above fixes.
    profile'' = cliArgs profile'
  in profile''

cliArgs :: Types.Profile -> Types.Profile
cliArgs p@(Types.Profile _ _ _ comp __ gsis _ _ _ _ _ _ _ dved _ _ _) =
  let --toJson = map (\(k,n) -> )
      fmtDecimal i =
           Scientific.formatScientific Scientific.Fixed (Just 0) (fromInteger i / 100000)
        ++ "00000"
      createTestnetDataArgs =
        [
          Aeson.String "--testnet-magic",    Aeson.Number $ fromInteger $ Types.network_magic gsis
        , Aeson.String "--total-supply",     Aeson.String $ Text.pack $ fmtDecimal $ Types.funds_balance gsis + Types.supply_delegated dved
        , Aeson.String "--utxo-keys",        Aeson.Number $ fromInteger $ Types.utxo_keys gsis
        , Aeson.String "--genesis-keys",     Aeson.Number $ fromInteger $ Types.n_bft_hosts comp
        , Aeson.String "--delegated-supply", Aeson.String $ Text.pack $ fmtDecimal $ Types.supply_delegated dved
        , Aeson.String "--pools",            Aeson.Number $ fromInteger $ Types.n_pools comp
        , Aeson.String "--stake-delegators", Aeson.Number $ fromInteger $ Types.delegators_effective dved
        , Aeson.String "--drep-keys",        Aeson.Number $ fromInteger $ Types.dreps gsis
        , Aeson.String "--stuffed-utxo",     Aeson.String $ Text.pack $ fmtDecimal $ Types.utxo_stuffed dved
        ]
      poolsArgs =
        [
          Aeson.String "--argjson"
        , Aeson.String "initialPoolCoin",    Aeson.String $ Text.pack $ fmtDecimal $ Types.pool_coin gsis
        ]
  in  p {Types.cli_args = Types.CliArgs {
            Types.createTestnetDataArgs = createTestnetDataArgs
          , Types.pools = poolsArgs
        }}

-- Step 6.
--------------------------------------------------------------------------------

-- Unlike `overlay` the preset content is not stored in the profile, the name of
-- the already part of the profile.
preset :: HasCallStack => Types.Profile -> Types.Profile
preset profile =
  case Types.preset profile of
    Nothing -> profile
    (Just presetName) -> unsafePerformIO $ do
      let dataFileName = "data/presets/" ++ presetName ++ ".json"
      fp <- Paths.getDataFileName dataFileName
      eitherValue <- Aeson.eitherDecodeFileStrict fp
      case eitherValue of
        (Right value) -> return $ applyOverlay value profile
        _ -> error $ "Not an Aeson Object: \"" ++ dataFileName ++ "\""

-- Merge (overlay / preset) utils.
--------------------------------------------------------------------------------

applyOverlay :: HasCallStack => Aeson.Object -> Types.Profile -> Types.Profile
applyOverlay overlaykeyMap profile =
  let profileKeyMap = case Aeson.toJSON profile of -- toJson, an Aeson.Value
                        (Aeson.Object keyMap) -> keyMap
                        _ -> error "What have you done?"
      union = KeyMap.unionWithKey unionWithKey profileKeyMap overlaykeyMap
  in case Aeson.fromJSON (Aeson.Object union) of
    -- Add the overlay to the profile.
    (Aeson.Success profile') -> profile'
    (Aeson.Error str) -> error $ "Could not apply overlay: " ++ str

-- Right-biased merge of both JSON objects at all depths.
unionWithKey :: KeyMap.Key -> Aeson.Value -> Aeson.Value -> Aeson.Value
-- Recurse if it's an object.
unionWithKey _ (Aeson.Object a) (Aeson.Object b) =
  Aeson.Object $ KeyMap.unionWithKey unionWithKey a b
-- If not an object prefer the right value.
unionWithKey _ _ b = b

-- Post-processing
--------------------------------------------------------------------------------

-- | Specialize profile to all valid eras and add era suffix(es) to profile name.
--   An era is considered valid based on the protocol version a profile might define.
addEras :: Map.Map String Types.Profile -> Map.Map String Types.Profile
addEras = foldMap
  (\profile -> Map.fromList $
      catMaybes
        [ addEra profile Types.Shelley "shey"
        , addEra profile Types.Allegra "alra"
        , addEra profile Types.Mary    "mary"
        , addEra profile Types.Alonzo  "alzo"
        , addEra profile Types.Babbage "bage"
        , addEra profile Types.Conway  "coay"
        ]
  )

addEra :: Types.Profile -> Types.Era -> String -> Maybe (String, Types.Profile)
addEra p era suffix
  | Just (major, _) <- Types.profileProtocolVersion p
  , era < Types.firstEraForMajorVersion major
    = Nothing
  | otherwise
    = let name = Types.name p
          newName = name ++ "-" ++ suffix
      in Just (newName, p {Types.name = newName, Types.era = era})

