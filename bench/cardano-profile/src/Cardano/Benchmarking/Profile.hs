{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile (realize) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.List (sort)
import           Control.Monad (foldM)
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Stack (HasCallStack)
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: text.
import qualified Data.Text            as Text
-- Package: scientific.
import qualified Data.Scientific as Scientific
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Paths_cardano_profile as Paths

--------------------------------------------------------------------------------

realize :: HasCallStack => Types.Profile -> Types.Profile
realize =
    -- Compose the profile in the same order as the `jq` profile machinery!
    -- 1) `addUnusedDefaults`: Adds all properties that are the same for all
    --                         profiles. This are all candidates to be removed
    --                         when we finally switch from `jq` to this.
    -- 2) `shelleyAlonzoConway`: Given an epoch number ("pparamsEpoch"
    --                           property) creates the "genesis" property
    --                           using "epoch-timeline.json" and applying the
    --                           genesis specific overlays ("pparamsOverlays"
    --                           property).
    -- 3) `overlay`: Applies an optional JSON object as an "overlay". The
    --               object is read from an envar ("WB_PROFILE_OVERLAY") in
    --               the `main` function and can override anything (some may
    --               overridden by later steps) as long as the result is a
    --               valid `Profile`.
    -- 4)  `derive`: Fills the "derive" property.
    -- 5)  `finalize`: Applies fixes (porting infelicities) needed to fill
    --                 the "cli_args" property that is also filled here.
    -- 6) "presets": A special case of `overlay` above. The JSON file to apply
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

And common modifiers `P.traceForwardingOn  . P.newTracing . P.p2pOff . P.tracerRtview`
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

-- | Fill the "genesis" object "shelley", "alonzo" and "conway" properties
--   using the profile's epoch number and overlay names.
shelleyAlonzoConway :: Types.Profile -> Types.Profile
shelleyAlonzoConway profile =
  let epochNumber  = getEpochNumber profile
      epochParams  = unsafePerformIO $ epochTimeline epochNumber
      epochParams' = unsafePerformIO $ foldM
        (flip genesisOverlay)
        epochParams
        (Types.pparamsOverlays $ Types.genesis profile)
      genesis f p = p {Types.genesis = f (Types.genesis p)}
  in genesis (\g -> g {
      Types.pparamsEpoch = epochNumber
    , Types.shelley =
        let shey' = KeyMap.fromList [
               ("slotLength", Aeson.Number $ realToFrac $ Types.slot_duration g)
             , ("epochLength", Aeson.Number $ fromInteger $ Types.epoch_length g)
             , ("securityParam", Aeson.Number $ fromInteger $ Types.parameter_k g)
             , ("activeSlotsCoeff", Aeson.Number $ Types.active_slots_coeff g)
             , ("protocolParams",
                 case KeyMap.lookup "shelley" epochParams' of
                   (Just shey) -> shey
                   _ -> error "Obtained no \"shelley\" from epoch-timeline.json"
               )
             ]
        -- Any property that was set before by the user takes precedence.
        in KeyMap.unionWithKey unionWithKey shey' (Types.shelley g)
    , Types.alonzo  =
        case KeyMap.lookup "alonzo" epochParams' of
          (Just (Aeson.Object alzo)) -> alzo
          _ -> error "Obtained no \"alonzo\" from epoch-timeline.json"
    -- The only "optional" genesis property.
    , Types.conway  =
        case KeyMap.lookup "conway" epochParams' of
          (Just (Aeson.Object coay)) -> Just coay
          _ -> Nothing
    }) profile

getEpochNumber :: HasCallStack => Types.Profile -> Integer
getEpochNumber profile =
  let number = Types.pparamsEpoch $ Types.genesis profile
  in if number <= 0
     then error $ "Profile \"" ++ Types.name profile ++ "\" has epoch number = " ++ show number
     else number

-- Collects all properties from epochs lower or equal than the desired one.
epochTimeline :: Integer -> IO (KeyMap.KeyMap Aeson.Value)
epochTimeline epochNumber = do
  fp <- Paths.getDataFileName "data/genesis/epoch-timeline.json"
  eitherValue <- Aeson.eitherDecodeFileStrict fp
  return $ case eitherValue of
    (Right (Aeson.Object keyMap)) -> foldl
      (\acc key ->
        -- TODO: It will fail if the number used as string key is above 999.
        if key <= read ("\"" ++ show epochNumber ++ "\"")
        then case KeyMap.lookup key keyMap of
          (Just (Aeson.Object obj)) ->
            -- Right-biased merge of both JSON objects at all depths.
            KeyMap.unionWithKey unionWithKey acc obj
          _ -> error "Key not an Aeson Object: \"data/epoch-timeline.json\""
        else acc
      )
      mempty
      (sort $ KeyMap.keys keyMap)
    _ -> error "Not an Aeson Object: \"data/epoch-timeline.json\""

-- The genesis overlay files are applied to the "genesis" property and the ones
-- available are defined as functions in `Primitives`.
genesisOverlay :: String -> KeyMap.KeyMap Aeson.Value -> IO (KeyMap.KeyMap Aeson.Value)
genesisOverlay overlayName epochParams = do
  let dataFileName = "data/genesis/overlays/" ++ overlayName ++ ".json"
  fp <- Paths.getDataFileName dataFileName
  eitherValue <- Aeson.eitherDecodeFileStrict fp
  return $ case eitherValue of
    (Right (Aeson.Object keyMap)) ->
      -- Right-biased merge of both JSON objects at all depths.
      KeyMap.unionWithKey unionWithKey epochParams keyMap
    _ -> error $ "Not an Aeson Object: \"" ++ fp ++ "\""

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
      createStakedArgs =
        [
          Aeson.String "--testnet-magic",    Aeson.Number $ fromInteger $ Types.network_magic gsis
        , Aeson.String "--supply",           Aeson.String $ Text.pack $ fmtDecimal $ Types.funds_balance gsis
        , Aeson.String "--gen-utxo-keys",    Aeson.Number $ fromInteger $ Types.utxo_keys gsis
        , Aeson.String "--gen-genesis-keys", Aeson.Number $ fromInteger $ Types.n_bft_hosts comp
        , Aeson.String "--supply-delegated", Aeson.String $ Text.pack $ fmtDecimal $ Types.supply_delegated dved
        , Aeson.String "--gen-pools",        Aeson.Number $ fromInteger $ Types.n_pools comp
        , Aeson.String "--gen-stake-delegs", Aeson.Number $ fromInteger $ Types.delegators_effective dved
        , Aeson.String "--num-stuffed-utxo", Aeson.String $ Text.pack $ fmtDecimal $ Types.utxo_stuffed dved
        ]
        ++
        if Types.dense_pool_density comp /= 1
        then
          [ 
            Aeson.String "--bulk-pool-cred-files", Aeson.Number $ fromInteger $ Types.n_dense_hosts comp
          , Aeson.String "--bulk-pools-per-file",  Aeson.Number $ fromInteger $ Types.dense_pool_density comp
          ]
        else []
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
            Types.createStakedArgs = createStakedArgs
          , Types.createTestnetDataArgs = createTestnetDataArgs
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
