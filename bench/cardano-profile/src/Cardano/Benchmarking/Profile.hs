{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile (
  names, namesNoEra, namesCloudNoEra
, byName
, profiles
, libMk
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.List (sort)
import           Control.Monad (foldM)
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Stack (HasCallStack)
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: containers.
import qualified Data.Map.Strict as Map
-- Package: text.
import qualified Data.Text            as Text
-- Package: scientific.
import qualified Data.Scientific as Scientific
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Paths_cardano_profile as Paths
-- Profiles to export!
import           Cardano.Benchmarking.Profile.Builtin.Cloud               (profilesNoEraCloud)
import           Cardano.Benchmarking.Profile.Builtin.Empty               (profilesNoEraEmpty)
import           Cardano.Benchmarking.Profile.Builtin.ForgeStress         (profilesNoEraForgeStress)
import           Cardano.Benchmarking.Profile.Builtin.K3                  (profilesNoEraK3)
import           Cardano.Benchmarking.Profile.Builtin.Legacy.Dense        (profilesNoEraDense)
import           Cardano.Benchmarking.Profile.Builtin.Legacy.Dish         (profilesNoEraDish)
import           Cardano.Benchmarking.Profile.Builtin.Miniature           (profilesNoEraMiniature)
import           Cardano.Benchmarking.Profile.Builtin.Model               (profilesNoEraModel)
import           Cardano.Benchmarking.Profile.Builtin.Plutuscall          (profilesNoEraPlutuscall)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.Chainsync  (profilesNoEraChainsync)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.Idle       (profilesNoEraIdle)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.Latency    (profilesNoEraLatency)
import           Cardano.Benchmarking.Profile.Builtin.Scenario.TracerOnly (profilesNoEraTracerOnly)
import           Cardano.Benchmarking.Profile.Extra.Scaling               (profilesNoEraScalingLocal, profilesNoEraScalingCloud)

--------------------------------------------------------------------------------

names :: [String]
-- Overlay not supported here, using an empty overlay.
names = Map.keys (profiles mempty)

namesCloudNoEra :: [String]
-- Overlay not supported here, using an empty overlay.
namesCloudNoEra = map Types.name profilesNoEraCloud

-- Names:
-- wb profile all-profiles | jq .[] | jq -r .name | sort | uniq | grep "\-bage"
namesNoEra :: [String]
-- Overlay not supported here, using an empty overlay.
namesNoEra = Map.keys (profilesNoEra mempty)

byName :: String -> Aeson.Object -> Maybe Types.Profile
byName name obj =
  case Map.lookup name (profiles obj) of
    Nothing -> Nothing
    (Just profile) -> Just profile

--------------------------------------------------------------------------------

-- | Construct Map with profile name as key, without eras (in name and object).
profilesNoEra :: HasCallStack => Aeson.Object -> Map.Map String Types.Profile
profilesNoEra obj = Map.fromList $ map
  -- Convert to tuple and apply fixes, defaults and derive.
  (\p ->
    ( Types.name p
    , finalize (overlay obj (addUnusedDefaults p))
    )
  )
  -- All the "families" of profiles. Grouped by common properties or intentions.
  (
       profilesNoEraCloud
    ++ profilesNoEraEmpty            -- Empty datasets running `FixedLoaded`.
    ++ profilesNoEraForgeStress      -- All the "forge-stress*" profiles.
    ++ profilesNoEraK3               -- K3
    -- Legacy.
    ++ profilesNoEraDense
    ++ profilesNoEraDish
    ++ profilesNoEraMiniature
    ++ profilesNoEraModel            --
    ++ profilesNoEraPlutuscall       --
    -- Empty datasets not running `FixedLoaded`.
    ++ profilesNoEraChainsync        -- Scenario `Chainsync`
    ++ profilesNoEraIdle             -- Scenario `Idle`
    ++ profilesNoEraLatency          -- Scenario `Latency`
    ++ profilesNoEraTracerOnly       -- Scenario `TracerOnly`
    -- Extra modules
    ++ profilesNoEraScalingLocal
    ++ profilesNoEraScalingCloud
  )

-- | Adds the eras to `profilesNoEra`.
profiles :: Aeson.Object -> Map.Map String Types.Profile
profiles obj = foldMap
  (\profile -> Map.fromList $
    let
        addEra p era suffix =
          let name = Types.name p
              newName = name ++ "-" ++ suffix
          in  (newName, p {Types.name = newName, Types.era = era})
    in 
        [ addEra profile Types.Allegra "alra"
        , addEra profile Types.Shelley "shey"
        , addEra profile Types.Mary    "mary"
        , addEra profile Types.Alonzo  "alzo"
        , addEra profile Types.Babbage "bage"
        , addEra profile Types.Conway  "coay"
        ]
  )
  (profilesNoEra obj)

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

overlay :: HasCallStack => Aeson.Object -> Types.Profile -> Types.Profile
overlay overlaykeyMap profile =
      -- data Value = Object !Object
      -- type Object = KeyMap Value
  let profileKeyMap = case Aeson.toJSON profile of -- toJson, an Aeson.Value
                        (Aeson.Object keyMap) -> keyMap
                        _ -> error "What have you done?"
      union = KeyMap.unionWithKey unionWithKey profileKeyMap overlaykeyMap
  in case Aeson.fromJSON (Aeson.Object union) of
    -- Add the overlay to the profile.
    (Aeson.Success profile') -> profile' {Types.overlay = Just overlaykeyMap}
    (Aeson.Error str) -> error $ "Could not apply overlay: " ++ str

-- Right-biased merge of both JSON objects at all depths.
unionWithKey :: KeyMap.Key -> Aeson.Value -> Aeson.Value -> Aeson.Value
-- Recurse if it's an object.
unionWithKey _ (Aeson.Object a) (Aeson.Object b) =
  Aeson.Object $ KeyMap.unionWithKey unionWithKey a b
-- If not an object prefer the right value.
unionWithKey _ _ b = b

finalize :: Types.Profile -> Types.Profile
finalize profile =
  let
    -- First fill the genesis' "shelley", "alonzo" and "conway" properties
    -- using the provided epoch number and overlay names.
    profile' = shelleyAlonzoConway (getEpochNumber profile) profile
    -- Second fill the "derive" property.
    -- "derive" needs above "shelley", "alonzo" and "conway" properties.
    profile'' = derive profile'
    -- Third, things not in "derived" that can't be a default.
    profile''' =
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
                    if Types.delegators genesis == Just 0
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
      profile''
    -- The "cli_args" property need the "derived" property and above fixes.
    profile'''' = cliArgs profile'''
  in profile''''

getEpochNumber :: HasCallStack => Types.Profile -> Integer
getEpochNumber profile =
  let number = Types.pparamsEpoch $ Types.genesis profile
  in if number <= 0
     then error $ "Profile \"" ++ Types.name profile ++ "\" has epoch number = " ++ show number
     else number

--------------------------------------------------------------------------------

-- | Fill the "genesis" object "shelley", "alonzo" and "conway" properties
--   using the provided epoch number and overlay names.
shelleyAlonzoConway :: Integer -> Types.Profile -> Types.Profile
shelleyAlonzoConway epochNumber profile =
  let epochParams  = unsafePerformIO $ epochTimeline epochNumber
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
    , Types.conway  =
        case KeyMap.lookup "conway" epochParams' of
          (Just (Aeson.Object coay)) -> Just coay
          _ -> Nothing
    }) profile

epochTimeline :: Integer -> IO (KeyMap.KeyMap Aeson.Value)
epochTimeline epochNumber = do
  fp <- Paths.getDataFileName "data/genesis/epoch-timeline.json"
  eitherValue <- Aeson.eitherDecodeFileStrict fp
  return $ case eitherValue of
    (Right (Aeson.Object keyMap)) -> foldl
      (\acc key ->
        -- TODO: It will fail if then number used as string key is above 999.
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

derive :: Types.Profile -> Types.Profile
derive p@(Types.Profile _ _ _ comp _era gsis _ n gtor _ _ ana _ _ _ _) =
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
        case Types.delegators gsis of
          (Just d) -> (d, max d (Types.n_pools comp))
          Nothing -> (Types.n_pools comp, Types.n_pools comp)
      utxo_generated = generator_tx_count * Types.inputs_per_tx gtor
      utxo_stuffed = max 0 (Types.utxo gsis)

      -- Dataset:
      -----------
      dataset_measure =
        if Types.utxo gsis == 0
        then 0
        else case Types.delegators gsis of
               (Just d) -> Types.utxo gsis + d
               Nothing -> Types.utxo gsis
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

{--
  { createStakedArgs:
    ([ "--testnet-magic",          $p.genesis.network_magic
     , "--supply",                 fmt_decimal_10_5($p.genesis.funds_balance)
     , "--gen-utxo-keys",          1
     , "--gen-genesis-keys",       $p.composition.n_bft_hosts
     , "--supply-delegated",       fmt_decimal_10_5($p.derived.supply_delegated)
     , "--gen-pools",              $p.composition.n_pools
     , "--gen-stake-delegs",       $p.derived.delegators_effective
     , "--num-stuffed-utxo",       fmt_decimal_10_5($p.derived.utxo_stuffed)
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [ "--bulk-pool-cred-files",   $p.composition.n_dense_hosts
     , "--bulk-pools-per-file",    $p.composition.dense_pool_density ]
     else [] end)
  , createTestnetDataArgs:
    ([ "--testnet-magic",          $p.genesis.network_magic
     , "--total-supply",           fmt_decimal_10_5($p.genesis.funds_balance + $p.derived.supply_delegated)
     , "--utxo-keys",              1
     , "--genesis-keys",           $p.composition.n_bft_hosts
     , "--delegated-supply",       fmt_decimal_10_5($p.derived.supply_delegated)
     , "--pools",                  $p.composition.n_pools
     , "--stake-delegators",       $p.derived.delegators_effective
     , "--drep-keys",              $p.genesis.dreps
     , "--stuffed-utxo",           fmt_decimal_10_5($p.derived.utxo_stuffed)
     ])
  , pools:
    [ "--argjson"
    , "initialPoolCoin",           fmt_decimal_10_5($p.genesis.pool_coin)
    ]
  }
--}

cliArgs :: Types.Profile -> Types.Profile
cliArgs p@(Types.Profile _ _ _ comp __ gsis _ _ _ _ _ _ dved _ _ _) =
  let --toJson = map (\(k,n) -> )
      fmtDecimal i =
           Scientific.formatScientific Scientific.Fixed (Just 0) (fromInteger i / 100000)
        ++ "00000"
      createStakedArgs =
        [
          Aeson.String "--testnet-magic",    Aeson.Number $ fromInteger $ Types.network_magic gsis
        , Aeson.String "--supply",           Aeson.String $ Text.pack $ fmtDecimal $ Types.funds_balance gsis
        , Aeson.String "--gen-utxo-keys",    Aeson.Number 1
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
        , Aeson.String "--utxo-keys",        Aeson.Number 1
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

--------------------------------------------------------------------------------

libMKLocations :: [ (String, [ (String, [Types.Profile]) ] ) ]
libMKLocations =
  [
    -- Local profiles.
    ("LOCAL_PROFILES", [
      -- Families of local profiles.
      ("PROFILES_EMPTY"        , profilesNoEraEmpty)
    , ("PROFILES_MINIATURE"    , profilesNoEraMiniature)
    , ("PROFILES_FORGE_STRESS" , profilesNoEraForgeStress)
    , ("PROFILES_PLUTUSCALL"   , profilesNoEraPlutuscall)
    , ("PROFILES_MODEL"        , profilesNoEraModel)
    , ("PROFILES_K3"           , profilesNoEraK3)
    , ("PROFILES_SCENARIOS"    ,
         profilesNoEraChainsync
      ++ profilesNoEraIdle
      ++ profilesNoEraLatency
      ++ profilesNoEraTracerOnly
      )
    , ("PROFILES_LEGACY"       ,
         profilesNoEraDense
      ++ profilesNoEraDish
      )
    , ("PROFILES_SCALING"      , profilesNoEraScalingLocal)
    ])
  -- Cloud profiles.
  , ("CLOUD_PROFILES", [
      -- Families of cloud profiles.
      ("PROFILES_NOMAD_PERF"   , profilesNoEraCloud)
    , ("PROFILES_NOMAD_PERFSSD", profilesNoEraScalingCloud)
    ])
  ]

libMk :: [String]
libMk =
    foldMap
      (\(_, families) ->
        map
          (\(familyName, ps) ->
            let profileNames = map Types.name ps
            in
              -- For example:
              -- PROFILES_EMPTY := fast fast-solo ...
              -- PROFILES_MINIATURE := ci-bench ...
              familyName ++ " := " ++ unwords profileNames
          )
          families
      )
      libMKLocations
  ++
    [""] -- Empty line.
  ++
    foldMap
      (\(locationName, families) ->
        map
          (\(familyName, _) ->
            -- LOCAL_PROFILES += $(PROFILES_VENDOR)
            -- ...
            -- CLOUD_PROFILES += $(PROFILES_NOMAD_PERF)
            -- ...
            locationName ++ " += $(" ++ familyName ++ ")"
          )
          families
      )
      libMKLocations
