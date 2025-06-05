{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

import           Prelude
import           Data.List (foldl', (\\))
-- Package: aeson.
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: containers.
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- Package: tasty.
import qualified Test.Tasty           as Tasty
-- Package: tasty-hunit.
import           Test.Tasty.HUnit
-- Package: self.
import qualified Cardano.Benchmarking.Profile as Profile
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.NodeSpecs.Tests as NodeSpecs
import qualified Paths_cardano_profile as Paths
-- Static / built-in / profiles part of the test-suite.
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
import           Cardano.Benchmarking.Profile.Builtin.Scenario.TracerOnly (profilesNoEraTracerOnly)
import           Cardano.Benchmarking.Profile.Extra.Scaling               (profilesNoEraScalingLocal, profilesNoEraScalingCloud)

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =  Tasty.testGroup "cardano-profile"
  [
    testGroupTypes
  , testGroupMap
  , testGroupOverlay
  , testGroupEra
  , NodeSpecs.tests
  ]

--------------------------------------------------------------------------------

-- "ci-test-bage" `fromJson` and `toJson`.
testGroupTypes :: Tasty.TestTree
testGroupTypes = Tasty.testGroup
  "Cardano.Benchmarking.Profile.Types"
  [ testCase "Profile FromJson / ToJson" $ do
      fp <- Paths.getDataFileName "data/test/ci-test-bage.json"
      eitherAns <- Aeson.eitherDecodeFileStrict fp
      case eitherAns of
        (Left err) -> fail err
        (Right profile) -> do
          assertEqual
            ("Profile == (decode \"" ++ fp ++ "\")")
            profile    -- expected
            ciTestBage -- got
          assertEqual
            ("(encode Profile) == (decode \"" ++ fp ++ "\")")
            -- Decode-encode the same file so it has the same style.
            (Aeson.encode profile)
            (Aeson.encode ciTestBage)
  ]

ciTestBage :: Types.Profile
ciTestBage = Types.Profile {
    Types.name = "ci-test-bage"
  , Types.desc = Just "Miniature dataset, CI-friendly duration, test scale"
  , Types.composition = Types.Composition {
      Types.locations = [Types.Loopback]
    , Types.n_bft_hosts = 0
    , Types.n_singular_hosts = 2
    , Types.n_dense_hosts = 0
    , Types.dense_pool_density = 1
    , Types.with_proxy = False
    , Types.with_explorer = False
    , Types.topology = Types.UniCircle
    , Types.with_chaindb_server = Nothing
    , Types.n_hosts = 2
    , Types.n_pools = 2
    , Types.n_singular_pools = 2
    , Types.n_dense_pools = 0
    , Types.n_pool_hosts = 2
  }
  , Types.era = Types.Babbage
  , Types.chaindb = Nothing
  , Types.genesis = Types.Genesis {
      Types.pparamsEpoch = 300
    , Types.pparamsOverlays = []
    , Types.shelley = KeyMap.fromList [
        ("activeSlotsCoeff", Aeson.Number 5.0e-2)
      , ("epochLength", Aeson.Number 600.0)
      , ("protocolParams", Aeson.Object (KeyMap.fromList [
          ("a0", Aeson.Number 0.3)
        , ("decentralisationParam", Aeson.Number 0.0)
        , ("eMax", Aeson.Number 18.0)
        , ("extraEntropy", Aeson.Object (KeyMap.fromList [
           ("tag", Aeson.String "NeutralNonce")
        ]))
        , ("keyDeposit", Aeson.Number 2000000.0)
        , ("maxBlockBodySize", Aeson.Number 65536.0)
        , ("maxBlockHeaderSize", Aeson.Number 1100.0)
        , ("maxTxSize", Aeson.Number 16384.0)
        , ("minFeeA", Aeson.Number 44.0)
        , ("minFeeB", Aeson.Number 155381.0)
        , ("minPoolCost", Aeson.Number 3.4e8)
        , ("minUTxOValue", Aeson.Number 0)
        , ("nOpt", Aeson.Number 500.0)
        , ("poolDeposit", Aeson.Number 5.0e8)
        , ("protocolVersion", Aeson.Object (KeyMap.fromList [
            ("major", Aeson.Number 6.0)
          , ("minor", Aeson.Number 0.0)]
        ))
        , ("rho", Aeson.Number 3.0e-3)
        , ("tau", Aeson.Number 0.2)
      ]))
      ,("securityParam", Aeson.Number 3.0)
      ,("slotLength", Aeson.Number 1.0)
    ]
    , Types.alonzo = KeyMap.fromList [
        ("collateralPercentage",Aeson.Number 150.0)
      , ("costModels",Aeson.Object (KeyMap.fromList [
          ("PlutusV1",Aeson.Object (KeyMap.fromList [
            ("addInteger-cpu-arguments-intercept",Aeson.Number 197209.0)
          , ("addInteger-cpu-arguments-slope",Aeson.Number 0.0)
          , ("addInteger-memory-arguments-intercept",Aeson.Number 1.0)
          , ("addInteger-memory-arguments-slope",Aeson.Number 1.0)
          , ("appendByteString-cpu-arguments-intercept",Aeson.Number 396231.0)
          , ("appendByteString-cpu-arguments-slope",Aeson.Number 621.0)
          , ("appendByteString-memory-arguments-intercept",Aeson.Number 0.0)
          , ("appendByteString-memory-arguments-slope",Aeson.Number 1.0)
          , ("appendString-cpu-arguments-intercept",Aeson.Number 150000.0)
          , ("appendString-cpu-arguments-slope",Aeson.Number 1000.0)
          , ("appendString-memory-arguments-intercept",Aeson.Number 0.0)
          , ("appendString-memory-arguments-slope",Aeson.Number 1.0)
          , ("bData-cpu-arguments",Aeson.Number 150000.0)
          , ("bData-memory-arguments",Aeson.Number 32.0)
          , ("blake2b-cpu-arguments-intercept",Aeson.Number 2477736.0)
          , ("blake2b-cpu-arguments-slope",Aeson.Number 29175.0)
          , ("blake2b-memory-arguments",Aeson.Number 4.0)
          , ("cekApplyCost-exBudgetCPU",Aeson.Number 29773.0)
          , ("cekApplyCost-exBudgetMemory",Aeson.Number 100.0)
          , ("cekBuiltinCost-exBudgetCPU",Aeson.Number 29773.0)
          , ("cekBuiltinCost-exBudgetMemory",Aeson.Number 100.0)
          , ("cekConstCost-exBudgetCPU",Aeson.Number 29773.0)
          , ("cekConstCost-exBudgetMemory",Aeson.Number 100.0)
          , ("cekDelayCost-exBudgetCPU",Aeson.Number 29773.0)
          , ("cekDelayCost-exBudgetMemory",Aeson.Number 100.0)
          , ("cekForceCost-exBudgetCPU",Aeson.Number 29773.0)
          , ("cekForceCost-exBudgetMemory",Aeson.Number 100.0)
          , ("cekLamCost-exBudgetCPU",Aeson.Number 29773.0)
          , ("cekLamCost-exBudgetMemory",Aeson.Number 100.0)
          , ("cekStartupCost-exBudgetCPU",Aeson.Number 100.0)
          , ("cekStartupCost-exBudgetMemory",Aeson.Number 100.0)
          , ("cekVarCost-exBudgetCPU",Aeson.Number 29773.0)
          , ("cekVarCost-exBudgetMemory",Aeson.Number 100.0)
          , ("chooseData-cpu-arguments",Aeson.Number 150000.0)
          , ("chooseData-memory-arguments",Aeson.Number 32.0)
          , ("chooseList-cpu-arguments",Aeson.Number 150000.0)
          , ("chooseList-memory-arguments",Aeson.Number 32.0)
          , ("chooseUnit-cpu-arguments",Aeson.Number 150000.0)
          , ("chooseUnit-memory-arguments",Aeson.Number 32.0)
          , ("consByteString-cpu-arguments-intercept",Aeson.Number 150000.0)
          , ("consByteString-cpu-arguments-slope",Aeson.Number 1000.0)
          , ("consByteString-memory-arguments-intercept",Aeson.Number 0.0)
          , ("consByteString-memory-arguments-slope",Aeson.Number 1.0)
          , ("constrData-cpu-arguments",Aeson.Number 150000.0)
          , ("constrData-memory-arguments",Aeson.Number 32.0)
          , ("decodeUtf8-cpu-arguments-intercept",Aeson.Number 150000.0)
          , ("decodeUtf8-cpu-arguments-slope",Aeson.Number 1000.0)
          , ("decodeUtf8-memory-arguments-intercept",Aeson.Number 0.0)
          , ("decodeUtf8-memory-arguments-slope",Aeson.Number 8.0)
          , ("divideInteger-cpu-arguments-constant",Aeson.Number 148000.0)
          , ("divideInteger-cpu-arguments-model-arguments-intercept",Aeson.Number 425507.0)
          , ("divideInteger-cpu-arguments-model-arguments-slope",Aeson.Number 118.0)
          , ("divideInteger-memory-arguments-intercept",Aeson.Number 0.0)
          , ("divideInteger-memory-arguments-minimum",Aeson.Number 1.0)
          , ("divideInteger-memory-arguments-slope",Aeson.Number 1.0)
          , ("encodeUtf8-cpu-arguments-intercept",Aeson.Number 150000.0)
          , ("encodeUtf8-cpu-arguments-slope",Aeson.Number 1000.0)
          , ("encodeUtf8-memory-arguments-intercept",Aeson.Number 0.0)
          , ("encodeUtf8-memory-arguments-slope",Aeson.Number 8.0)
          , ("equalsByteString-cpu-arguments-constant",Aeson.Number 150000.0)
          , ("equalsByteString-cpu-arguments-intercept",Aeson.Number 112536.0)
          , ("equalsByteString-cpu-arguments-slope",Aeson.Number 247.0)
          , ("equalsByteString-memory-arguments",Aeson.Number 1.0)
          , ("equalsData-cpu-arguments-intercept",Aeson.Number 150000.0)
          , ("equalsData-cpu-arguments-slope",Aeson.Number 10000.0)
          , ("equalsData-memory-arguments",Aeson.Number 1.0)
          , ("equalsInteger-cpu-arguments-intercept",Aeson.Number 136542.0)
          , ("equalsInteger-cpu-arguments-slope",Aeson.Number 1326.0)
          , ("equalsInteger-memory-arguments",Aeson.Number 1.0)
          , ("equalsString-cpu-arguments-constant",Aeson.Number 1000.0)
          , ("equalsString-cpu-arguments-intercept",Aeson.Number 150000.0)
          , ("equalsString-cpu-arguments-slope",Aeson.Number 1000.0)
          , ("equalsString-memory-arguments",Aeson.Number 1.0)
          , ("fstPair-cpu-arguments",Aeson.Number 150000.0)
          , ("fstPair-memory-arguments",Aeson.Number 32.0)
          , ("headList-cpu-arguments",Aeson.Number 150000.0)
          , ("headList-memory-arguments",Aeson.Number 32.0)
          , ("iData-cpu-arguments",Aeson.Number 150000.0)
          , ("iData-memory-arguments",Aeson.Number 32.0)
          , ("ifThenElse-cpu-arguments",Aeson.Number 1.0)
          , ("ifThenElse-memory-arguments",Aeson.Number 1.0)
          , ("indexByteString-cpu-arguments",Aeson.Number 150000.0)
          , ("indexByteString-memory-arguments",Aeson.Number 1.0)
          , ("lengthOfByteString-cpu-arguments",Aeson.Number 150000.0)
          , ("lengthOfByteString-memory-arguments",Aeson.Number 4.0)
          , ("lessThanByteString-cpu-arguments-intercept",Aeson.Number 103599.0)
          , ("lessThanByteString-cpu-arguments-slope",Aeson.Number 248.0)
          , ("lessThanByteString-memory-arguments",Aeson.Number 1.0)
          , ("lessThanEqualsByteString-cpu-arguments-intercept",Aeson.Number 103599.0)
          , ("lessThanEqualsByteString-cpu-arguments-slope",Aeson.Number 248.0)
          , ("lessThanEqualsByteString-memory-arguments",Aeson.Number 1.0)
          , ("lessThanEqualsInteger-cpu-arguments-intercept",Aeson.Number 145276.0)
          , ("lessThanEqualsInteger-cpu-arguments-slope",Aeson.Number 1366.0)
          , ("lessThanEqualsInteger-memory-arguments",Aeson.Number 1.0)
          , ("lessThanInteger-cpu-arguments-intercept",Aeson.Number 179690.0)
          , ("lessThanInteger-cpu-arguments-slope",Aeson.Number 497.0)
          , ("lessThanInteger-memory-arguments",Aeson.Number 1.0)
          , ("listData-cpu-arguments",Aeson.Number 150000.0)
          , ("listData-memory-arguments",Aeson.Number 32.0)
          , ("mapData-cpu-arguments",Aeson.Number 150000.0)
          , ("mapData-memory-arguments",Aeson.Number 32.0)
          , ("mkCons-cpu-arguments",Aeson.Number 150000.0)
          , ("mkCons-memory-arguments",Aeson.Number 32.0)
          , ("mkNilData-cpu-arguments",Aeson.Number 150000.0)
          , ("mkNilData-memory-arguments",Aeson.Number 32.0)
          , ("mkNilPairData-cpu-arguments",Aeson.Number 150000.0)
          , ("mkNilPairData-memory-arguments",Aeson.Number 32.0)
          , ("mkPairData-cpu-arguments",Aeson.Number 150000.0)
          , ("mkPairData-memory-arguments",Aeson.Number 32.0)
          , ("modInteger-cpu-arguments-constant",Aeson.Number 148000.0)
          , ("modInteger-cpu-arguments-model-arguments-intercept",Aeson.Number 425507.0)
          , ("modInteger-cpu-arguments-model-arguments-slope",Aeson.Number 118.0)
          , ("modInteger-memory-arguments-intercept",Aeson.Number 0.0)
          , ("modInteger-memory-arguments-minimum",Aeson.Number 1.0)
          , ("modInteger-memory-arguments-slope",Aeson.Number 1.0)
          , ("multiplyInteger-cpu-arguments-intercept",Aeson.Number 61516.0)
          , ("multiplyInteger-cpu-arguments-slope",Aeson.Number 11218.0)
          , ("multiplyInteger-memory-arguments-intercept",Aeson.Number 0.0)
          , ("multiplyInteger-memory-arguments-slope",Aeson.Number 1.0)
          , ("nullList-cpu-arguments",Aeson.Number 150000.0)
          , ("nullList-memory-arguments",Aeson.Number 32.0)
          , ("quotientInteger-cpu-arguments-constant",Aeson.Number 148000.0)
          , ("quotientInteger-cpu-arguments-model-arguments-intercept",Aeson.Number 425507.0)
          , ("quotientInteger-cpu-arguments-model-arguments-slope",Aeson.Number 118.0)
          , ("quotientInteger-memory-arguments-intercept",Aeson.Number 0.0)
          , ("quotientInteger-memory-arguments-minimum",Aeson.Number 1.0)
          , ("quotientInteger-memory-arguments-slope",Aeson.Number 1.0)
          , ("remainderInteger-cpu-arguments-constant",Aeson.Number 148000.0)
          , ("remainderInteger-cpu-arguments-model-arguments-intercept",Aeson.Number 425507.0)
          , ("remainderInteger-cpu-arguments-model-arguments-slope",Aeson.Number 118.0)
          , ("remainderInteger-memory-arguments-intercept",Aeson.Number 0.0)
          , ("remainderInteger-memory-arguments-minimum",Aeson.Number 1.0)
          , ("remainderInteger-memory-arguments-slope",Aeson.Number 1.0)
          , ("sha2_256-cpu-arguments-intercept",Aeson.Number 2477736.0)
          , ("sha2_256-cpu-arguments-slope",Aeson.Number 29175.0)
          , ("sha2_256-memory-arguments",Aeson.Number 4.0)
          , ("sha3_256-cpu-arguments-intercept",Aeson.Number 0.0)
          , ("sha3_256-cpu-arguments-slope",Aeson.Number 82363.0)
          , ("sha3_256-memory-arguments",Aeson.Number 4.0)
          , ("sliceByteString-cpu-arguments-intercept",Aeson.Number 150000.0)
          , ("sliceByteString-cpu-arguments-slope",Aeson.Number 5000.0)
          , ("sliceByteString-memory-arguments-intercept",Aeson.Number 0.0)
          , ("sliceByteString-memory-arguments-slope",Aeson.Number 1.0)
          , ("sndPair-cpu-arguments",Aeson.Number 150000.0)
          , ("sndPair-memory-arguments",Aeson.Number 32.0)
          , ("subtractInteger-cpu-arguments-intercept",Aeson.Number 197209.0)
          , ("subtractInteger-cpu-arguments-slope",Aeson.Number 0.0)
          , ("subtractInteger-memory-arguments-intercept",Aeson.Number 1.0)
          , ("subtractInteger-memory-arguments-slope",Aeson.Number 1.0)
          , ("tailList-cpu-arguments",Aeson.Number 150000.0)
          , ("tailList-memory-arguments",Aeson.Number 32.0)
          , ("trace-cpu-arguments",Aeson.Number 150000.0)
          , ("trace-memory-arguments",Aeson.Number 32.0)
          , ("unBData-cpu-arguments",Aeson.Number 150000.0)
          , ("unBData-memory-arguments",Aeson.Number 32.0)
          , ("unConstrData-cpu-arguments",Aeson.Number 150000.0)
          , ("unConstrData-memory-arguments",Aeson.Number 32.0)
          , ("unIData-cpu-arguments",Aeson.Number 150000.0)
          , ("unIData-memory-arguments",Aeson.Number 32.0)
          , ("unListData-cpu-arguments",Aeson.Number 150000.0)
          , ("unListData-memory-arguments",Aeson.Number 32.0)
          , ("unMapData-cpu-arguments",Aeson.Number 150000.0)
          , ("unMapData-memory-arguments",Aeson.Number 32.0)
          , ("verifySignature-cpu-arguments-intercept",Aeson.Number 3345831.0)
          , ("verifySignature-cpu-arguments-slope",Aeson.Number 1.0)
          , ("verifySignature-memory-arguments",Aeson.Number 1.0)
          ]))
        ]))
    , ("executionPrices", Aeson.Object (KeyMap.fromList [
        ("prMem",Aeson.Object (KeyMap.fromList [
          ("denominator",Aeson.Number 10000.0)
        , ("numerator",Aeson.Number 577.0)
        ]))
      , ("prSteps",Aeson.Object (KeyMap.fromList [
          ("denominator",Aeson.Number 1.0e7)
        , ("numerator",Aeson.Number 721.0)
        ]))
      ]))
    , ("lovelacePerUTxOWord",Aeson.Number 34482.0)
    , ("maxBlockExUnits",Aeson.Object (KeyMap.fromList [
        ("exUnitsMem",Aeson.Number 5.0e7)
      , ("exUnitsSteps",Aeson.Number 4.0e10)
      ]))
    , ("maxCollateralInputs",Aeson.Number 3.0)
    , ("maxTxExUnits",Aeson.Object (KeyMap.fromList [
        ("exUnitsMem",Aeson.Number 1.0e7)
      , ("exUnitsSteps",Aeson.Number 1.0e10)
      ]))
    , ("maxValueSize",Aeson.Number 5000.0)
    ]
    , Types.conway = Nothing
    , Types.slot_duration = 1
    , Types.epoch_length = 600
    , Types.active_slots_coeff = 0.05
    , Types.parameter_k = 3
    , Types.utxo = 0
    , Types.delegators = 0
    , Types.dreps = 0
    , Types.extra_future_offset = 0
    , Types.per_pool_balance = 1000000000000000
    , Types.funds_balance = 10000000000000
    , Types.utxo_keys = 1
    , Types.network_magic = 42
    , Types.pool_coin = 1000000000000000
    , Types.delegator_coin = 0
    , Types.single_shot = True
    , Types.max_block_size = Nothing
  }
  , Types.scenario = Types.FixedLoaded
  , Types.node = Types.Node {
      Types.utxo_lmdb = False
    , Types.ssd_directory = Nothing
    , Types.verbatim = Types.NodeVerbatim Nothing
    , Types.trace_forwarding = True
    , Types.tracing_backend = "trace-dispatcher"
    , Types.rts_flags_override = []
    , Types.heap_limit = Nothing
    , Types.shutdown_on_slot_synced = Nothing
    , Types.shutdown_on_block_synced = Just 3
  }
  , Types.generator = Types.Generator {
      Types.tps = 15
    , Types.inputs_per_tx = 2
    , Types.outputs_per_tx = 2
    , Types.tx_fee = 1000000
    , Types.init_cooldown = 5
    , Types.plutus = Types.Plutus {
        Types.plutusType = Nothing
      , Types.plutusScript = Nothing
      , Types.redeemer = Nothing
      }
    , Types.epochs = 3
    , Types.tx_count = Just 9000
    , Types.add_tx_size = 100
  }
  , Types.workloads = []
  , Types.tracer = Types.Tracer {
      Types.rtview = False
    , Types.ekg = False
    , Types.withresources = False
  }
  , Types.cluster = Nothing
  , Types.analysis = Types.Analysis {
      Types.analysisType = Just "standard"
    , Types.cluster_base_startup_overhead_s = 40
    , Types.start_log_spread_s = 120
    , Types.last_log_spread_s = 120
    , Types.silence_since_last_block_s = 120
    , Types.tx_loss_ratio = 2.0e-2
    , Types.finish_patience = 21
    , Types.filters = []
    , Types.filter_exprs = [
      (Types.AnalysisFilterExpression {
        Types.tag = "CBlock"
      , Types.contents = Types.AnalysisFilterExpressionContent {
          Types.innerTag = "BMinimumAdoptions"
        , Types.innerContents = 1
        }
      })
    ]
    , Types.minimum_chain_density = 2.5e-2
    , Types.cluster_startup_overhead_s = 40
  }
  , Types.derived = Types.Derived {
      Types.epoch_duration = 600
    , Types.effective_epochs = 1
    , Types.genesis_future_offset = 40
    , Types.generator_duration = 600
    , Types.generator_tx_count = 9000
    , Types.dataset_measure = 0
    , Types.delegators_effective = 2
    , Types.supply_total = 2010000000000000
    , Types.supply_delegated = 2000000000000000
    , Types.utxo_delegated = 0
    , Types.utxo_generated = 18000
    , Types.utxo_stuffed = 0
    , Types.shutdown_time = Nothing
    , Types.default_value_tx_size_estimate = 381
    , Types.default_value_tx_per_block_estimate = 172
    , Types.generator_blocks_lower_bound = 61
    , Types.dataset_induced_startup_delay_optimistic = 40
    , Types.dataset_induced_startup_delay_conservative = 40
  }
  , Types.cli_args = Types.CliArgs {
      Types.createStakedArgs = [
          Aeson.String "--testnet-magic", Aeson.Number 42.0
        , Aeson.String "--supply", Aeson.String "10000000000000"
        , Aeson.String "--gen-utxo-keys", Aeson.Number 1.0
        , Aeson.String "--gen-genesis-keys", Aeson.Number 0.0
        , Aeson.String "--supply-delegated", Aeson.String "2000000000000000"
        , Aeson.String "--gen-pools", Aeson.Number 2.0
        , Aeson.String "--gen-stake-delegs", Aeson.Number 2.0
        , Aeson.String "--num-stuffed-utxo", Aeson.String "000000"
      ]
    , Types.createTestnetDataArgs = [
          Aeson.String "--testnet-magic", Aeson.Number 42.0
        , Aeson.String "--total-supply", Aeson.String "2010000000000000"
        , Aeson.String "--utxo-keys", Aeson.Number 1.0
        , Aeson.String "--genesis-keys", Aeson.Number 0.0
        , Aeson.String "--delegated-supply", Aeson.String "2000000000000000"
        , Aeson.String "--pools", Aeson.Number 2.0
        , Aeson.String "--stake-delegators", Aeson.Number 2.0
        , Aeson.String "--drep-keys", Aeson.Number 0.0
        , Aeson.String "--stuffed-utxo", Aeson.String "000000"
      ]
    , Types.pools = [
          Aeson.String "--argjson"
        , Aeson.String "initialPoolCoin", Aeson.String "1000000000000000"
      ]
  }
  , Types.preset = mempty
  , Types.overlay = mempty
}

--------------------------------------------------------------------------------

profiles :: Map.Map String Types.Profile
profiles = Map.fromList $ map
  (\p ->
    ( Types.name p
    , Profile.realize p -- No overlay added!
    )
  )
  profilesRaw

-- All profiles without an overlay.
profilesRaw :: [Types.Profile]
profilesRaw =
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
  ++ profilesNoEraTracerOnly       -- Scenario `TracerOnly`
  -- Extra modules
  ++ profilesNoEraScalingLocal
  ++ profilesNoEraScalingCloud

-- Check all builtin profiles (no overlay) with "data/all-profiles.json".
-- `Profile` properties are checked independently for better error messages.
testGroupMap :: Tasty.TestTree
testGroupMap = Tasty.testGroup
  "Cardano.Benchmarking.Profile.Map (Without overlay)"
  [ testCase "Profiles (Builtin)" $ do
      fp <- Paths.getDataFileName "data/all-profiles-coay.json"
      eitherAns <- Aeson.eitherDecodeFileStrict fp
      case eitherAns of
        (Left err) -> fail err
        (Right allProfiles) -> do
          ----------------------------------------------------------------------
          -- Check all keys/names first (if error what's below makes no sense!)
          ----------------------------------------------------------------------
          assertEqual
            ("Profile == (decode \"" ++ fp ++ "\") - Keys difference")
            [] -- Expected value.
            (Map.keys $ Map.difference
              profiles
              (allProfiles :: Map.Map String Types.Profile)
            )
          -- Check names.
          assertEqual
            ("Profile == (decode \"" ++ fp ++ "\") - Name")
            [] -- Expected value.
            ((\\)
              (Map.keys $ Map.map Types.name allProfiles)
              (Map.keys $ Map.map Types.name profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Scenario type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Scenario")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.scenario allProfiles)
              (Map.assocs $ Map.map Types.scenario profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Composition type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Composition")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.composition allProfiles)
              (Map.assocs $ Map.map Types.composition profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Era type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Era")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.era allProfiles)
              (Map.assocs $ Map.map Types.era profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Genesis type (main).
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Genesis (main)")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              -- Everything except "shelley", "alonzo" and "conway".
              (Map.assocs $ Map.map
                (\p -> (Types.genesis p) {
                  Types.shelley = mempty
                , Types.alonzo = mempty
                , Types.conway = mempty
                })
                allProfiles
              )
              -- Everything except "shelley", "alonzo" and "conway".
              (Map.assocs $ Map.map
                (\p -> (Types.genesis p) {
                  Types.shelley = mempty
                , Types.alonzo = mempty
                , Types.conway = mempty
                })
                profiles
              )
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Genesis type (Shelley).
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Genesis (Shelley)")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map (Types.shelley . Types.genesis) allProfiles)
              (Map.assocs $ Map.map (Types.shelley . Types.genesis) profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Genesis type (Alonzo).
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Genesis (Alonzo)")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map (Types.alonzo . Types.genesis) allProfiles)
              (Map.assocs $ Map.map (Types.alonzo . Types.genesis) profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Genesis type (Conway).
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Genesis (Conway)")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map (Types.conway . Types.genesis) allProfiles)
              (Map.assocs $ Map.map (Types.conway . Types.genesis) profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the ChainDB type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - ChainDB")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.chaindb allProfiles)
              (Map.assocs $ Map.map Types.chaindb profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Node type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Node")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.node allProfiles)
              (Map.assocs $ Map.map Types.node profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Generator type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Generator")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.generator allProfiles)
              (Map.assocs $ Map.map Types.generator profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Workloads list.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - [Workload]")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.workloads allProfiles)
              (Map.assocs $ Map.map Types.workloads profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Tracer type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Tracer")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.tracer allProfiles)
              (Map.assocs $ Map.map Types.tracer profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Cluster type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Cluster")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.cluster allProfiles)
              (Map.assocs $ Map.map Types.cluster profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Analysis type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Analysis")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.analysis allProfiles)
              (Map.assocs $ Map.map Types.analysis profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the Derived type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Derived")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.derived allProfiles)
              (Map.assocs $ Map.map Types.derived profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in the CliArgs type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - CliArgs")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.cli_args allProfiles)
              (Map.assocs $ Map.map Types.cli_args profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in "preset".
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - preset")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.preset allProfiles)
              (Map.assocs $ Map.map Types.preset profiles)
            )
          ----------------------------------------------------------------------
          -- Show first profile with differences in "overlay".
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - overlay")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.overlay allProfiles)
              (Map.assocs $ Map.map Types.overlay profiles)
            )
  , testCase "Profiles (Duplicate names)" $
      let
        go (T set duplicates) name
          | name `Set.member` set = T set (name:duplicates)
          | otherwise             = T (name `Set.insert` set) duplicates
      in assertEqual "Duplicate definition(s) for profile(s)" []
          $ sndT $ foldl' go (T Set.empty []) (map Types.name profilesRaw)
  ]

-- little helper type for tuples strict in the first element
data TupleStrictFirst a b = T !a b

sndT :: TupleStrictFirst a b -> b
sndT (T _ b) = b

--------------------------------------------------------------------------------

-- The overlay to apply.
overlay :: Aeson.Object
overlay =
  KeyMap.fromList [
    -- By applying an overlay to the name, all profiles will have the same name,
    -- with the eras appended. The first one to match will be the one that was
    -- already first, actually "10-alra" (containers implementation dependent).
    ("name",    Aeson.String "HOLA!")
  , ("genesis", Aeson.Object $ KeyMap.fromList [
      ("network_magic"     , Aeson.Number 1327330847)
      -- As "10-*" profiles have "--shutdown-on-block-synced", the effective
      -- epochs will be calculated using the blocks per epochs number obtained
      -- using the active slots coefficient.
    , ("active_slots_coeff", Aeson.Number 0.000001)
    ])
  ]

testGroupOverlay :: Tasty.TestTree
testGroupOverlay = Tasty.testGroup
  "Cardano.Benchmarking.Profile.Map (With overlay)"
  [ testCase "HOLA!" $ do
      fp <- Paths.getDataFileName "data/test/ci-test-bage.json"
      eitherAns <- Aeson.eitherDecodeFileStrict fp
      case eitherAns of
        (Left err) -> fail err
        (Right profile) -> do
          let profileWithOverlay = Profile.realize (profile {Types.overlay = overlay})
          assertEqual "New name"
            "HOLA!"
            (Types.name profileWithOverlay)
          assertEqual "New genesis.network_magic"
            1327330847
            (Types.network_magic $ Types.genesis profileWithOverlay)
          -- The overlay should be applied before calculating derived values.
          assertEqual "New derived.effective_epochs"
            5000
            (Types.effective_epochs $ Types.derived profileWithOverlay)
          -- The overlay used is added to the profile.
          assertEqual "New overlay"
            overlay
            (Types.overlay profileWithOverlay)
  ]

testGroupEra :: Tasty.TestTree
testGroupEra = Tasty.testGroup
  "Cardano.Benchmarking.Profile.addEras"
  [ testCase "Era specific profiles" $
      let
        isDefined = (`Set.member` Map.keysSet (Profile.addEras profiles))
        failures  =
          [ "ci-bench-plutusv3-ripemd-bage"     -- PlutusV3 - should require PV 9.0 / Conway
          , "ci-bench-plutus-secp-schnorr-alzo" -- PlutusV2 - should require PV 7.0 / Babbage
          , "ci-test-plutus-mary"               -- PlutusV1 - should require PV 5.0 / Alonzo
          ]
        successes =
          [ "ci-bench-plutusv3-ripemd-coay"
          , "ci-bench-plutus-secp-schnorr-bage"
          , "ci-test-plutus-alzo"
          ]
      in do
        assertEqual "Profiles that are expected to not be defined, yet they are:"
          []
          (filter isDefined failures)
        assertEqual "Profiles that are expected to be defined, but some are missing:"
          successes
          (filter isDefined successes)
  ]
