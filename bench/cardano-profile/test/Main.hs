{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

import           Prelude

import           Data.Maybe (fromJust)
import           Data.List ((\\))

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Test.Tasty           as Tasty
import           Test.Tasty.HUnit

import qualified Cardano.Benchmarking.Profile.Map as Profiles
import qualified Cardano.Benchmarking.Profile.Types as Types

import qualified Paths_cardano_profile as Paths

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =  Tasty.testGroup "cardano-profile"
  [
    profileTypes -- "ci-test-bage" `fromJson` and `toJson`.
  , profilesMap
  ]

profileTypes :: Tasty.TestTree
profileTypes = Tasty.testGroup
  "Cardano.Benchmarking.Profile.Types"
  [ testCase "Profile FromJson" $ do
      fp <- Paths.getDataFileName "data/ci-test-bage.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("Profile == (decode \"" ++ fp ++ "\")")
        (Right ciTestBage)
        ans
  , testCase "Profile ToJson" $ do
      fp <- Paths.getDataFileName "data/ci-test-bage.json"
      bsl <- BSL.readFile fp
      assertEqual
        ("(encode Profile) == (decode \"" ++ fp ++ "\")")
        (Aeson.encode ciTestBage)
        -- Decode-encode the same file so it has the same style.
        (Aeson.encode $
          fromJust (Aeson.decode bsl :: (Maybe Types.Profile))
        )
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
        , ("minUTxOValue", Aeson.Number 34482.0)
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
    , Types.delegators = Just 0
    , Types.dreps = 0
    , Types.extra_future_offset = 0
    , Types.per_pool_balance = 1000000000000000
    , Types.funds_balance = 10000000000000
    , Types.network_magic = 42
    , Types.pool_coin = 1000000000000000
    , Types.delegator_coin = 0
    , Types.single_shot = True
  }
  , Types.scenario = Types.FixedLoaded
  , Types.node = Types.Node {
      Types.utxo_lmdb = False
    , Types.verbatim = Types.NodeVerbatim Nothing
    , Types.nodeTracer = True
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
  , Types.tracer = Types.Tracer {
      Types.rtview = False
    , Types.ekg = False
    , Types.withresources = False
  }
  , Types.cluster = Types.Cluster {
      Types.nomad = Types.ClusterNomad {
        Types.namespace = "default"
      , Types.nomad_class = ""
      , Types.resources = Types.ByNodeType {
          Types.producer = Types.Resources 2 15000 16000
        , Types.explorer = Just $ Types.Resources 2 15000 16000
        }
      , Types.host_volumes = Nothing
      , Types.fetch_logs_ssh = False
      }
    , Types.aws = Types.ClusterAWS {
        Types.instance_type = Types.ByNodeType {
          Types.producer = "c5.2xlarge"
        , Types.explorer = Just "m5.4xlarge"
        }
      , Types.use_public_routing = False
      }
    , Types.minimun_storage = Just $ Types.ByNodeType {
        Types.producer = 12582912
      , Types.explorer = Just 14155776
      }
    , Types.ssd_directory = Nothing
    , Types.keep_running = False
  }
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

profilesMap :: Tasty.TestTree
profilesMap = Tasty.testGroup
  "Cardano.Benchmarking.Profile.Map"
  [ testCase "Profile Builtin" $ do
      fp <- Paths.getDataFileName "data/all-profiles.json"
      eitherAns <- Aeson.eitherDecodeFileStrict fp
      case eitherAns of
        (Left err) -> fail err
        (Right allProfiles) -> do
          ----------------------------------------------------------------------
          -- Check all keys/names first (if error what's below makes no sense!)
          ----------------------------------------------------------------------
          assertEqual
            ("Profile == (decode \"" ++ fp ++ "\") - Keys difference")
            []
            (Map.keys $ Map.difference
              Profiles.profiles
              (allProfiles :: Map.Map String Types.Profile)
            )
          -- Check names.
          assertEqual
            ("Profile == (decode \"" ++ fp ++ "\") - Name")
            []
            ((\\)
              (Map.keys $ Map.map Types.name allProfiles)
              (Map.keys $ Map.map Types.name Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Scenario type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Scenario")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.scenario allProfiles)
              (Map.assocs $ Map.map Types.scenario Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Composition type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Composition")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.composition allProfiles)
              (Map.assocs $ Map.map Types.composition Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Era type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Era")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.era allProfiles)
              (Map.assocs $ Map.map Types.era Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Genesis type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Genesis")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.genesis allProfiles)
              (Map.assocs $ Map.map Types.genesis Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Node type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Node")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.node allProfiles)
              (Map.assocs $ Map.map Types.node Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Generator type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Generator")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.generator allProfiles)
              (Map.assocs $ Map.map Types.generator Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Tracer type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Tracer")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.tracer allProfiles)
              (Map.assocs $ Map.map Types.tracer Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Cluster type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Cluster")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.cluster allProfiles)
              (Map.assocs $ Map.map Types.cluster Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Analysis type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Analysis")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.analysis allProfiles)
              (Map.assocs $ Map.map Types.analysis Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Derived type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Derived")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.derived allProfiles)
              (Map.assocs $ Map.map Types.derived Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the CliArgs type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - CliArgs")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.cli_args allProfiles)
              (Map.assocs $ Map.map Types.cli_args Profiles.profiles)
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in "preset".
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - preset")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map Types.preset allProfiles)
              (Map.assocs $ Map.map Types.preset Profiles.profiles)
            )
  ]
