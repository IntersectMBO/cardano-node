{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------------------
module Main (main) where

import           Prelude

import           Data.Maybe (fromJust)

import qualified Data.Aeson           as Aeson
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
    profileTypes
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
      Types.network_magic = 0
    , Types.single_shot = False
    , Types.per_pool_balance = 0
    , Types.funds_balance = 0
    , Types.utxo = 0
    , Types.active_slots_coeff = 0
    , Types.epoch_length = 0
    , Types.parameter_k = 0
    , Types.slot_duration = 0
    , Types.extra_future_offset = 0
    , Types.pparamsEpoch = 0
    , Types.delegators = 0
    , Types.shelley = mempty
    , Types.alonzo = mempty
    , Types.pool_coin = 0
    , Types.delegator_coin = 0
  }
  , Types.scenario = Types.FixedLoaded
  , Types.node = Types.Node {
      Types.rts_flags_override = []
    , Types.shutdown_on_slot_synced = Nothing
    , Types.shutdown_on_block_synced = Just 3
    , Types.tracing_backend = "trace-dispatcher"
    , Types.nodeTracer = True
    , Types.verbatim = Types.NodeVerbatim Nothing
  }
  , Types.tracer = Types.Tracer {
      Types.rtview = False
    , Types.ekg = False
    , Types.withresources = False
  }
  , Types.generator = Types.Generator {
      Types.add_tx_size = 100
    , Types.init_cooldown = 5
    , Types.inputs_per_tx = 2
    , Types.outputs_per_tx = 2
    , Types.tx_fee = 1000000
    , Types.epochs = 3
    , Types.tps = 15
    , Types.plutus = Just (Types.Plutus {
        Types.plutusType = Nothing
      , Types.plutusScript = Nothing
      })
    , Types.tx_count = 9000
  }
  , Types.cluster = Types.Cluster {
      Types.nomad = Types.Nomad {
        Types.namespace = "default"
      , Types.nomad_class = ""
      , Types.resources = Types.ByNodeType {
          Types.producer = Types.Resources 2 15000 16000
        , Types.explorer = Just $ Types.Resources 2 15000 16000
        }
      , Types.fetch_logs_ssh = False
      }
    , Types.aws = Types.ClusterAWS {
        Types.instance_type = Types.ByNodeType {
          Types.producer = "c5.2xlarge"
        , Types.explorer = Just "m5.4xlarge"
        }
      }
    , Types.minimun_storage = Just $ Types.ByNodeType {
        Types.producer = 12582912
      , Types.explorer = Just 14155776
      }
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
}

profilesMap :: Tasty.TestTree
profilesMap = Tasty.testGroup
  "Cardano.Benchmarking.Profile.Map"
  [ testCase "Profile FromJson" $ do
      fp <- Paths.getDataFileName "data/all-profiles.json"
      eitherAns <- Aeson.eitherDecodeFileStrict fp
      case eitherAns of
        (Left err) -> fail err
        (Right ans) -> do
          ----------------------------------------------------------------------
          -- Check all keys/names first (if error what's below makes no sense!)
          ----------------------------------------------------------------------
          assertEqual
            ("Profile == (decode \"" ++ fp ++ "\") - Keys difference")
            []
            (Map.keys $ Map.difference
              Profiles.profiles
              (ans :: Map.Map String Types.Profile)
            )
          -- Check names.
          assertEqual
            ("Profile == (decode \"" ++ fp ++ "\") - Name")
            (Map.map
              (\p -> Types.name p)
              (ans :: Map.Map String Types.Profile)
            )
            (Map.map
              (\p -> Types.name p)
              Profiles.profiles
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
              (Map.assocs $ Map.map
                (\p -> Types.scenario p)
                (ans :: Map.Map String Types.Profile)
              )
              (Map.assocs $ Map.map
                (\p -> Types.scenario p) Profiles.profiles
              )
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
              (Map.assocs $ Map.map
                (\p -> Types.composition p)
                (ans :: Map.Map String Types.Profile)
              )
              (Map.assocs $ Map.map
                (\p -> Types.composition p) Profiles.profiles
              )
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Genesis type (partial).
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Analysis (partial)")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map
                (\p ->
                  let g = Types.genesis p
                  in (Types.utxo g, Types.delegators g, Types.epoch_length g, Types.parameter_k g)
                )
                (ans :: Map.Map String Types.Profile)
              )
              (Map.assocs $ Map.map
                (\p ->
                  let g = Types.genesis p
                  in (Types.utxo g, Types.delegators g, Types.epoch_length g, Types.parameter_k g)
                )
                Profiles.profiles
              )
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
              (Map.assocs $ Map.map
                (\p -> Types.node p)
                (ans :: Map.Map String Types.Profile)
              )
              (Map.assocs $ Map.map
                (\p -> Types.node p) Profiles.profiles
              )
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
              (Map.assocs $ Map.map
                (\p -> Types.tracer p)
                (ans :: Map.Map String Types.Profile)
              )
              (Map.assocs $ Map.map
                (\p -> Types.tracer p) Profiles.profiles
              )
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Tracer type.
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Cluster")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map
                (\p -> Types.cluster p)
                (ans :: Map.Map String Types.Profile)
              )
              (Map.assocs $ Map.map
                (\p -> Types.cluster p) Profiles.profiles
              )
            )
          ----------------------------------------------------------------------
          -- Show the first profile with differences in the Analysis type (partial).
          ----------------------------------------------------------------------
          mapM_
            (uncurry $ assertEqual
              ("Profile == (decode \"" ++ fp ++ "\") - Analysis (partial)")
            )
            -- Map.Map to keep the key / profile name.
            (zip
              (Map.assocs $ Map.map
                (\p ->
                  let a = Types.analysis p
                  in (Types.analysisType a, Types.filters a)
                )
                (ans :: Map.Map String Types.Profile)
              )
              (Map.assocs $ Map.map
                (\p ->
                  let a = Types.analysis p
                  in (Types.analysisType a, Types.filters a)
                )
                Profiles.profiles
              )
            )
  ]
