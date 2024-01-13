{-# LANGUAGE Trustworthy #-}

--------------------------------------------------------------------------------
module Main (main) where

import           Prelude

import           Data.Maybe (fromJust)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Test.Tasty           as Tasty
import           Test.Tasty.HUnit

import qualified Cardano.Benchmarking.Topology as Topo
import qualified Cardano.Benchmarking.Topology.Types as Types

import qualified Paths_cardano_topology as Paths

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =  Tasty.testGroup "cardano-topology"
  [
      topologyTypes
    , topology
  ]

topologyTypes :: Tasty.TestTree
topologyTypes = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Types"
  [ testCase "Topology FromJson" $ do
      fp <- Paths.getDataFileName "data/bench-torus-dense-52.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("Topology == (decode \"" ++ fp ++ "\")")
        (Right benchTorusDense52)
        ans
  , testCase "Topology ToJson" $ do
      fp <- Paths.getDataFileName "data/bench-torus-dense-52.json"
      bsl <- BSL.readFile fp
      assertEqual
        ("(encode Topology) == (decode \"" ++ fp ++ "\")")
        (Aeson.encode benchTorusDense52)
        -- Decode-encode the same file so it has the same style.
        (Aeson.encode $
          fromJust (Aeson.decode bsl :: (Maybe Types.Topology))
        )
  ]

-- Create what profiles use and compare with previous profiles outputs.
topology :: Tasty.TestTree
topology = Tasty.testGroup
  "Cardano.Benchmarking.Topology"
  [ testCase "ci-test" $ do
      fp <- Paths.getDataFileName "data/ci-test.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("UniCircle (ci-test loopback) == (decode \"" ++ fp ++ "\")")
        ans
        (Right $ Topo.mkTopology
          (Topo.UniCircle
            2
            Types.Loopback
            (\_ -> Just 1)
          )
          Nothing
        )
  , testCase "ci-test-nomadcwqa" $ do
      fp <- Paths.getDataFileName "data/ci-test-nomadcwqa.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("Torus (ci-test nomadcwqa) == (decode \"" ++ fp ++ "\")")
        ans
        (Right $ Topo.mkTopology
          (Topo.Torus
            2
            [
              Types.AWS Types.EU_CENTRAL_1
            , Types.AWS Types.US_EAST_2
            ]
            (\_ -> Just 1)
          )
          (Just (Types.AWS Types.EU_CENTRAL_1))
        )
  , testCase "ci-test-nomadperf" $ do
      fp <- Paths.getDataFileName "data/ci-test-nomadperf.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("Torus (ci-test nomadcwqa) == (decode \"" ++ fp ++ "\")")
        ans
        (Right $ Topo.mkTopology
          (Topo.Torus
            2
            [
              Types.AWS Types.EU_CENTRAL_1
            , Types.AWS Types.US_EAST_1
            , Types.AWS Types.AP_SOUTHEAST_2
            ]
            (\_ -> Just 1)
          )
          (Just (Types.AWS Types.EU_CENTRAL_1))
        )
  -- For some reason "default" for supervisor/local is using UniCircle and
  -- Nomad/cloud runs it's using Torus.
  , testCase "default" $ do
      fp <- Paths.getDataFileName "data/default.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("UniCircle (loopback) == (decode \"" ++ fp ++ "\")")
        ans
        (Right $ Topo.mkTopology
          (Topo.UniCircle
            6
            Types.Loopback
            (\_ -> Just 1)
          )
          Nothing
        )
  , testCase "default-nomadcwqa" $ do
      fp <- Paths.getDataFileName "data/default-nomadcwqa.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("Torus (default nomadcwqa) == (decode \"" ++ fp ++ "\")")
        ans
        (Right $ Topo.mkTopology
          (Topo.Torus
            6
            [
              Types.AWS Types.EU_CENTRAL_1
            , Types.AWS Types.US_EAST_2
            ]
            (\_ -> Just 1)
          )
          (Just (Types.AWS Types.EU_CENTRAL_1))
        )
  , testCase "default-nomadperf" $ do
      fp <- Paths.getDataFileName "data/default-nomadperf.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("Torus (default nomadperf) == (decode \"" ++ fp ++ "\")")
        ans
        (Right $ Topo.mkTopology
          (Topo.Torus
            6
            [
              Types.AWS Types.EU_CENTRAL_1
            , Types.AWS Types.US_EAST_1
            , Types.AWS Types.AP_SOUTHEAST_2
            ]
            (\_ -> Just 1)
          )
          (Just (Types.AWS Types.EU_CENTRAL_1))
        )
  , testCase "value-nomadperf" $ do
      fp <- Paths.getDataFileName "data/bench-torus-dense-52.json"
      ans <- Aeson.eitherDecodeFileStrict fp
      assertEqual
        ("TorusDense (value nomadperf) == (decode \"" ++ fp ++ "\")")
        ans
        (Right $ Topo.mkTopology
          (Topo.TorusDense
            52
            [
              Types.AWS Types.EU_CENTRAL_1
            , Types.AWS Types.AP_SOUTHEAST_2
            , Types.AWS Types.US_EAST_1
            ]
            (\_ -> Just 1)
          )
          (Just (Types.AWS Types.EU_CENTRAL_1))
        )
  ]

benchTorusDense52 :: Types.Topology
benchTorusDense52 = Types.Topology {
  Types.coreNodes = [
  -- EU nodes x 18.
    Types.Node {Types.name = "node-0",  Types.nodeId =  0, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-1", "node-2", "node-3", "node-51","node-18","node-36"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-3",  Types.nodeId =  3, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-4", "node-5", "node-6", "node-0", "node-21","node-39"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-6",  Types.nodeId =  6, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-7", "node-8", "node-9", "node-3", "node-24","node-42"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-9",  Types.nodeId =  9, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-10","node-11","node-12","node-6", "node-27","node-45"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-12", Types.nodeId = 12, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-13","node-14","node-15","node-9", "node-30","node-48"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-15", Types.nodeId = 15, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-16","node-17","node-18","node-12","node-33","node-51"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-18", Types.nodeId = 18, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-19","node-20","node-21","node-15","node-36","node-0" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-21", Types.nodeId = 21, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-22","node-23","node-24","node-18","node-39","node-3" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-24", Types.nodeId = 24, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-25","node-26","node-27","node-21","node-42","node-6" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-27", Types.nodeId = 27, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-28","node-29","node-30","node-24","node-45","node-9" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-30", Types.nodeId = 30, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-31","node-32","node-33","node-27","node-48","node-12"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-33", Types.nodeId = 33, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-34","node-35","node-36","node-30","node-51","node-15"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-36", Types.nodeId = 36, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-37","node-38","node-39","node-33","node-0", "node-18"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-39", Types.nodeId = 39, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-40","node-41","node-42","node-36","node-3", "node-21"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-42", Types.nodeId = 42, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-43","node-44","node-45","node-39","node-6", "node-24"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-45", Types.nodeId = 45, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-46","node-47","node-48","node-42","node-9", "node-27"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-48", Types.nodeId = 48, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-49","node-50","node-51","node-45","node-12","node-30"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-51", Types.nodeId = 51, Types.region = Types.AWS Types.EU_CENTRAL_1, Types.producers = ["node-1", "node-2", "node-0", "node-48","node-15","node-33"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  -- AP nodes x 17.
  , Types.Node {Types.name = "node-1",  Types.nodeId =  1, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-2", "node-0", "node-4", "node-49","node-16","node-34"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-4",  Types.nodeId =  4, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-5", "node-3", "node-7", "node-1", "node-19","node-37"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-7",  Types.nodeId =  7, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-8", "node-6", "node-10","node-4", "node-22","node-40"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-10", Types.nodeId = 10, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-11","node-9", "node-13","node-7", "node-25","node-43"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-13", Types.nodeId = 13, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-14","node-12","node-16","node-10","node-28","node-46"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-16", Types.nodeId = 16, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-17","node-15","node-19","node-13","node-31","node-49"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-19", Types.nodeId = 19, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-20","node-18","node-22","node-16","node-34","node-1" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-22", Types.nodeId = 22, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-23","node-21","node-25","node-19","node-37","node-4" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-25", Types.nodeId = 25, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-26","node-24","node-28","node-22","node-40","node-7" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-28", Types.nodeId = 28, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-29","node-27","node-31","node-25","node-43","node-10"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-31", Types.nodeId = 31, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-32","node-30","node-34","node-28","node-46","node-13"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-34", Types.nodeId = 34, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-35","node-33","node-37","node-31","node-49","node-16"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-37", Types.nodeId = 37, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-38","node-36","node-40","node-34","node-1", "node-19"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-40", Types.nodeId = 40, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-41","node-39","node-43","node-37","node-4", "node-22"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-43", Types.nodeId = 43, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-44","node-42","node-46","node-40","node-7", "node-25"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-46", Types.nodeId = 46, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-47","node-45","node-49","node-43","node-10","node-28"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-49", Types.nodeId = 49, Types.region = Types.AWS Types.AP_SOUTHEAST_2, Types.producers = ["node-50","node-48","node-1", "node-46","node-13","node-31"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  -- US nodes x 17.
  , Types.Node {Types.name = "node-2",  Types.nodeId =  2, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-0", "node-1", "node-5", "node-50","node-17","node-35"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-5",  Types.nodeId =  5, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-3", "node-4", "node-8", "node-2", "node-20","node-38"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-8",  Types.nodeId =  8, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-6", "node-7", "node-11","node-5", "node-23","node-41"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-11", Types.nodeId = 11, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-9", "node-10","node-14","node-8", "node-26","node-44"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-14", Types.nodeId = 14, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-12","node-13","node-17","node-11","node-29","node-47"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-17", Types.nodeId = 17, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-15","node-16","node-20","node-14","node-32","node-50"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-20", Types.nodeId = 20, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-18","node-19","node-23","node-17","node-35","node-2" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-23", Types.nodeId = 23, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-21","node-22","node-26","node-20","node-38","node-5" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-26", Types.nodeId = 26, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-24","node-25","node-29","node-23","node-41","node-8" ], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-29", Types.nodeId = 29, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-27","node-28","node-32","node-26","node-44","node-11"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-32", Types.nodeId = 32, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-30","node-31","node-35","node-29","node-47","node-14"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-35", Types.nodeId = 35, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-33","node-34","node-38","node-32","node-50","node-17"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-38", Types.nodeId = 38, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-36","node-37","node-41","node-35","node-2", "node-20"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-41", Types.nodeId = 41, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-39","node-40","node-44","node-38","node-5", "node-23"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-44", Types.nodeId = 44, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-42","node-43","node-47","node-41","node-8", "node-26"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-47", Types.nodeId = 47, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-45","node-46","node-50","node-44","node-11","node-29"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  , Types.Node {Types.name = "node-50", Types.nodeId = 50, Types.region = Types.AWS Types.US_EAST_1, Types.producers = ["node-48","node-49","node-2", "node-47","node-14","node-32"], Types.org = "IOHK", Types.pools = Just 1, Types.stakePool = Just True}
  ]
, Types.relayNodes = [
    Types.Node {
      Types.name = "explorer"
    , Types.nodeId = 52
    , Types.region = Types.AWS Types.EU_CENTRAL_1
    , Types.producers = [
        "node-0", "node-1", "node-2", "node-3", "node-4"
      , "node-5", "node-6", "node-7", "node-8", "node-9"
      , "node-10","node-11","node-12","node-13","node-14"
      , "node-15","node-16","node-17","node-18","node-19"
      , "node-20","node-21","node-22","node-23","node-24"
      , "node-25","node-26","node-27","node-28","node-29"
      , "node-30","node-31","node-32","node-33","node-34"
      , "node-35","node-36","node-37","node-38","node-39"
      , "node-40","node-41","node-42","node-43","node-44"
      , "node-45","node-46","node-47","node-48","node-49"
      , "node-50","node-51"
      ]
    , Types.org = "IOHK"
    , Types.pools = Nothing
    , Types.stakePool = Nothing
    }
  ]
}
