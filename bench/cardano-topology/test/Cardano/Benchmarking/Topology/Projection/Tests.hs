{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Topology.Projection.Tests (tests) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: aeson.
import qualified Data.Aeson           as Aeson
-- Package: tasty.
import qualified Test.Tasty           as Tasty
-- Package: tasty-hunit.
import           Test.Tasty.HUnit
-- Package: self.
import qualified Cardano.Benchmarking.Topology.Projection as Projection
import qualified Paths_cardano_topology as Paths

--------------------------------------------------------------------------------

tests :: Tasty.TestTree
tests =  Tasty.testGroup "Cardano.Benchmarking.Topology.Projection"
  [
    projection
  , projectionP2P
  , projectionExplorer
  , projectionChainDB
  ]

projection :: Tasty.TestTree
projection = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Projection.projection" $
  map
    (\(profileName, projections) -> Tasty.testGroup profileName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let profileDir   = "data/test/" ++ profileName ++ "/"
          topologyPath   <- Paths.getDataFileName $ profileDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ profileDir ++ projectionName
          eitherTopology   <- Aeson.eitherDecodeFileStrict topologyPath
          eitherProjection <- Aeson.eitherDecodeFileStrict projectionPath
          case (eitherTopology, eitherProjection) of
            (Right topology, Right proj) ->
              assertEqual
                (profileName ++ "/" ++ projectionName ++ " == (projection \"" ++ show projectionId ++ "\")")
                proj                                                -- expected
                (Projection.projection topology projectionId 30000) -- got
            errors -> fail (show errors)
        )
        projections
    )
    [ (
        "10-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        , ( 3, "node-3.json")
        , ( 4, "node-4.json")
        , ( 5, "node-5.json")
        , ( 6, "node-6.json")
        , ( 7, "node-7.json")
        , ( 8, "node-8.json")
        , ( 9, "node-9.json")
        ]
      )
    , (
        "ci-test-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        ]
      )
    , (
        "ci-test-dense10-coay"
      , [
          ( 0, "node-0.json")
        ]
      )
    , (
        "ci-test-nomadperf-nop2p-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        ]
      )
    , (
        "default-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        , ( 3, "node-3.json")
        , ( 4, "node-4.json")
        , ( 5, "node-5.json")
        ]
      )
    , (
        "default-nomadperf-nop2p-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        , ( 3, "node-3.json")
        , ( 4, "node-4.json")
        , ( 5, "node-5.json")
        ]
      )
    , (
        "fast-solo-coay"
      , [
          ( 0, "node-0.json")
        ]
      )
    , (
        "forge-stress-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        ]
      )
    , (
        "model-value-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        , ( 3, "node-3.json")
        ]
      )
    , (
        "trace-bench-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        , ( 3, "node-3.json")
        , ( 4, "node-4.json")
        , ( 5, "node-5.json")
        ]
      )
    , (
        "value-nomadperf-nop2p-coay"
      --  (  0, "node-0.json" )
      --, (  1, "node-1.json" )
      -- ...
      --, ( 51, "node-51.json")
      --, ( 52, "explorer.json")
      , map (\i -> (i, "node-" ++ show i ++ ".json")) [0..51]
      )
    ]

projectionP2P :: Tasty.TestTree
projectionP2P = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Projection.projectionP2P" $
  map
    (\(profileName, projections) -> Tasty.testGroup profileName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let profileDir   = "data/test/" ++ profileName ++ "/"
          topologyPath   <- Paths.getDataFileName $ profileDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ profileDir ++ projectionName
          eitherTopology   <- Aeson.eitherDecodeFileStrict topologyPath
          eitherProjection <- Aeson.eitherDecodeFileStrict projectionPath
          case (eitherTopology, eitherProjection) of
            (Right topology, Right projP2P) ->
              assertEqual
                (profileName ++ "/" ++ projectionName ++ " == (projectionP2P \"" ++ show projectionId ++ "\")")
                projP2P                                                -- expected
                (Projection.projectionP2P topology projectionId 30000) -- got
            errors -> fail (show errors)
        )
        projections
    )
    [
      (
        "6-dense-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        , ( 3, "node-3.json")
        , ( 4, "node-4.json")
        , ( 5, "node-5.json")
        ]
      )
    , (
        "ci-test-nomadperf-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        ]
      )
    , (
        "ci-test-p2p-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        ]
      )
    , (
        "default-nomadperf-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        , ( 3, "node-3.json")
        , ( 4, "node-4.json")
        , ( 5, "node-5.json")
        ]
      )
    , (
        "forge-stress-p2p-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
        , ( 2, "node-2.json")
        ]
      )
    , (
        "value-volt-nomadperf-coay"
      --  (  0, "node-0.json" )
      --, (  1, "node-1.json" )
      -- ...
      --, ( 51, "node-51.json")
      --, ( 52, "explorer.json")
      , map (\i -> (i, "node-" ++ show i ++ ".json")) [0..51]
      )
    ]

projectionExplorer :: Tasty.TestTree
projectionExplorer = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Projection.projectionExplorer" $
  map
    (\(profileName, projections) -> Tasty.testGroup profileName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let profileDir   = "data/test/" ++ profileName ++ "/"
          projectionPath <- Paths.getDataFileName $ profileDir ++ projectionName
          eitherProjection <- Aeson.eitherDecodeFileStrict projectionPath
          case eitherProjection of
            (Right projExplorer) ->
              assertEqual
                (profileName ++ "/" ++ projectionName ++ " == (projectionExplorer \"" ++ show projectionId ++ "\")")
                projExplorer                                                -- expected
                (Projection.projectionExplorer projectionId 30000) -- got
            errors -> fail (show errors)
        )
        projections
    )
    [ (
        "chainsync-early-alonzo-coay"
      , [(  1, "explorer.json")]
      )
    , (
        "ci-test-nomadperf-coay"
      , [(  2, "explorer.json")]
      )
    , (
        "ci-test-nomadperf-nop2p-coay"
      , [(  2, "explorer.json")]
      )
    , (
        "default-nomadperf-coay"
      , [(  6, "explorer.json")]
      )
    , (
        "default-nomadperf-nop2p-coay"
      , [(  6, "explorer.json")]
      )
    , (
        "value-volt-nomadperf-coay"
      , [( 52, "explorer.json")]
      )
    , (
        "value-nomadperf-nop2p-coay"
      , [( 52, "explorer.json")]
      )
    ]

projectionChainDB :: Tasty.TestTree
projectionChainDB = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Projection.projectionChainDB" [
    (\(profileName, projections) -> Tasty.testGroup profileName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let profileDir   = "data/test/" ++ profileName ++ "/"
          topologyPath   <- Paths.getDataFileName $ profileDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ profileDir ++ projectionName
          eitherTopology   <- Aeson.eitherDecodeFileStrict topologyPath
          eitherProjection <- Aeson.eitherDecodeFileStrict projectionPath
          case (eitherTopology, eitherProjection) of
            (Right topology, Right proj) ->
              assertEqual
                (profileName ++ "/" ++ projectionName ++ " == (projectionChainDB \"" ++ show (projectionId :: Int) ++ "\")")
                proj                                    -- expected
                (Projection.projectionChainDB topology) -- got
            errors -> fail (show errors)
        )
        projections
    )
    (
      "chainsync-early-alonzo-coay"
    , [
        ( 0, "node-0.json")
      ]
    )
  ]
