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
  ]

projection :: Tasty.TestTree
projection = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Projection.projection" $
  map
    (\(topologyName, projections) -> Tasty.testGroup topologyName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let profileDir   = "data/test/" ++ topologyName ++ "/"
          topologyPath   <- Paths.getDataFileName $ profileDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ profileDir ++ projectionName
          eitherTopology   <- Aeson.eitherDecodeFileStrict topologyPath
          eitherProjection <- Aeson.eitherDecodeFileStrict projectionPath
          case (eitherTopology, eitherProjection) of
            (Right topology, Right proj) ->
              assertEqual
                ("node-specs.json == (nodeSpecs \"" ++ topologyName ++ "\")")
                proj                                               -- expected
                (Projection.projection topology projectionId 30000) -- got
            errors -> fail (show errors)
        )
        projections
    )
    [ (
        "ci-test-coay"
      , [
          ( 0, "node-0.json")
        , ( 1, "node-1.json")
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
    (\(topologyName, projections) -> Tasty.testGroup topologyName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let profileDir   = "data/test/" ++ topologyName ++ "/"
          topologyPath   <- Paths.getDataFileName $ profileDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ profileDir ++ projectionName
          eitherTopology   <- Aeson.eitherDecodeFileStrict topologyPath
          eitherProjection <- Aeson.eitherDecodeFileStrict projectionPath
          case (eitherTopology, eitherProjection) of
            (Right topology, Right projP2P) ->
              assertEqual
                ("node-specs.json == (nodeSpecs \"" ++ topologyName ++ "\")")
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
    (\(topologyName, projections) -> Tasty.testGroup topologyName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let profileDir   = "data/test/" ++ topologyName ++ "/"
          topologyPath   <- Paths.getDataFileName $ profileDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ profileDir ++ projectionName
          eitherTopology   <- Aeson.eitherDecodeFileStrict topologyPath
          eitherProjection <- Aeson.eitherDecodeFileStrict projectionPath
          case (eitherTopology, eitherProjection) of
            (Right topology, Right projExplorer) ->
              assertEqual
                ("node-specs.json == (nodeSpecs \"" ++ topologyName ++ "\")")
                projExplorer                                                -- expected
                (Projection.projectionExplorer topology projectionId 30000) -- got
            errors -> fail (show errors)
        )
        projections
    )
    [ (
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
