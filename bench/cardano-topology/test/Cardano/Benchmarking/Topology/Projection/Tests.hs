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
  ]

projection :: Tasty.TestTree
projection = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Projection.projection" $
  map
    (\(topologyName, projections) -> Tasty.testGroup topologyName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let topologyDir   = "data/test/" ++ topologyName ++ "/"
          let projectionDir = topologyDir
          topologyPath   <- Paths.getDataFileName $ topologyDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ projectionDir ++ projectionName
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
    ]

projectionP2P :: Tasty.TestTree
projectionP2P = Tasty.testGroup
  "Cardano.Benchmarking.Topology.Projection.projectionP2P" $
  map
    (\(topologyName, projections) -> Tasty.testGroup topologyName $
      map
        (\(projectionId, projectionName) -> testCase projectionName $ do
          let topologyDir   = "data/test/" ++ topologyName ++ "/"
          let projectionDir = topologyDir
          topologyPath   <- Paths.getDataFileName $ topologyDir ++ "topology.json"
          projectionPath <- Paths.getDataFileName $ projectionDir ++ projectionName
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
    [ (
        "value-volt-nomadperf-coay"
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
        , (10, "node-10.json")
        , (11, "node-11.json")
        ]
      )
    ]
