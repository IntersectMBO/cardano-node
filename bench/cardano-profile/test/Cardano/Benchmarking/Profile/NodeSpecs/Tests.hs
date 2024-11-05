{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.NodeSpecs.Tests (tests) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: aeson.
import qualified Data.Aeson           as Aeson
-- Package: tasty.
import qualified Test.Tasty           as Tasty
-- Package: tasty-hunit.
import           Test.Tasty.HUnit
-- Package: self.
import qualified Cardano.Benchmarking.Profile.NodeSpecs as NodeSpecs
import qualified Paths_cardano_profile as Paths

--------------------------------------------------------------------------------

tests :: Tasty.TestTree
tests =  Tasty.testGroup "Cardano.Benchmarking.Profile.NodeSpecs"
  [
    nodeSpecs
  ]

nodeSpecs :: Tasty.TestTree
nodeSpecs = Tasty.testGroup
  "Cardano.Benchmarking.Profile.NodeSpecs" $
  map
    (\profileName -> testCase profileName $ do
      let dir = "data/test/" ++ profileName ++ "/"
      profilePath   <- Paths.getDataFileName $ dir ++ "profile.json"
      topologyPath  <- Paths.getDataFileName $ dir ++ "topology.json"
      nodeSpecsPath <- Paths.getDataFileName $ dir ++ "node-specs.json"
      eitherProfile   <- Aeson.eitherDecodeFileStrict profilePath
      eitherTopology  <- Aeson.eitherDecodeFileStrict topologyPath
      eitherNodeSpecs <- Aeson.eitherDecodeFileStrict nodeSpecsPath
      case (eitherProfile, eitherTopology, eitherNodeSpecs) of
        (Right profile, Right topology, Right nodeSpecsMap) -> do
          assertEqual
            ("node-specs.json == (nodeSpecs \"" ++ profileName ++ "\")")
            nodeSpecsMap                            -- expected
            (NodeSpecs.nodeSpecs profile topology)  -- got
        errors -> fail (show errors)
    )
    [ "ci-test-coay"
    , "ci-test-dense10-coay"
    , "default-coay"
    , "fast-nomadperf-coay"
    , "chainsync-early-alonzo-coay"
    , "chainsync-early-byron-coay"
    ]
