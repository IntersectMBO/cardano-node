{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Main (main) where

import           Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

import           Cardano.Benchmarking.GeneratorTx.SizedMetadata
import           NestedTxTest (nestedTxTests)
import           TestnetDiscoveryTest (testnetDiscoveryTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =  testGroup "cardano-tx-generator"
  [
    sizedMetadata
  , nestedTxTests
  , testnetDiscoveryTests
  ]

sizedMetadata :: TestTree
sizedMetadata = testGroup "properties of the CBOR encoding relevant for generating sized metadat"
  [ testCase "Conway metadata map costs"           $ assertBool "metadata map costs" prop_mapCostsConway
  , testCase "Conway metadata ByteString costs"    $ assertBool "metadata ByteString costs" prop_bsCostsConway
  , testCase "Dijkstra metadata map costs"         $ assertBool "metadata map costs" prop_mapCostsDijkstra
  , testCase "Dijkstra metadata ByteString costs"  $ assertBool "metadata ByteString costs" prop_bsCostsDijkstra
  ]
