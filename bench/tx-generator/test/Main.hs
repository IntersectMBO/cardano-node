{-# LANGUAGE Trustworthy #-}

module Main (main) where

import           Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

import           Cardano.Benchmarking.GeneratorTx.SizedMetadata

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =  testGroup "cardano-tx-generator"
  [
    sizedMetadata
  ]

sizedMetadata = testGroup "properties of the CBOR encoding relevant for generating sized metadat"
  [ testCase "Shelley metadata map costs" $ assertBool "metadata map costs" prop_mapCostsShelley
  , testCase "Shelley metadata ByteString costs" $ assertBool "metadata ByteString costs" prop_bsCostsShelley
  , testCase "Allegra metadata map costs" $ assertBool "metadata map costs" prop_mapCostsAllegra
  , testCase "Allegra metadata ByteString costs" $ assertBool "metadata ByteString costs" prop_bsCostsAllegra
  , testCase "Mary metadata map costs"    $ assertBool "metadata map costs" prop_mapCostsMary
  , testCase "Marymetadata ByteString costs"    $ assertBool "metadata ByteString costs" prop_bsCostsMary
  , testCase "Test mkMetadata" $ assertBool "" True --WIP
  ]
