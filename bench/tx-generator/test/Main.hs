{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

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

sizedMetadata :: TestTree
sizedMetadata = testGroup "properties of the CBOR encoding relevant for generating sized metadat"
  [ testCase "Shelley metadata map costs"          $ assertBool "metadata map costs" prop_mapCostsShelley
  , testCase "Shelley metadata ByteString costs"   $ assertBool "metadata ByteString costs" prop_bsCostsShelley
  , testCase "Allegra metadata map costs"          $ assertBool "metadata map costs" prop_mapCostsAllegra
  , testCase "Allegra metadata ByteString costs"   $ assertBool "metadata ByteString costs" prop_bsCostsAllegra
  , testCase "Mary metadata map costs"             $ assertBool "metadata map costs" prop_mapCostsMary
  , testCase "Mary metadata ByteString costs"      $ assertBool "metadata ByteString costs" prop_bsCostsMary
  , testCase "Alonzo metadata map costs"           $ assertBool "metadata map costs" prop_mapCostsAlonzo
  , testCase "Alonzo metadata ByteString costs"    $ assertBool "metadata ByteString costs" prop_bsCostsAlonzo
  , testCase "Babbage metadata map costs"           $ assertBool "metadata map costs" prop_mapCostsBabbage
  , testCase "Babbage metadata ByteString costs"    $ assertBool "metadata ByteString costs" prop_bsCostsBabbage
  , testCase "Test mkMetadata" $ assertBool "" True --WIP
  ]
