{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.CalculateMinFee
  ( golden_shelleyTransactionCalculateMinFee
  , golden_shelleyTransactionCalculateMinFee_outFile
  ) where

import           Hedgehog (Property, (===))
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Golden as H
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyTransactionCalculateMinFee :: Property
golden_shelleyTransactionCalculateMinFee = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  protocolParamsJsonFile <- noteInputFile "test/data/golden/shelley/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  minFeeTxtFile <- noteTempFile tempDir "min-fee.txt"

  minFeeTxt <- execCardanoCLI
    [ "transaction","calculate-min-fee"
    , "--tx-in-count", "32"
    , "--tx-out-count", "27"
    , "--byron-witness-count", "5"
    , "--witness-count", "10"
    , "--testnet-magic", "4036000900"
    , "--protocol-params-file", protocolParamsJsonFile
    , "--tx-body-file", txBodyFile
    ]

  H.writeFile minFeeTxtFile minFeeTxt

  H.assertFileOccurences 1 "5083100" minFeeTxtFile
  H.assertFileLines (== 1) minFeeTxtFile
  H.assertEndsWithSingleNewline minFeeTxtFile

golden_shelleyTransactionCalculateMinFee_outFile :: Property
golden_shelleyTransactionCalculateMinFee_outFile = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  protocolParamsJsonFile <- noteInputFile "test/data/golden/shelley/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  outFile <- noteTempFile tempDir "out-file"

  minFeeTxt <- execCardanoCLI
    [ "transaction","calculate-min-fee"
    , "--out-file", outFile
    , "--tx-in-count", "32"
    , "--tx-out-count", "27"
    , "--byron-witness-count", "5"
    , "--witness-count", "10"
    , "--testnet-magic", "4036000900"
    , "--protocol-params-file", protocolParamsJsonFile
    , "--tx-body-file", txBodyFile
    ]

  minFeeTxt === ""

  resultJson <- H.readFile outFile

  H.diffVsGoldenFile resultJson "test/data/golden/shelley/tx/calculate-min-fee/out-file.json"
