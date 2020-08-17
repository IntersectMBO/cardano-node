{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.CalculateMinFee
  ( golden_shelleyTransactionCalculateMinFee
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified System.IO as IO

{- HLINT ignore "Use camelCase" -}

golden_shelleyTransactionCalculateMinFee :: Property
golden_shelleyTransactionCalculateMinFee = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  protocolParamsJsonFile <- noteInputFile "test/data/golden/shelley/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- noteInputFile "test/data/golden/shelley/transaction-calculate-min-fee/tx-body-file"
  minFeeTxtFile <- noteTempFile tempDir "min-fee.txt"

  minFeeTxt <- execCardanoCLI
    [ "shelley","transaction","calculate-min-fee"
    , "--tx-in-count", "32"
    , "--tx-out-count", "27"
    , "--byron-witness-count", "5"
    , "--witness-count", "10"
    , "--testnet-magic", "4036000900"
    , "--protocol-params-file", protocolParamsJsonFile
    , "--tx-body-file", txBodyFile
    ]

  liftIO $ IO.writeFile minFeeTxtFile minFeeTxt

  assertFileOccurences 1 "2541502" minFeeTxtFile
  assertFileLines (== 1) minFeeTxtFile
  assertEndsWithSingleNewline minFeeTxtFile
