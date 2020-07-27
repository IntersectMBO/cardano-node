{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Transaction.CalculateMinFee
  ( golden_shelleyTransactionCalculateMinFee
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified System.IO as IO
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyTransactionCalculateMinFee :: Property
golden_shelleyTransactionCalculateMinFee = OP.propertyOnce $ OP.workspace "tmp/transaction-calculate-min-fee" $ \tempDir -> do
  protocolParamsJsonFile <- OP.noteInputFile "test/Test/golden/shelley/transaction-calculate-min-fee/protocol-params.json"
  txBodyFile <- OP.noteInputFile "test/Test/golden/shelley/transaction-calculate-min-fee/tx-body-file"
  minFeeTxtFile <- OP.noteTempFile tempDir "min-fee.txt"

  minFeeTxt <- OP.noteEvalM $ OP.execCardanoCLI
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

  OP.assertFileOccurences 1 "2541502" minFeeTxtFile
  OP.assertFileLines (== 1) minFeeTxtFile
  OP.assertEndsWithSingleNewline minFeeTxtFile
