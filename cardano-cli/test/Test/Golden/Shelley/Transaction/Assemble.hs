{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Assemble
  ( golden_shelleyTransactionAssembleWitness_AllMultiSig
  , golden_shelleyTransactionAssembleWitness_AnyMultiSig
  , golden_shelleyTransactionAssembleWitness_AtLeastMultiSig
  , golden_shelleyTransactionAssembleWitness_SigningKey
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

-- Check that we can assemble a txbody and a tx witness to form a transaction

golden_shelleyTransactionAssembleWitness_AllMultiSig :: Property
golden_shelleyTransactionAssembleWitness_AllMultiSig = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  allWitnessTx <- noteTempFile tempDir "all-witness-tx"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  allScriptWitnessFile <- noteInputFile "test/data/golden/shelley/witnesses/allScriptWitness"
  void $ execCardanoCLI
    [ "shelley","transaction","sign-witness"
    , "--tx-body-file", txBodyFile
    , "--witness-file", allScriptWitnessFile
    , "--out-file", allWitnessTx
    ]

  assertFileOccurences 1 "TxSignedShelley" allWitnessTx

golden_shelleyTransactionAssembleWitness_AnyMultiSig :: Property
golden_shelleyTransactionAssembleWitness_AnyMultiSig = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  anyWitnessTx <- noteTempFile tempDir "any-witness-tx"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  anyScriptWitnessFile <- noteInputFile "test/data/golden/shelley/witnesses/anyScriptWitness"
  void $ execCardanoCLI
    [ "shelley","transaction","sign-witness"
    , "--tx-body-file", txBodyFile
    , "--witness-file", anyScriptWitnessFile
    , "--out-file", anyWitnessTx
    ]

  assertFileOccurences 1 "TxSignedShelley" anyWitnessTx

golden_shelleyTransactionAssembleWitness_AtLeastMultiSig :: Property
golden_shelleyTransactionAssembleWitness_AtLeastMultiSig = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  atLeastWitnessTx <- noteTempFile tempDir "atLeast-witness-tx"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  atLeastScriptWitnessFile <- noteInputFile "test/data/golden/shelley/witnesses/anyScriptWitness"
  void $ execCardanoCLI
    [ "shelley","transaction","sign-witness"
    , "--tx-body-file", txBodyFile
    , "--witness-file", atLeastScriptWitnessFile
    , "--out-file", atLeastWitnessTx
    ]

  assertFileOccurences 1 "TxSignedShelley" atLeastWitnessTx

golden_shelleyTransactionAssembleWitness_SigningKey :: Property
golden_shelleyTransactionAssembleWitness_SigningKey = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  witnessTx <- noteTempFile tempDir "single-signing-key-witness-tx"
  txBodyFile <- noteInputFile "test/data/golden/shelley/tx/txbody"
  signingKeyWitnessFile <- noteInputFile "test/data/golden/shelley/witnesses/singleSigningKeyWitness"
  void $ execCardanoCLI
    [ "shelley","transaction","sign-witness"
    , "--tx-body-file", txBodyFile
    , "--witness-file", signingKeyWitnessFile
    , "--witness-file", signingKeyWitnessFile
    , "--out-file", witnessTx
    ]

  assertFileOccurences 1 "TxSignedShelley" witnessTx
