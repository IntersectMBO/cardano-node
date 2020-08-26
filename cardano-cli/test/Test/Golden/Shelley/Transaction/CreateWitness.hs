{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.CreateWitness
  ( golden_shelleyTransactionAllMultiSigWitness
  , golden_shelleyTransactionAnyMultiSigWitness
  , golden_shelleyTransactionAtLeastMultiSigWitness
  , golden_shelleyTransactionSigningKeyWitness
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Hedgehog (Property)
import           Test.OptParse


{- HLINT ignore "Use camelCase" -}

txIn :: String
txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

txOut :: String
txOut = "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

golden_shelleyTransactionAllMultiSigWitness :: Property
golden_shelleyTransactionAllMultiSigWitness = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  txBodyOutFile <- noteTempFile tempDir "tx-body-out"

  -- Create tx body file
  void $ execCardanoCLI
    [ "shelley","transaction","build-raw"
    , "--tx-in", txIn
    , "--tx-out", txOut
    , "--ttl", "60"
    , "--fee", "12"
    , "--tx-body-file", txBodyOutFile
    ]

  assertFileOccurences 1 "TxUnsignedShelley" txBodyOutFile
  assertEndsWithSingleNewline txBodyOutFile


  -- Create all multisig witness
  allMultiSigWitnessOutFile <- noteTempFile tempDir "all-multisig-witness"
  allScriptWitnessFile <- noteInputFile "test/data/golden/shelley/multisig/scripts/all"
  void $ execCardanoCLI
    [ "shelley","transaction","witness"
    , "--tx-body-file", txBodyOutFile
    , "--script-file", allScriptWitnessFile
    , "--mainnet"
    , "--out-file", allMultiSigWitnessOutFile
    ]

  assertFileOccurences 1 "TxWitnessShelley" allMultiSigWitnessOutFile
  assertEndsWithSingleNewline txBodyOutFile


golden_shelleyTransactionAnyMultiSigWitness :: Property
golden_shelleyTransactionAnyMultiSigWitness = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  txBodyOutFile <- noteTempFile tempDir "tx-body-out"

  -- Create tx body file
  void $ execCardanoCLI
    [ "shelley","transaction","build-raw"
    , "--tx-in", txIn
    , "--tx-out", txOut
    , "--ttl", "60"
    , "--fee", "12"
    , "--tx-body-file", txBodyOutFile
    ]

  -- Create all multisig witness
  allMultiSigWitnessOutFile <- noteTempFile tempDir "any-multisig-witness"
  anyScriptWitnessFile <- noteInputFile "test/data/golden/shelley/multisig/scripts/any"
  void $ execCardanoCLI
    [ "shelley","transaction","witness"
    , "--tx-body-file", txBodyOutFile
    , "--script-file", anyScriptWitnessFile
    , "--mainnet"
    , "--out-file", allMultiSigWitnessOutFile
    ]

  assertFileOccurences 1 "TxWitnessShelley" allMultiSigWitnessOutFile
  assertEndsWithSingleNewline txBodyOutFile

golden_shelleyTransactionAtLeastMultiSigWitness :: Property
golden_shelleyTransactionAtLeastMultiSigWitness = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  txBodyOutFile <- noteTempFile tempDir "tx-body-out"

  -- Create tx body file
  void $ execCardanoCLI
    [ "shelley","transaction","build-raw"
    , "--tx-in", txIn
    , "--tx-out", txOut
    , "--ttl", "60"
    , "--fee", "12"
    , "--tx-body-file", txBodyOutFile
    ]

  -- Create all multisig witness
  atLeastMultiSigWitnessOutFile <- noteTempFile tempDir "atleast-multisig-witness"
  atLeastScriptWitnessFile <- noteInputFile "test/data/golden/shelley/multisig/scripts/atleast"
  void $ execCardanoCLI
    [ "shelley","transaction","witness"
    , "--tx-body-file", txBodyOutFile
    , "--script-file", atLeastScriptWitnessFile
    , "--mainnet"
    , "--out-file", atLeastMultiSigWitnessOutFile
    ]

  assertFileOccurences 1 "TxWitnessShelley" atLeastMultiSigWitnessOutFile
  assertEndsWithSingleNewline txBodyOutFile

golden_shelleyTransactionSigningKeyWitness :: Property
golden_shelleyTransactionSigningKeyWitness = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  txBodyOutFile <- noteTempFile tempDir "tx-body-out"

  -- Create tx body file
  void $ execCardanoCLI
    [ "shelley","transaction","build-raw"
    , "--tx-in", txIn
    , "--tx-out", txOut
    , "--ttl", "60"
    , "--fee", "12"
    , "--tx-body-file", txBodyOutFile
    ]

  -- Create all multisig witness
  witnessOutFile <- noteTempFile tempDir "signingkey-witness"
  signingKeyFile <- noteInputFile "test/data/golden/shelley/keys/payment_keys/signing_key"
  void $ execCardanoCLI
    [ "shelley","transaction","witness"
    , "--tx-body-file", txBodyOutFile
    , "--signing-key-file", signingKeyFile
    , "--mainnet"
    , "--out-file", witnessOutFile
    ]

  assertFileOccurences 1 "TxWitnessShelley" witnessOutFile
  assertEndsWithSingleNewline txBodyOutFile
