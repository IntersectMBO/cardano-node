{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.CliIntermediateFormat
  ( tests
  ) where

import           Control.Monad (void)

import           Hedgehog (Property, discover)
import           Test.Cardano.CLI.Util

import qualified Hedgehog
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | We test to make sure that we can deserialize a tx body in the intermediate format
prop_backwardsCompatibleCliFormat :: Property
prop_backwardsCompatibleCliFormat = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  txBodyFile <- noteInputFile "test/cardano-cli-test/files/golden/babbage/deprecated-cli-format.body"
  witness <- noteInputFile "test/cardano-cli-test/files/golden/babbage/tx-key-witness"
  initialUtxo1SigningKeyFile <- noteInputFile "test/cardano-cli-test/files/golden/shelley/keys/payment_keys/signing_key"
  signedTransactionFile <- noteTempFile tempDir "signed.tx"


  void $ execCardanoCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", initialUtxo1SigningKeyFile
    , "--tx-file", signedTransactionFile
    ]

  H.assertFileOccurences 1 "Tx BabbageEra" signedTransactionFile
  H.assertEndsWithSingleNewline signedTransactionFile

  void $ execCardanoCLI
    [ "transaction","assemble"
    , "--tx-body-file", txBodyFile
    , "--witness-file", witness
    , "--out-file", signedTransactionFile
    ]

  H.assertFileOccurences 1 "Tx BabbageEra" signedTransactionFile
  H.assertEndsWithSingleNewline signedTransactionFile

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover

