{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise2
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H

-- | 1. We generate a payment signing key
--   2. We create a tx body
--   3. We sign the tx body with the generated payment signing key
prop_createTransaction :: Property
prop_createTransaction = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  paymentVerKey <- noteTempFile tempDir "payment-verification-key-file"
  paymentSignKey <- noteTempFile tempDir "payment-signing-key-file"
  transactionBodyFile <- noteTempFile tempDir "transaction-body"
  transactionFile <- noteTempFile tempDir "transaction-file"

  -- Generate payment signing key to sign transaction
  void $ execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", paymentVerKey
    , "--signing-key-file", paymentSignKey
    ]

  assertFilesExist [paymentVerKey, paymentSignKey]

  -- Create transaction body
  void $ execCardanoCLI
    [ "shelley","transaction", "build-raw"
    , "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
    , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
    , "--fee", "1000000"
    , "--ttl", "500000"
    , "--out-file", transactionBodyFile
    ]

  assertFilesExist [transactionBodyFile]

  -- Sign transaction
  void $ execCardanoCLI
    [ "shelley","transaction", "sign"
    , "--tx-body-file", transactionBodyFile
    , "--signing-key-file", paymentSignKey
    , "--mainnet"
    , "--out-file", transactionFile
    ]

  assertFilesExist [paymentVerKey, paymentSignKey, transactionBodyFile, transactionFile]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 2"
        [ ("prop_createTransaction", prop_createTransaction)
        ]
