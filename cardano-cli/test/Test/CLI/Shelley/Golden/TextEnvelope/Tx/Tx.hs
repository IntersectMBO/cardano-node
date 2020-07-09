{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Tx.Tx
  ( golden_shelleyTx
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. Generate a key pair
--   2. Create tx body
--   3. Sign tx body
--   4. Check the TextEnvelope serialization format has not changed.
golden_shelleyTx :: Property
golden_shelleyTx =
  propertyOnce $ do
    -- Reference keys
    let referenceTx = "test/Test/golden/shelley/tx/tx"

    -- Key filepaths
    let paymentVerKey = "payment-verification-key-file"
        paymentSignKey = "payment-signing-key-file"
        transactionFile = "tx-file"
        transactionBodyFile = "tx-body-file"
        createdFiles = [ paymentVerKey
                       , paymentSignKey
                       , transactionBodyFile
                       , transactionBodyFile
                       , transactionFile]


    -- Generate payment signing key to sign transaction
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--verification-key-file", paymentVerKey
                               , "--signing-key-file", paymentSignKey
                               ]

    -- Create transaction body
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","transaction", "build-raw"
                               , "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                               , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
                               , "--fee", "1000000"
                               , "--ttl", "500000"
                               , "--out-file", transactionBodyFile
                               ]

    -- Sign transaction
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","transaction", "sign"
                               , "--tx-body-file", transactionBodyFile
                               , "--signing-key-file", paymentSignKey
                               , "--mainnet"
                               , "--out-file", transactionFile
                               ]

    assertFilesExist createdFiles

    let txType = textEnvelopeType AsShelleyTx

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles txType referenceTx transactionFile

    liftIO $ fileCleanup createdFiles
    H.success
