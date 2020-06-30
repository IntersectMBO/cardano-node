{-# LANGUAGE OverloadedStrings #-}

module Test.Pioneers.Exercise2
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. We generate a payment signing key
--   2. We create a tx body
--   3. We sign the tx body with the generated payment signing key
prop_createTransaction :: Property
prop_createTransaction =
  propertyOnce $ do

    -- Key filepaths
    let paymentVerKey = "payment-verification-key-file"
        paymentSignKey = "payment-signing-key-file"
        transactionBodyFile = "transaction-body"
        transactionFile = "transactionFile"
        allFiles = [paymentVerKey, paymentSignKey, transactionBodyFile, transactionFile]

    -- Generate payment signing key to sign transaction
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--verification-key-file", paymentVerKey
                               , "--signing-key-file", paymentSignKey
                               ]

    assertFilesExist [paymentVerKey, paymentSignKey]

    -- Create transaction body
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","transaction", "build-raw"
                               , "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                               , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
                               , "--fee", "1000000"
                               , "--ttl", "500000"
                               , "--out-file", transactionBodyFile
                               ]

    assertFilesExist [transactionBodyFile]

    -- Sign transaction
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","transaction", "sign"
                               , "--tx-body-file", transactionBodyFile
                               , "--signing-key-file", paymentSignKey
                               , "--mainnet"
                               , "--out-file", transactionFile
                               ]


    assertFilesExist allFiles

    liftIO $ fileCleanup allFiles
    H.success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 2"
        [ ("prop_createTransaction", prop_createTransaction)
        ]
