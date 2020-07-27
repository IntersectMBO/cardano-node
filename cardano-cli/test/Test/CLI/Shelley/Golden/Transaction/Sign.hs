{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Transaction.Sign
  ( golden_shelleyTransactionSign
  )
where

import           Cardano.Prelude

import           Hedgehog                       ( Property )

import qualified Test.OptParse                 as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyTransactionSign :: Property
golden_shelleyTransactionSign =
  OP.propertyOnce
    $ OP.workspace "tmp/transaction-calculate-min-fee"
    $ \tempDir -> do
        txBodyFile <- OP.noteInputFile
          "test/Test/golden/shelley/transaction-sign/tx-body-file"
        initialUtxo1SigningKeyFile <- OP.noteInputFile
          "test/Test/golden/shelley/transaction-sign/initial-utxo1.skey"
        initialUtxo2SigningKeyFile <- OP.noteInputFile
          "test/Test/golden/shelley/transaction-sign/initial-utxo2.skey"
        utxoSigningKeyFile <- OP.noteInputFile
          "test/Test/golden/shelley/transaction-sign/utxo.skey"
        stakeSigningKeyFile <- OP.noteInputFile
          "test/Test/golden/shelley/transaction-sign/stake.skey"
        nodeColdSigningKeyFile <- OP.noteInputFile
          "test/Test/golden/shelley/transaction-sign/node-cold.skey"
        signedTransactionFile <- OP.noteTempFile tempDir "signed.tx"
        transactionPoolRegSignedFile <- OP.noteTempFile tempDir
                                                        "tx-pool-reg.signed"

        -- Defaults to signing a Mainnet transaction

        void . OP.noteEvalM $ OP.execCardanoCLI
          [ "shelley"
          , "transaction"
          , "sign"
          , "--mainnet"
          , "--tx-body-file"
          , txBodyFile
          , "--signing-key-file"
          , initialUtxo1SigningKeyFile
          , "--tx-file"
          , signedTransactionFile
          ]

        OP.assertFileOccurences 1 "TxSignedShelley" signedTransactionFile
        OP.assertEndsWithSingleNewline signedTransactionFile

        -- Sign for a testnet with a testnet network magic of 11, but use two signing keys

        void . OP.noteEvalM $ OP.execCardanoCLI
          [ "shelley"
          , "transaction"
          , "sign"
          , "--mainnet"
          , "--tx-body-file"
          , txBodyFile
          , "--signing-key-file"
          , initialUtxo1SigningKeyFile
          , "--signing-key-file"
          , initialUtxo2SigningKeyFile
          , "--tx-file"
          , signedTransactionFile
          ]

        OP.assertFileOccurences 1 "TxSignedShelley" signedTransactionFile
        OP.assertEndsWithSingleNewline signedTransactionFile

        -- Sign a pool registration transaction.
        -- TODO: This needs to use an unsigned tx with a registration certificate

        void . OP.noteEvalM $ OP.execCardanoCLI
          [ "shelley"
          , "transaction"
          , "sign"
          , "--mainnet"
          , "--tx-body-file"
          , txBodyFile
          , "--signing-key-file"
          , utxoSigningKeyFile
          , "--signing-key-file"
          , stakeSigningKeyFile
          , "--signing-key-file"
          , nodeColdSigningKeyFile
          , "--tx-file"
          , transactionPoolRegSignedFile
          ]

        OP.assertFileOccurences 1 "TxSignedShelley" transactionPoolRegSignedFile
        OP.assertEndsWithSingleNewline transactionPoolRegSignedFile
