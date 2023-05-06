{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Tx.Tx
  ( golden_shelleyTx
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Create tx body
--   3. Sign tx body
--   4. Check the TextEnvelope serialization format has not changed.
golden_shelleyTx :: Property
golden_shelleyTx = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  let referenceTx = "test/data/golden/alonzo/tx"

  -- Key filepaths
  paymentSignKey <- noteInputFile "test/data/golden/shelley/transaction-sign/utxo.skey"
  transactionFile <- noteTempFile tempDir "tx-file"
  transactionBodyFile <- noteTempFile tempDir "tx-body-file"

  -- Create transaction body
  void $ execCardanoCLI
    [ "transaction", "build-raw"
    , "--alonzo-era"
    , "--tx-in", "f62cd7bc15d8c6d2c8519fb8d13c57c0157ab6bab50af62bc63706feb966393d#0"
    , "--tx-out", "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh+1199989833223"
    , "--tx-out", "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4+10000000"
    , "--fee", "166777"
    , "--out-file", transactionBodyFile
    ]

  -- Sign transaction
  void $ execCardanoCLI
    [ "transaction", "sign"
    , "--tx-body-file", transactionBodyFile
    , "--signing-key-file", paymentSignKey
    , "--testnet-magic", "42"
    , "--out-file", transactionFile
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTxCddlFormat referenceTx transactionFile
