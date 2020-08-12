{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.InitialTxIn
  ( golden_shelleyGenesisInitialTxIn
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified System.IO as IO
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisInitialTxIn :: Property
golden_shelleyGenesisInitialTxIn = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteInputFile "test/data/golden/shelley/keys/genesis_verification_keys/genesis-utxo.vkey"
  goldenUtxoHashFile <- noteInputFile "test/data/golden/shelley/keys/genesis_utxo_hashes/utxo_hash"
  utxoHashFile <- noteTempFile tempDir "utxo_hash"

  utxoHash <- execCardanoCLI
    [ "shelley","genesis","initial-txin"
    , "--testnet-magic", "16"
    , "--verification-key-file", verificationKeyFile
    ]

  liftIO $ IO.writeFile utxoHashFile utxoHash

  goldenUtxoHash <- OP.readFile goldenUtxoHashFile

  equivalence utxoHash goldenUtxoHash
