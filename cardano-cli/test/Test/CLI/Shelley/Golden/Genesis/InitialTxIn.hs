{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Genesis.InitialTxIn
  ( golden_shelleyGenesisInitialTxIn
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)

import qualified System.IO as IO
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisInitialTxIn :: Property
golden_shelleyGenesisInitialTxIn = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/keys/genesis_verification_keys/genesis-utxo.vkey"
  goldenUtxoHashFile <- OP.noteInputFile "test/Test/golden/shelley/keys/genesis_utxo_hashes/utxo_hash"
  utxoHashFile <- OP.noteTempFile tempDir "utxo_hash"

  utxoHash <- OP.execCardanoCLI
      [ "shelley","genesis","initial-txin"
      , "--testnet-magic", "16"
      , "--verification-key-file", verificationKeyFile
      ]

  liftIO $ IO.writeFile utxoHashFile utxoHash

  goldenUtxoHash <- OP.readFile goldenUtxoHashFile

  OP.equivalence utxoHash goldenUtxoHash
