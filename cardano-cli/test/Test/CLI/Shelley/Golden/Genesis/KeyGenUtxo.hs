{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Genesis.KeyGenUtxo
  ( golden_shelleyGenesisKeyGenUtxo
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenUtxo :: Property
golden_shelleyGenesisKeyGenUtxo = OP.propertyOnce $ do
  OP.workspace "tmp/genesis-key-gen-utxo" $ \tempDir -> do
    utxoVerificationKeyFile <- OP.noteTempFile tempDir "utxo.vkey"
    utxoSigningKeyFile <- OP.noteTempFile tempDir "utxo.skey"

    void $ OP.execCardanoCLI
        [ "shelley","genesis","key-gen-utxo"
        , "--verification-key-file", utxoVerificationKeyFile
        , "--signing-key-file", utxoSigningKeyFile
        ]

    OP.assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519" $ utxoVerificationKeyFile
    OP.assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" $ utxoSigningKeyFile
