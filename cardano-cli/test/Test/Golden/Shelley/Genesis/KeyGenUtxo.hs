{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenUtxo
  ( golden_shelleyGenesisKeyGenUtxo
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenUtxo :: Property
golden_shelleyGenesisKeyGenUtxo = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  utxoVerificationKeyFile <- noteTempFile tempDir "utxo.vkey"
  utxoSigningKeyFile <- noteTempFile tempDir "utxo.skey"

  void $ execCardanoCLI
    [ "shelley","genesis","key-gen-utxo"
    , "--verification-key-file", utxoVerificationKeyFile
    , "--signing-key-file", utxoSigningKeyFile
    ]

  assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519" $ utxoVerificationKeyFile
  assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" $ utxoSigningKeyFile

  assertEndsWithSingleNewline utxoVerificationKeyFile
  assertEndsWithSingleNewline utxoSigningKeyFile
