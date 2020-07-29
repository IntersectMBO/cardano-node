{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.GenesisUTxOKeys
  ( golden_shelleyGenesisUTxOKeys
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyGenesisUTxOKeys :: Property
golden_shelleyGenesisUTxOKeys = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- OP.noteInputFile "test/Test/golden/shelley/keys/genesis_utxo_keys/verification_key"
  referenceSignKey <- OP.noteInputFile "test/Test/golden/shelley/keys/genesis_utxo_keys/signing_key"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "genesis-utxo-verification-key-file"
  signKey <- OP.noteTempFile tempDir "genesis-utxo-signing-key-file"

  -- Generate payment verification key
  void $ OP.execCardanoCLI
    [ "shelley","genesis","key-gen-utxo"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisUTxOKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisUTxOKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  OP.checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
