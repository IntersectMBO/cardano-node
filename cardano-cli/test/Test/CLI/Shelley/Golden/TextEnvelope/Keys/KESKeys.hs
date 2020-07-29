{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.KESKeys
  ( golden_shelleyKESKeys
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyKESKeys :: Property
golden_shelleyKESKeys = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- OP.noteInputFile "test/Test/golden/shelley/keys/kes_keys/verification_key"
  referenceSignKey <- OP.noteInputFile "test/Test/golden/shelley/keys/kes_keys/signing_key"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "kes-verification-key-file"
  signKey <- OP.noteTempFile tempDir "kes-signing-key-file"

  -- Generate payment verification key
  void $ OP.execCardanoCLI
    [ "shelley","node","key-gen-KES"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsKesKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsKesKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  OP.checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
