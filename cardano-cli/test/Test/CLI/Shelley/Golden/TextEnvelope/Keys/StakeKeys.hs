{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.StakeKeys
  ( golden_shelleyStakeKeys
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakeKeys :: Property
golden_shelleyStakeKeys = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/verification_key"
  referenceSignKey <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/signing_key"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "stake-verification-key-file"
  signKey <- OP.noteTempFile tempDir "stake-signing-key-file"

  -- Generate stake key pair
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsStakeKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsStakeKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  OP.checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
