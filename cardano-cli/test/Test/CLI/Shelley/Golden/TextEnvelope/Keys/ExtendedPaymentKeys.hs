{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.ExtendedPaymentKeys
  ( golden_shelleyExtendedPaymentKeys
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyExtendedPaymentKeys :: Property
golden_shelleyExtendedPaymentKeys = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- OP.noteInputFile "test/Test/golden/shelley/keys/extended_payment_keys/verification_key"
  referenceSignKey <- OP.noteInputFile "test/Test/golden/shelley/keys/extended_payment_keys/signing_key"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "extended-payment-verification-key-file"
  signKey <- OP.noteTempFile tempDir "extended-payment-signing-key-file"

  -- Generate payment verification key
  void $ OP.execCardanoCLI
    [ "shelley","address","key-gen"
    , "--extended-key"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentExtendedKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentExtendedKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  OP.checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
