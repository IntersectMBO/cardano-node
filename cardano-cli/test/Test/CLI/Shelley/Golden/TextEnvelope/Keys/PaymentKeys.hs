{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.PaymentKeys
  ( golden_shelleyPaymentKeys
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyPaymentKeys :: Property
golden_shelleyPaymentKeys = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- OP.noteInputFile "test/Test/golden/shelley/keys/payment_keys/verification_key"
  referenceSignKey <- OP.noteInputFile "test/Test/golden/shelley/keys/payment_keys/signing_key"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "payment-verification-key-file"
  signKey <- OP.noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $ OP.execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  OP.checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
