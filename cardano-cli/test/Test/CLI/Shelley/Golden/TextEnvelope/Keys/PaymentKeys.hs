{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.PaymentKeys
  ( golden_shelleyPaymentKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyPaymentKeys :: Property
golden_shelleyPaymentKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/keys/payment_keys/verification_key"
        referenceSignKey = "test/Test/golden/shelley/keys/payment_keys/signing_key"

    -- Key filepaths
    let verKey = "payment-verification-key-file"
        signKey = "payment-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist createdFiles


    let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat createdFiles signingKeyType referenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
