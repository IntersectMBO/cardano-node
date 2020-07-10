{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.ExtendedPaymentKeys
  ( golden_shelleyExtendedPaymentKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyExtendedPaymentKeys :: Property
golden_shelleyExtendedPaymentKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/keys/extended_payment_keys/verification_key"
        referenceSignKey = "test/Test/golden/shelley/keys/extended_payment_keys/signing_key"

    -- Key filepaths
    let verKey = "extended-payment-verification-key-file"
        signKey = "extended-payment-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--extended-key"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist createdFiles


    let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentExtendedKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentExtendedKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat createdFiles signingKeyType referenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
