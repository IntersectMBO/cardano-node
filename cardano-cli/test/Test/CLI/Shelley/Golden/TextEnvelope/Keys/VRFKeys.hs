{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.VRFKeys
  ( golden_shelleyVRFKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyVRFKeys :: Property
golden_shelleyVRFKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/keys/vrf_keys/verification_key"
        referenceSignKey = "test/Test/golden/shelley/keys/vrf_keys/signing_key"

    -- Key filepaths
    let verKey = "vrf-verification-key-file"
        signKey = "vrf-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate vrf verification key
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","node","key-gen-VRF"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist createdFiles

    let signingKeyType = textEnvelopeType (AsSigningKey AsVrfKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsVrfKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat createdFiles signingKeyType referenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
