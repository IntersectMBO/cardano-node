{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.StakeKeys
  ( golden_shelleyStakeKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakeKeys :: Property
golden_shelleyStakeKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/keys/stake_keys/verification_key"
        referenceSignKey = "test/Test/golden/shelley/keys/stake_keys/signing_key"

    -- Key filepaths
    let verKey = "stake-verification-key-file"
        signKey = "stake-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate stake key pair
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","key-gen"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist createdFiles


    let signingKeyType = textEnvelopeType (AsSigningKey AsStakeKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsStakeKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat createdFiles signingKeyType referenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
