{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.StakeKeys
  ( golden_shelleyStakeKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. We generate a key pair
--   2. We check for the existence of the key pair
--   3. We check the TextEnvelope serialization format has not changed.
golden_shelleyStakeKeys :: Property
golden_shelleyStakeKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/stake_keys/verification_key"
        rreferenceSignKey = "test/Test/golden/shelley/stake_keys/signing_key"

    -- Key filepaths
    let verKey = "stake-verification-key-file"
        signKey = "stake-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
      "golden_shelleyStakeKeys.stake_keypair_gen"
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
    checkTextEnvelopeFormat createdFiles signingKeyType rreferenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
