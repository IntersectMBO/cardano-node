{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.GenesisKeys
  ( golden_shelleyGenesisKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed
golden_shelleyGenesisKeys :: Property
golden_shelleyGenesisKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/keys/genesis_keys/verification_key"
        referenceSignKey = "test/Test/golden/shelley/keys/genesis_keys/signing_key"

    -- Key filepaths
    let verKey = "genesis-verification-key-file"
        signKey = "genesis-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","genesis","key-gen-genesis"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist createdFiles


    let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat createdFiles signingKeyType referenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
