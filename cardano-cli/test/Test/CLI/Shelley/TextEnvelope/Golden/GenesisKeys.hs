{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.GenesisKeys
  ( golden_shelleyGenesisKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. We generate a key pair
--   2. We check for the existence of the key pair
--   3. We check the TextEnvelope serialization format has not changed.
golden_shelleyGenesisKeys :: Property
golden_shelleyGenesisKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/genesis_keys/verification_key"
        rreferenceSignKey = "test/Test/golden/shelley/genesis_keys/signing_key"

    -- Key filepaths
    let verKey = "genesis-verification-key-file"
        signKey = "genesis-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
      "golden_shelleyGenesisKeys.genesis_keypair_gen"
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
    checkTextEnvelopeFormat createdFiles signingKeyType rreferenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
