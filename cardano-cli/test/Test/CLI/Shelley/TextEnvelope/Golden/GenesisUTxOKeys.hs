{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.GenesisUTxOKeys
  ( golden_shelleyGenesisUTxOKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. We generate a key pair & operational certificate counter file
--   2. We check for the existence of the key pair & counter file
--   3. We check the TextEnvelope serialization format has not changed.
golden_shelleyGenesisUTxOKeys :: Property
golden_shelleyGenesisUTxOKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/genesis_utxo_keys/verification_key"
        rreferenceSignKey = "test/Test/golden/shelley/genesis_utxo_keys/signing_key"

    -- Key filepaths
    let verKey = "genesis-utxo-verification-key-file"
        signKey = "genesis-utxo-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
      "golden_shelleyGenesisUTxOKeys.genesis_utxo_keypair_gen"
        $ evalCardanoCLIParser [ "shelley","genesis","key-gen-utxo"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist createdFiles


    let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisUTxOKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisUTxOKey)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat createdFiles signingKeyType rreferenceSignKey signKey

    liftIO $ fileCleanup createdFiles
    H.success
