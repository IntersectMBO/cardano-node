{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.StakeAddress.KeyGen
  ( golden_shelleyStakeAddressKeyGen
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressKeyGen :: Property
golden_shelleyStakeAddressKeyGen = OP.propertyOnce $ do
  OP.workspace "tmp/stake-address-key-gen" $ \tempDir -> do
    verificationKeyFile <- OP.noteTempFile tempDir "kes.vkey"
    signingKeyFile <- OP.noteTempFile tempDir "kes.skey"

    void $ OP.execCardanoCLI
        [ "shelley","stake-address","key-gen"
        , "--verification-key-file", verificationKeyFile
        , "--signing-key-file", signingKeyFile
        ]

    OP.assertFileOccurences 1 "StakeVerificationKeyShelley_ed25519" verificationKeyFile
    OP.assertFileOccurences 1 "StakeSigningKeyShelley_ed25519" signingKeyFile
