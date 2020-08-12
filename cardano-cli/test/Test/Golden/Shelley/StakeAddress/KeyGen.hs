{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.KeyGen
  ( golden_shelleyStakeAddressKeyGen
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressKeyGen :: Property
golden_shelleyStakeAddressKeyGen = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "kes.vkey"
  signingKeyFile <- noteTempFile tempDir "kes.skey"

  void $ execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    ]

  assertFileOccurences 1 "StakeVerificationKeyShelley_ed25519" verificationKeyFile
  assertFileOccurences 1 "StakeSigningKeyShelley_ed25519" signingKeyFile

  assertEndsWithSingleNewline verificationKeyFile
  assertEndsWithSingleNewline signingKeyFile
