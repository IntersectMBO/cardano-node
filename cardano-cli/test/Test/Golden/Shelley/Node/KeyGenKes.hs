{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenKes
  ( golden_shelleyNodeKeyGenKes
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGenKes :: Property
golden_shelleyNodeKeyGenKes = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $ execCardanoCLI
    [ "shelley","node","key-gen-KES"
    , "--verification-key-file", verificationKey
    , "--signing-key-file", signingKey
    ]

  assertFileOccurences 1 "KesVerificationKey_ed25519_kes_2^6" verificationKey
  assertFileOccurences 1 "KesSigningKey_ed25519_kes_2^6" signingKey

  assertEndsWithSingleNewline verificationKey
  assertEndsWithSingleNewline signingKey
