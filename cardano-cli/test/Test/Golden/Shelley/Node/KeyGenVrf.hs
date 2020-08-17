{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGenVrf
  ( golden_shelleyNodeKeyGenVrf
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGenVrf :: Property
golden_shelleyNodeKeyGenVrf = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $ execCardanoCLI
    [ "shelley","node","key-gen-VRF"
    , "--verification-key-file", verificationKey
    , "--signing-key-file", signingKey
    ]

  assertFileOccurences 1 "VRF Verification Key" verificationKey
  assertFileOccurences 1 "VRF Signing Key" signingKey

  assertEndsWithSingleNewline verificationKey
  assertEndsWithSingleNewline signingKey
