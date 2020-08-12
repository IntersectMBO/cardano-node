{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenGenesis
  ( golden_shelleyGenesisKeyGenGenesis
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenGenesis :: Property
golden_shelleyGenesisKeyGenGenesis = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"

  void $ execCardanoCLI
    [ "shelley","genesis","key-gen-genesis"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    ]

  assertFileOccurences 1 "GenesisVerificationKey_ed25519" $ verificationKeyFile
  assertFileOccurences 1 "GenesisSigningKey_ed25519" $ signingKeyFile

  assertEndsWithSingleNewline verificationKeyFile
  assertEndsWithSingleNewline signingKeyFile
