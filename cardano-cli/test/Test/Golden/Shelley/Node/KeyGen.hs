{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGen
  ( golden_shelleyNodeKeyGen
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGen :: Property
golden_shelleyNodeKeyGen = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "shelley","node","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  assertFileOccurences 1 "StakePoolVerificationKey_ed25519" $ verificationKeyFile
  assertFileOccurences 1 "StakePoolSigningKey_ed25519" $ signingKeyFile
  assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ opCertCounterFile

  assertEndsWithSingleNewline verificationKeyFile
  assertEndsWithSingleNewline signingKeyFile
  assertEndsWithSingleNewline opCertCounterFile
