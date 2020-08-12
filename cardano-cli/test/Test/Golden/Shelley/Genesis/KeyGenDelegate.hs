{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyGenDelegate
  ( golden_shelleyGenesisKeyGenDelegate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenDelegate :: Property
golden_shelleyGenesisKeyGenDelegate = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  operationalCertificateIssueCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "shelley","genesis","key-gen-delegate"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", operationalCertificateIssueCounterFile
    ]

  assertFileOccurences 1 "GenesisDelegateVerificationKey_ed25519" $ verificationKeyFile
  assertFileOccurences 1 "GenesisDelegateSigningKey_ed25519" $ signingKeyFile
  assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ operationalCertificateIssueCounterFile

  assertFileOccurences 1 "Genesis delegate operator key" $ verificationKeyFile
  assertFileOccurences 1 "Genesis delegate operator key" $ signingKeyFile

  assertEndsWithSingleNewline verificationKeyFile
  assertEndsWithSingleNewline signingKeyFile
  assertEndsWithSingleNewline operationalCertificateIssueCounterFile
