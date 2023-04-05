{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Node.KeyGen
  ( golden_shelleyNodeKeyGen
  , golden_shelleyNodeKeyGen_bech32
  , golden_shelleyNodeKeyGen_te
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGen :: Property
golden_shelleyNodeKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "node","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  H.assertFileOccurences 1 "StakePoolVerificationKey_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "StakePoolSigningKey_ed25519" signingKeyFile
  H.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" opCertCounterFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
  H.assertEndsWithSingleNewline opCertCounterFile

golden_shelleyNodeKeyGen_te :: Property
golden_shelleyNodeKeyGen_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "node","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  H.assertFileOccurences 1 "StakePoolVerificationKey_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "StakePoolSigningKey_ed25519" signingKeyFile
  H.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" opCertCounterFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
  H.assertEndsWithSingleNewline opCertCounterFile

golden_shelleyNodeKeyGen_bech32 :: Property
golden_shelleyNodeKeyGen_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"
  opCertCounterFile <- noteTempFile tempDir "op-cert.counter"

  void $ execCardanoCLI
    [ "node","key-gen"
    , "--key-output-format", "bech32"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    , "--operational-certificate-issue-counter", opCertCounterFile
    ]

  H.assertFileOccurences 1 "pool_vk" verificationKeyFile
  H.assertFileOccurences 1 "pool_sk" signingKeyFile
  H.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" opCertCounterFile

  H.assertEndsWithSingleNewline opCertCounterFile
