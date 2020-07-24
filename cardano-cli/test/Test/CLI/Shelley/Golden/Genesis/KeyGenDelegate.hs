{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Genesis.KeyGenDelegate
  ( golden_shelleyGenesisKeyGenDelegate
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenDelegate :: Property
golden_shelleyGenesisKeyGenDelegate = OP.propertyOnce $ do
  OP.workspace "tmp/genesis-key-gen-delegate" $ \tempDir -> do
    verificationKeyFile <- OP.noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- OP.noteTempFile tempDir "key-gen.skey"
    operationalCertificateIssueCounterFile <- OP.noteTempFile tempDir "op-cert.counter"

    void $ OP.execCardanoCLI
        [ "shelley","genesis","key-gen-delegate"
        , "--verification-key-file", verificationKeyFile
        , "--signing-key-file", signingKeyFile
        , "--operational-certificate-issue-counter", operationalCertificateIssueCounterFile
        ]

    OP.assertFileOccurences 1 "GenesisDelegateVerificationKey_ed25519" $ verificationKeyFile
    OP.assertFileOccurences 1 "GenesisDelegateSigningKey_ed25519" $ signingKeyFile
    OP.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ operationalCertificateIssueCounterFile

    OP.assertFileOccurences 1 "Genesis delegate operator key" $ verificationKeyFile
    OP.assertFileOccurences 1 "Genesis delegate operator key" $ signingKeyFile
