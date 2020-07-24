{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Node.KeyGen
  ( golden_shelleyNodeKeyGen
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGen :: Property
golden_shelleyNodeKeyGen = OP.propertyOnce $ do
  OP.workspace "tmp/node-key-gen" $ \tempDir -> do
    verificationKeyFile <- OP.noteTempFile tempDir "key-gen.vkey"
    signingKeyFile <- OP.noteTempFile tempDir "key-gen.skey"
    opCertCounterFile <- OP.noteTempFile tempDir "op-cert.counter"

    void $ OP.execCardanoCLI
        [ "shelley","node","key-gen"
        , "--verification-key-file", verificationKeyFile
        , "--signing-key-file", signingKeyFile
        , "--operational-certificate-issue-counter", opCertCounterFile
        ]

    OP.assertFileOccurences 1 "StakePoolVerificationKey_ed25519" $ verificationKeyFile
    OP.assertFileOccurences 1 "StakePoolSigningKey_ed25519" $ signingKeyFile
    OP.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ opCertCounterFile
