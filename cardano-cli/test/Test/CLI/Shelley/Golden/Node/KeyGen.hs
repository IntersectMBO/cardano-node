{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Node.KeyGen
  ( golden_shelleyNodeKeyGen
  ) where

import Cardano.Prelude hiding (to)

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGen :: Property
golden_shelleyNodeKeyGen = OP.propertyOnce $ do
  OP.workspace "tmp/node-key-gen" $ \tempDir -> do
    let verificationKeyFile = tempDir <> "/key-gen.vkey"
        signingKeyFile = tempDir <> "/key-gen.skey"
        opCertCounterFile = tempDir <> "/op-cert.counter"

    void . liftIO $ OP.execCardanoCLI
        [ "shelley","node","key-gen"
        , "--verification-key-file", verificationKeyFile
        , "--signing-key-file", signingKeyFile
        , "--operational-certificate-issue-counter", opCertCounterFile
        ]

    OP.assertFileOccurences 1 "StakePoolVerificationKey_ed25519" $ verificationKeyFile
    OP.assertFileOccurences 1 "StakePoolSigningKey_ed25519" $ signingKeyFile
    OP.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ opCertCounterFile
