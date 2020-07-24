{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Node.KeyGenVrf
  ( golden_shelleyNodeKeyGenVrf
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGenVrf :: Property
golden_shelleyNodeKeyGenVrf = OP.propertyOnce $ do
  OP.workspace "tmp/node-key-gen-vrf" $ \tempDir -> do
    verificationKey <- OP.noteTempFile tempDir "kes.vkey"
    signingKey <- OP.noteTempFile tempDir "kes.skey"

    void $ OP.execCardanoCLI
        [ "shelley","node","key-gen-VRF"
        , "--verification-key-file", verificationKey
        , "--signing-key-file", signingKey
        ]

    OP.assertFileOccurences 1 "VRF Verification Key" verificationKey
    OP.assertFileOccurences 1 "VRF Signing Key" signingKey
