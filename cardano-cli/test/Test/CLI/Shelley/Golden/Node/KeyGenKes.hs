{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Node.KeyGenKes
  ( golden_shelleyNodeKeyGenKes
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyNodeKeyGenKes :: Property
golden_shelleyNodeKeyGenKes = OP.propertyOnce $ do
  OP.workspace "tmp/node-key-gen-kes" $ \tempDir -> do
    verificationKey <- OP.noteTempFile tempDir "kes.vkey"
    signingKey <- OP.noteTempFile tempDir "kes.skey"

    void $ OP.execCardanoCLI
        [ "shelley","node","key-gen-KES"
        , "--verification-key-file", verificationKey
        , "--signing-key-file", signingKey
        ]

    OP.assertFileOccurences 1 "KesVerificationKey_ed25519_kes_2^6" verificationKey
    OP.assertFileOccurences 1 "KesSigningKey_ed25519_kes_2^6" signingKey
