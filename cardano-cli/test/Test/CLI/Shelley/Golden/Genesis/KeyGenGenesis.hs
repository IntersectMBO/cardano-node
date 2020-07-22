{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Genesis.KeyGenGenesis
  ( golden_shelleyGenesisKeyGenGenesis
  ) where

import Cardano.Prelude hiding (to)

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyGenGenesis :: Property
golden_shelleyGenesisKeyGenGenesis = OP.propertyOnce $ do
  OP.workspace "tmp/genesis-key-gen-genesis" $ \tempDir -> do
    let verificationKeyFile = tempDir <> "/key-gen.vkey"
        signingKeyFile = tempDir <> "/key-gen.skey"

    void . liftIO $ OP.execCardanoCLI
        [ "shelley","genesis","key-gen-genesis"
        , "--verification-key-file", verificationKeyFile
        , "--signing-key-file", signingKeyFile
        ]

    OP.assertFileOccurences 1 "GenesisVerificationKey_ed25519" $ verificationKeyFile
    OP.assertFileOccurences 1 "GenesisSigningKey_ed25519" $ signingKeyFile
