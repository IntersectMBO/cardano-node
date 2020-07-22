{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Genesis.KeyHash
  ( golden_shelleyGenesisKeyHash
  ) where

import Cardano.Prelude hiding (to)

import Hedgehog (Property, (===))

import qualified System.IO as IO
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyHash :: Property
golden_shelleyGenesisKeyHash = OP.propertyOnce $ do
  OP.workspace "tmp/genesis-key-hash" $ \tempDir -> do
    let referenceVerificationKey = "test/Test/golden/shelley/keys/genesis_keys/verification_key"
        genesisVerificationKeyHashFile = tempDir <> "/key-hash.hex"
        goldenGenesisVerificationKeyHashFile = "test/Test/golden/shelley/keys/genesis_keys/verification_key.key-hash"

    genesisVerificationKeyHash <-liftIO $ OP.execCardanoCLI
        [ "shelley","genesis","key-hash"
        , "--verification-key-file", referenceVerificationKey
        ]

    liftIO $ IO.writeFile genesisVerificationKeyHashFile genesisVerificationKeyHash

    goldenGenesisVerificationKeyHash <- OP.noteEvalM . liftIO $ IO.readFile goldenGenesisVerificationKeyHashFile

    genesisVerificationKeyHash === goldenGenesisVerificationKeyHash
