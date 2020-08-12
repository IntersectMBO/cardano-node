{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Genesis.KeyHash
  ( golden_shelleyGenesisKeyHash
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, (===))
import           Test.OptParse as OP

import qualified System.IO as IO

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyHash :: Property
golden_shelleyGenesisKeyHash = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  referenceVerificationKey <- noteInputFile "test/data/golden/shelley/keys/genesis_keys/verification_key"
  goldenGenesisVerificationKeyHashFile <- noteInputFile "test/data/golden/shelley/keys/genesis_keys/verification_key.key-hash"
  genesisVerificationKeyHashFile <- noteTempFile tempDir "key-hash.hex"

  genesisVerificationKeyHash <- execCardanoCLI
    [ "shelley","genesis","key-hash"
    , "--verification-key-file", referenceVerificationKey
    ]

  liftIO $ IO.writeFile genesisVerificationKeyHashFile genesisVerificationKeyHash

  goldenGenesisVerificationKeyHash <- OP.readFile goldenGenesisVerificationKeyHashFile

  genesisVerificationKeyHash === goldenGenesisVerificationKeyHash
