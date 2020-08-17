{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressKeyGen :: Property
golden_shelleyAddressKeyGen = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ OP.readFile addressVKeyFile
  void $ OP.readFile addressSKeyFile

  assertFileOccurences 1 "PaymentVerificationKeyShelley" addressVKeyFile
  assertFileOccurences 1 "PaymentSigningKeyShelley_ed25519" addressSKeyFile

  assertEndsWithSingleNewline addressVKeyFile
  assertEndsWithSingleNewline addressSKeyFile
