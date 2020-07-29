{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressKeyGen :: Property
golden_shelleyAddressKeyGen = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- OP.noteTempFile tempDir "address.vkey"
  addressSKeyFile <- OP.noteTempFile tempDir "address.skey"

  void $ OP.execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ OP.readFile addressVKeyFile
  void $ OP.readFile addressSKeyFile

  OP.assertFileOccurences 1 "PaymentVerificationKeyShelley" addressVKeyFile
  OP.assertFileOccurences 1 "PaymentSigningKeyShelley_ed25519" addressSKeyFile

  OP.assertEndsWithSingleNewline addressVKeyFile
  OP.assertEndsWithSingleNewline addressSKeyFile
