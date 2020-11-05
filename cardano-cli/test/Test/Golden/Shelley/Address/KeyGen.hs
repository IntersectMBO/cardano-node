{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  , golden_shelleyAddressKeyGen_bech32
  , golden_shelleyAddressKeyGen_te
  , golden_shelleyAddressExtendedKeyGen
  , golden_shelleyAddressExtendedKeyGen_bech32
  , golden_shelleyAddressExtendedKeyGen_te
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressKeyGen :: Property
golden_shelleyAddressKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "address","key-gen"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentVerificationKeyShelley_ed25519" addressVKeyFile
  H.assertFileOccurences 1 "PaymentSigningKeyShelley_ed25519" addressSKeyFile

golden_shelleyAddressKeyGen_te :: Property
golden_shelleyAddressKeyGen_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "address","key-gen"
    , "--key-output-format", "text-envelope"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentVerificationKeyShelley_ed25519" addressVKeyFile
  H.assertFileOccurences 1 "PaymentSigningKeyShelley_ed25519" addressSKeyFile

golden_shelleyAddressKeyGen_bech32 :: Property
golden_shelleyAddressKeyGen_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "address","key-gen"
    , "--key-output-format", "bech32"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "addr_vk" addressVKeyFile
  H.assertFileOccurences 1 "addr_sk" addressSKeyFile

golden_shelleyAddressExtendedKeyGen :: Property
golden_shelleyAddressExtendedKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "shelley","address","key-gen"
    , "--extended-key"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentExtendedVerificationKeyShelley_ed25519_bip32" addressVKeyFile
  H.assertFileOccurences 1 "PaymentExtendedSigningKeyShelley_ed25519_bip32" addressSKeyFile

golden_shelleyAddressExtendedKeyGen_te :: Property
golden_shelleyAddressExtendedKeyGen_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "shelley","address","key-gen"
    , "--key-output-format", "text-envelope"
    , "--extended-key"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentExtendedVerificationKeyShelley_ed25519_bip32" addressVKeyFile
  H.assertFileOccurences 1 "PaymentExtendedSigningKeyShelley_ed25519_bip32" addressSKeyFile

golden_shelleyAddressExtendedKeyGen_bech32 :: Property
golden_shelleyAddressExtendedKeyGen_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execCardanoCLI
    [ "shelley","address","key-gen"
    , "--key-output-format", "bech32"
    , "--extended-key"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "addr_xvk" addressVKeyFile
  H.assertFileOccurences 1 "addr_xsk" addressSKeyFile
