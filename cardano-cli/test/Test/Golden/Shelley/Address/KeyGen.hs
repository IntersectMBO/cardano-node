{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  , golden_shelleyAddressExtendedKeyGen
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
