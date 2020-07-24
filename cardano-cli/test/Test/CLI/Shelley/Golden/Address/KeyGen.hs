{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP
import qualified System.IO as IO
import qualified Control.Exception as E
import qualified Control.DeepSeq as CSD

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressKeyGen :: Property
golden_shelleyAddressKeyGen = OP.propertyOnce $ OP.workspace "tmp/address-key-gen" $ \tempDir -> do
  addressVKeyFile <- OP.noteTempFile tempDir "address.vkey"
  addressSKeyFile <- OP.noteTempFile tempDir "address.skey"

  void $ OP.execCardanoCLI
    [ "shelley","address","key-gen"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ OP.noteEvalM $ liftIO $ E.evaluate . CSD.force =<< IO.readFile addressVKeyFile
  void $ OP.noteEvalM $ liftIO $ E.evaluate . CSD.force =<< IO.readFile addressSKeyFile

  OP.assertFileOccurences 1 "PaymentVerificationKeyShelley" addressVKeyFile
  OP.assertFileOccurences 1 "PaymentSigningKeyShelley_ed25519" addressSKeyFile
