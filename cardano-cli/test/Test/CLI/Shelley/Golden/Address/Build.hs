{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Address.Build
  ( golden_shelleyAddressBuild
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Control.DeepSeq as CSD
import qualified Control.Exception as E
import qualified System.IO as IO
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressBuild :: Property
golden_shelleyAddressBuild = OP.propertyOnce $ OP.workspace "tmp/address-build" $ \tempDir -> do
  addressVKeyFile <- OP.noteInputFile "test/Test/golden/shelley/keys/payment_keys/verification_key"
  addressSKeyFile <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/verification_key"
  goldenStakingAddressHexFile <- OP.noteInputFile "test/Test/golden/shelley/addresses/staking-address.hex"
  goldenEnterpriseAddressHexFile <- OP.noteInputFile "test/Test/golden/shelley/addresses/enterprise-address.hex"
  stakingAddressHexFile <- OP.noteTempFile tempDir "staking-address.hex"
  enterpriseAddressHexFile <- OP.noteTempFile tempDir "enterprise-address.hex"

  void $ OP.noteEvalM $ liftIO $ E.evaluate . CSD.force =<< IO.readFile addressVKeyFile

  stakingAddressText <- OP.noteEvalM $ OP.execCardanoCLI
    [ "shelley","address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenStakingAddressHex <- OP.noteEvalM . liftIO $ IO.readFile goldenStakingAddressHexFile
  
  liftIO $ IO.writeFile stakingAddressHexFile stakingAddressText

  OP.equivalence [] stakingAddressText goldenStakingAddressHex

  void $ OP.noteEvalM $ liftIO $ E.evaluate . CSD.force =<< IO.readFile addressSKeyFile

  enterpriseAddressText <- OP.noteEvalM $ OP.execCardanoCLI
    [ "shelley","address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenEnterpriseAddressHex <- OP.noteEvalM . liftIO $ IO.readFile goldenEnterpriseAddressHexFile

  liftIO $ IO.writeFile enterpriseAddressHexFile enterpriseAddressText

  OP.equivalence [] enterpriseAddressText goldenEnterpriseAddressHex
